create_train_test <- function(outfile, joined_aq_adaps_file, split_method, 
                              lower_bound_include, upper_bound_include) {
  joined_data <- readRDS(joined_aq_adaps_file)
  joined_cleaned <- select(joined_data, -matches('corrected|adaps'), 
                           value_adaps_edit, -datetime, -datetime_tz, -timestamp) %>% 
    mutate(na_edit = is.na(value_adaps_edit) & !is.na(aq_raw_value),
           not_equal = value_adaps_edit != aq_raw_value) %>% 
    filter(datetime_parsed == round_date(datetime_parsed, '15mins'),
           !is.na(aq_raw_value),
           aq_raw_value <= upper_bound_include,
           aq_raw_value >= lower_bound_include,
           value_adaps_edit == aq_raw_value | is.na(value_adaps_edit)) %>% 
    mutate(across(where(is.data.frame), ~.x[[1]]),
           remove_point = as.numeric(na_edit),
           doy = yday(datetime_parsed)) %>% 
    select(-not_equal, -value_adaps_edit, -na_edit) %>% 
    drop_na()
  if(split_method == "test_future") {
    cleaned_rows <- nrow(joined_cleaned)
    positives_df <- joined_cleaned %>% 
      mutate(index = row_number()) %>% 
      filter(remove_point == 1)
    #60/40 split based on positives or 50/50 whichever is higher for total training rows
    split_point_positive_index <- round(.6*nrow(positives_df))
    split_point_full_index <- positives_df$index[split_point_positive_index]
    split_point_max <- max(c(split_point_full_index, round(.5*cleaned_rows)))
    train <- slice(joined_cleaned, 1:split_point_max) 
    test <- slice(joined_cleaned, split_point_max+1:cleaned_rows)
  } else {
    stop('split method not implemented')
  }
  split_data <- list(train_df = train %>% select(-datetime_parsed),
                     test_df = test %>% select(-datetime_parsed),
                     train_dates = train %>% select(datetime_parsed),
                     test_dates = test %>% select(datetime_parsed),
                     train_fraction_positive = sum(train$remove_point)/nrow(train),
                     test_fraction_positive = sum(test$remove_point)/nrow(train))
  message('train fraction: ', split_data$train_fraction_positive)
  message('test fraction: ', split_data$test_fraction_positive)
  saveRDS(split_data, file = outfile)
}

model_and_predict_log_reg <- function(outfile, split_data_file) {
  split_data <- readRDS(split_data_file)
  
  recipe_simple <- function(dataset) {
    recipe(remove_point ~ ., data = dataset) %>% 
      step_bin2factor(remove_point) %>% 
      prep(data = dataset)
  }
  train_df <- split_data$train_df 
  test_df <- split_data$test_df
  
  #TODO: filter all cols if they exist; do models converge then?
  #train_df_filt <- train_df %>% filter(`specific cond at 25c_aq_raw_value` > -0.1)
  non_negative_params <- c('gage height_aq_raw_value', 'specific cond at 25c_aq_raw_value',
                           'dissolved oxygen_aq_raw_value', 'ph_aq_raw_value', 'turbidity, fnu_aq_raw_value')
  train_filtered <- train_df %>% filter(across(any_of(non_negative_params), ~.x > 0),
                                        across(any_of('ph_aq_raw_value'), ~.x < 14),
                                        across(any_of('dissolved oxygen_aq_raw_value'), ~.x < 50))
  recipe_prepped <- recipe_simple(dataset = train_df)
  train_baked <- bake(recipe_prepped, new_data = train_df)
  train_filtered_baked <- bake(recipe_prepped, new_data = train_filtered)
  test_baked  <- bake(recipe_prepped, new_data = test_df)
  logistic_glm <- logistic_reg(mode = 'classification') %>% 
    set_engine('glm') %>% 
    fit(remove_point ~ ., data = train_filtered_baked)
  predictions_glm <- logistic_glm %>% 
    predict(new_data = test_baked, type = 'prob') %>% 
    bind_cols(test_baked %>% select(remove_point))
  predictions_train_glm <- logistic_glm %>% 
    predict(new_data = train_baked, type = 'prob') %>% 
    bind_cols(train_baked %>% select(remove_point))
  model_outputs <- list(model = logistic_glm,
                        test_predictions = predictions_glm,
                        train_predictions = predictions_train_glm)
  saveRDS(model_outputs, file = outfile)
}
confusion_tibble <- function(pred_df){
  pred_df %>% 
    conf_mat(remove_point, .pred_class) %>% 
    pluck(1) %>% 
    as_tibble()
}

model_results_metric_df <- function(results_file) {
  
  model_results <- readRDS(results_file)
  file_split <- str_split(basename(results_file), pattern = '_', 
                          simplify = TRUE)
  results_summary <- tibble(
    site_no = file_split[1],
    parameter = file_split[2],
    train_conf_mat = list(model_results$train_predictions %>% confusion_tibble()),
    test_conf_mat = list(model_results$test_predictions %>% confusion_tibble()),
    train_precision = precision(model_results$train_predictions, 
                                remove_point, .pred_class) %>%
      pull(.estimate),
    train_recall = recall(model_results$train_predictions, remove_point, .pred_class) %>%
      pull(.estimate),
    test_precision = precision(model_results$test_predictions, 
                               remove_point, .pred_class) %>%
      pull(.estimate),
    test_recall = recall(model_results$test_predictions, remove_point, .pred_class) %>%
      pull(.estimate),
    train_f1 = f_meas(model_results$train_predictions, truth = remove_point, 
                      estimate = .pred_class)  %>% 
      pull(.estimate),
    test_f1 = f_meas(model_results$test_predictions, truth = remove_point, 
                     estimate = .pred_class)  %>% 
      pull(.estimate),
    n_train = nrow(model_results$train_predictions),
    n_test = nrow(model_results$test_predictions),
    model_coefs = list(model_results$model$fit$coefficients)
  )
  return(results_summary)
}

generate_combine_model_metrics <- function(...) {
  dots <- list(...)
  model_eval_dots <- dots[purrr::map_lgl(dots, is_tibble)]
  metrics_df <- do.call(bind_rows, model_eval_dots)
  return(metrics_df)
}

log_reg_roc_curve <- function(outfile, model_output_file) {
  #generate curve with yardstick
  #combine into one image with prec-recall curve?
  file_name_elements <- outfile %>% basename() %>% 
    str_split(pattern = '_', simplify = TRUE)
  plot_title <- paste('Site', file_name_elements[1], 'predicting', file_name_elements[2])  
  
  model_output <- readRDS(model_output_file)
  train_summary <- model_output$train_predictions %>% 
    group_by(remove_point) %>% summarize(n=n(), .groups = 'drop')
  test_summary <- model_output$test_predictions %>% 
    group_by(remove_point) %>% summarize(n=n(), .groups = 'drop')
  if('yes' %in% train_summary$remove_point) {
    train_roc_plot <- roc_curve(model_output$train_predictions, remove_point, .pred_yes) %>%   
      ggplot(aes(x = 1 - specificity, y = sensitivity)) +
      geom_path() +
      geom_abline(lty = 3) +
      coord_equal() +
      theme_bw() +
      ggtitle('Training data ROC curve')
    train_pr_plot <- pr_curve(model_output$train_predictions, remove_point, .pred_yes) %>%   
      ggplot(aes(x = recall, y = precision)) +
      geom_path() +
      geom_abline(lty = 3) +
      coord_equal() +
      theme_bw() +
      ggtitle('Training data precision-recall curve')
  } else {
    train_pr_plot <- train_roc_plot <- ggplot() + ggtitle('No positive values')
  }
  if('yes' %in% test_summary$remove_point) {
    test_roc_plot <- roc_curve(model_output$test_predictions, remove_point, .pred_yes) %>%   
      ggplot(aes(x = 1 - specificity, y = sensitivity)) +
      geom_path() +
      geom_abline(lty = 3) +
      coord_equal() +
      theme_bw() +
      ggtitle('Test data ROC curve')
    
    test_pr_plot <- pr_curve(model_output$test_predictions, remove_point, .pred_yes) %>%   
      ggplot(aes(x = recall, y = precision)) +
      geom_path() +
      geom_abline(lty = 3) +
      coord_equal() +
      theme_bw() +
      ggtitle('Test data precision-recall curve')
  } else {
    test_pr_plot <- test_roc_plot <- ggplot() + ggtitle('No positive values')
  }
  plot_grid <- grid.arrange(train_roc_plot, test_roc_plot, train_pr_plot, 
                            test_pr_plot, nrow = 2,
                            top = plot_title)
  ggsave(filename = outfile, plot = plot_grid)
}

#use F1 score to choose
#choose_threshold_


eval_model <- function(model_output_file) {
  file_name_elements <- model_output_file %>% basename() %>% 
    str_split(pattern = '_', simplify = TRUE)
  plot_title <- paste('Site', file_name_elements[1], 'predicting', file_name_elements[2])  
  model_output <- readRDS(model_output_file)
  #choose threshold via f1 on training
  f1_df <- pr_curve(model_output$train_predictions, remove_point, .pred_yes) %>%
    filter(.threshold <= .9999999 & .threshold > 1e-10) %>% 
    mutate(f1 = 2 * (precision*recall) / (precision + recall))
  #browser()
  #stopifnot(nrow(f1_df) > 0)
  max_f1 <- max(f1_df$f1, na.rm = TRUE)
  train_results <- filter(f1_df, f1 == max_f1) %>% 
    mutate(n_train = nrow(model_output$train_predictions),
           n_positives_train = sum(model_output$train_predictions$remove_point == 'yes')) %>% 
    rename_with(.fn = ~paste0('train_', .))
  train_threshold <- ifelse(length(train_results$train_.threshold) > 0,
                            yes = train_results$train_.threshold,
                            no = NA)
  test_vals <- model_output$test_predictions %>% 
    mutate(estimate = if_else(.pred_yes > train_threshold,
                              true = factor('yes', levels = c('yes', 'no')),
                              false = factor('no', levels = c('yes', 'no'))))
  test_results <-  tibble(
    test_precision = precision(test_vals, 
                               truth = remove_point, 
                               estimate = estimate) %>%
      pull(.estimate),
    test_recall = recall(test_vals, 
                         truth = remove_point, 
                         estimate = estimate) %>%
      pull(.estimate),
    test_f1 = f_meas(test_vals, 
                     truth = remove_point, 
                     estimate = estimate) %>%
      pull(.estimate),
    n_test = nrow(model_output$test_predictions),
    n_positives_test = sum(model_output$test_predictions$remove_point == 'yes'))
  all_results <- bind_cols(train_results, test_results) %>% 
    mutate(site = file_name_elements[1],
           param_predicted = file_name_elements[2],
           cutoff_value = train_threshold)
  return(all_results)
}
