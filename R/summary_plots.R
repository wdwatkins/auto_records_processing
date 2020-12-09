n_diffs_summary <- function(summary_df) {
  df_unnest <- all_summ %>% unnest_wider(col = summary_object)
}

dygraph_edited_raw <- function(outfile, joined_data_file, summary_file, 
                               site_no, parameter) {
  joined_data <- readRDS(joined_data_file)
  dygraph_cols <- joined_data %>% 
    select(datetime_parsed, aq_raw_value, value_adaps_edit) %>% 
    mutate(aq_raw_value = aq_raw_value[[1]])
  dygraph_to_plot <- xts(dygraph_cols[,-1], 
                         order.by = dygraph_cols$datetime_parsed)
  summary_ob <- readRDS(summary_file)
  site_name <- readNWISsite(site_no) %>% pull(station_nm)
  plot_title <- sprintf("%s %s\n%s", site_name, site_no, parameter)
  dy_obj <- dygraph(dygraph_to_plot, xlab = "Date", ylab = parameter,
                    main = plot_title)
  outfile_absolute <- file.path(getwd(), outfile) #due to bug in saveWidget
  saveWidget(widget = dy_obj, file = outfile_absolute)
}

dygraph_model_output <- function(outfile, split_data_file, model_output_file, model_eval_df) {
  #symbols for true/false negatives and positives
  #print summary result in title?
  #line to split train/test? or different color?
  file_name_elements <- model_output_file %>% basename() %>% 
    str_split(pattern = '_', simplify = TRUE)
  model_output <- readRDS(model_output_file)
  split_data <- readRDS(split_data_file)
  
  #row bind test/train model output and inputs after appending test/train col
  #generate yes/no based on cutoff value from pred
  train_full_in_out <- split_data$train_df %>% 
    rename(remove_point_bin = remove_point) %>% 
    bind_cols(model_output$train_predictions, split_data$train_dates) %>% 
    mutate(set = 'train')
  test_full_in_out <- split_data$test_df %>% 
    rename(remove_point_bin = remove_point) %>% 
    bind_cols(model_output$test_predictions, split_data$test_dates) %>% 
    mutate(set = 'test')
  full_timeseries_in_out <- bind_rows(train_full_in_out, test_full_in_out) %>% 
    mutate(model_remove_point = .pred_yes > model_eval_df$train_.threshold,
           remove_point_bin = as.logical(remove_point_bin),
           point_type = case_when(model_remove_point & remove_point_bin ~ 'true positive',
                                  !model_remove_point & !remove_point_bin ~ 'true negative',
                                  model_remove_point & !remove_point_bin ~ 'false positive',
                                  !model_remove_point & remove_point_bin ~ 'false negative'),
           pivot_col = TRUE) %>% 
    pivot_wider(id_cols = -matches('point_type|pivot_col'), names_from = point_type, values_from = pivot_col) %>% 
    mutate(aq_raw_pivot = aq_raw_value) %>% 
    pivot_wider(id_cols = -matches('pivot|set'), names_from = set, values_from = aq_raw_pivot,
                names_prefix = 'raw_value_')
  assert_that(length(unique(full_timeseries_in_out$datetime_parsed)) == nrow(full_timeseries_in_out))
  #make plot, decide on visuals
  site_name <- readNWISsite(model_eval_df$site) %>% pull(station_nm)
  plot_title <- sprintf("%s %s\n%s", site_name, model_eval_df$site, model_eval_df$param_predicted)
  dygraph_cols <- full_timeseries_in_out %>% 
    select(datetime_parsed, aq_raw_value, contains('raw_value_'), contains('false'), `true positive`) %>% 
    mutate(across(matches('negative|positive'), .fns = ~if_else(condition = .x, true = aq_raw_value, false = NA_real_))) %>% 
    select(-aq_raw_value)
  dygraph_to_plot <- xts(dygraph_cols[,-1], 
                         order.by = dygraph_cols$datetime_parsed)
  browser()
  dy_obj <- dygraph(dygraph_to_plot, xlab = "Date", ylab = model_eval_df$param_predicted,
                    main = plot_title) %>%  
    dySeries(name = 'raw_value_test', color = 'forestgreen') %>% 
    dySeries(name = 'false positive', color = 'red', pointShape = 'dot', drawPoints = TRUE, pointSize = 1, strokeWidth = 0) %>%
    #dyOptions(pointShape = 'ex') %>%
    dySeries(name = 'true positive', color = 'blue', drawPoints = TRUE, pointShape = 'ex', pointSize = 3, strokeWidth = 0) %>% 
    dySeries(name = 'false negative', color = 'red', drawPoints = TRUE, pointShape = 'ex', pointSize = 3, strokeWidth = 0) %>%
    #dyGroup(c('false negative', 'true positive'), pointShape = c('ex', 'ex')) %>% 
    dyUnzoom() %>% dyRangeSelector()
  outfile_absolute <- file.path(getwd(), outfile) #due to bug in saveWidget
  saveWidget(widget = dy_obj, file = outfile_absolute)
}