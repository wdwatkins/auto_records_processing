download_data_bundles <- function(sites_params, ...) {
  #unique sites
  task_names <- unique(sites_params$site_no)
  
  download_step <- create_task_step(
    step_name = "download_bundles",
    target_name = function(task_name, ...) {
      sprintf("1_fetch/zip/bundle_%s.zip", task_name)
    },
    command = function(task_name, ...) {
      psprintf("get_bundle_trycatch(outfile = target_name, site = I('%s'))" =  task_name)
    }
  )
  
  task_plan <- create_task_plan(
    task_names = task_names,
    task_steps = list(download_step),
    add_complete = FALSE
  )
  create_task_makefile(
    task_plan = task_plan,
    makefile = "1_bundles_download.yml",
    sources = c(...),
    packages = "dataRetrieval",
    finalize_funs = 'get_bundle_inventory',
    final_targets = 'bundle_inventory',
    as_promises = TRUE
  )
  
  #build
  loop_tasks(task_plan = task_plan, task_makefile = '1_bundles_download.yml')
  bundle_inventory <- remake::fetch('bundle_inventory_promise', remake_file = '1_bundles_download.yml')
  return(bundle_inventory)
}

get_bundle_inventory <- function(...) {
  dots <- list(...)
  zip_dots <- dots[purrr::map_lgl(dots, grepl, pattern = ".zip")]
  do.call(combine_to_tibble, c(zip_dots))
}


do_aquarius_downloads <- function(sites_parameters, host, ...) {
  task_names <- paste(sites_parameters$site_no, sites_parameters$parameter, sep = "_")
  
  download_timeseries_info_step <- create_task_step(
    step_name = "timeseries_info",
    target_name = function(task_name, ...) {
      sprintf('timeseries_info_%s', task_name)
    },
    command = function(task_name, ...) {
      site <- strsplit(task_name, split = '_')[[1]][1]
      parameter <- strsplit(task_name, split = '_')[[1]][2]
      psprintf("get_aq_ts_ids(site = I('%s')," = site,
                              "param = I('%s')," = parameter,
                               "host = I('%s'))" = host)
    }
  )
  
  aquarius_download_step <- create_task_step(
    step_name = "aquarius_download",
    target_name = function(task_name, ...) {
      sprintf("1_fetch/out/aquarius_download_%s.rds", task_name)
    },
    command = function(task_name, steps, ...) {
      site <- strsplit(task_name, split = '_')[[1]][1]
      parameter <- strsplit(task_name, split = '_')[[1]][2]
      psprintf("download_aquarius_raw_data(outfile = target_name, site = I('%s')," =  site,
               "parameter = I('%s')," = parameter,
                "timeseries_info = `%s`," = steps[['timeseries_info']]$target_name,
               "host = I('%s'))" = host)
    }
  )
  
  task_plan <- create_task_plan(
    task_names = task_names,
    task_steps = list(download_timeseries_info_step, aquarius_download_step),
    add_complete = FALSE)
  
  create_task_makefile(
    task_plan = task_plan,
    makefile = '1_aquarius_download.yml',
    sources = c(...),
    packages = c('tidyverse', 'lubridate', 'utc2nwislocal', 'aqapi'),
    final_targets = 'aq_inventory',
    finalize_funs = 'get_aq_inventory',
    as_promises = TRUE
  )
  
  loop_tasks(task_plan = task_plan, task_makefile = '1_aquarius_download.yml',
             num_tries = 2)
  return(remake::fetch('aq_inventory_promise', remake_file = '1_aquarius_download.yml'))
}

download_aquarius_raw_data <- function(site, parameter, timeseries_info, outfile, host) {
  refresh_aq_token(host = host)
  start_date = '2007-10-01'

  raw_timeseries <- GetTimeSeriesRawData(TimeSeriesUniqueId = timeseries_info$unique_id,
                                         host = host,
                                         QueryFrom = start_date)
  raw_data <- raw_timeseries$Points %>% 
    mutate(datetime_parsed = parse_date_time(Timestamp, orders = 'Y m d H M OSz', 
                                             tz = 'America/New_York')) %>% 
    rename(aq_raw_value = Value)
  aq_corr_timeseries <- GetTimeSeriesCorrectedData(TimeSeriesUniqueId = timeseries_info$unique_id,
                                                   host = host,
                                                   QueryFrom = start_date) 
  aq_corr_data <- aq_corr_timeseries$Points %>% 
    mutate(datetime_parsed = parse_date_time(Timestamp, orders = 'Y m d H M OSz', 
                                             tz = 'America/New_York')) %>% 
    rename(aq_corrected_value = Value)
  joined_aq_data <- full_join(raw_data, aq_corr_data, by = c('datetime_parsed', 'Timestamp'))
  saveRDS(joined_aq_data, file = outfile)
}

refresh_aq_token <- function(host) {
  if(!testToken(host)) {
    getToken(id = 'apiwwatkins', pw =rstudioapi::askForPassword(), host = host)
  }
}

get_aq_inventory <- function(...) {
  dots <- list(...)
  file_dots <- dots[purrr::map_lgl(dots, is.character)]
  do.call(combine_to_tibble, c(file_dots))
}

assemble_summarize_data_tasks <- function(sites_parameters, aquarius_data, data_bundles,
                                          param_to_predict, aq_param_to_predict, ...) {
  
  sites_parameters_to_predict <- sites_parameters %>% 
    filter(parm_cd == param_to_predict)
  task_names <- paste(sites_parameters_to_predict$site_no, sites_parameters_to_predict$parameter, sep = "_")
  read_bundle_join_data_step <- create_task_step(
    step_name = 'read_bundle_join_data',
    target_name = function(task_name, ...) {
      sprintf('2_process/out/joined_data_%s.rds', task_name)
    },
    command = function(task_name, ...) {
      site <- strsplit(task_name, split = '_')[[1]][1]
      parameter <- strsplit(task_name, split = '_')[[1]][2]
      timeseries_id_target <- paste('timeseries_info', task_name, sep = "_")
      bundle_target <- sprintf('1_fetch/zip/bundle_%s.zip', site)
      #aquarius_data_target <- sprintf('1_fetch/out/aquarius_download_%s.rds', task_name)
      #TODO: get all targets from aq download matching site number; pass in as vector
      aq_download_targets <- remake::list_targets('1_aquarius_download.yml') %>% 
        grep(pattern = '.rds', value = TRUE) %>% 
        grep(pattern = site, value = TRUE) %>% 
        paste0("'", ., "'") %>% 
        paste(collapse = ',')
      aq_param_to_predict <- remake::fetch('aq_param_to_predict')
      
      aq_ts_info_target <- remake::list_targets('1_aquarius_download.yml') %>% 
        grep(pattern = 'timeseries_info', value = TRUE) %>% 
        grep(pattern = site, value = TRUE) %>% 
        grep(pattern = aq_param_to_predict, value = TRUE)
      assert_that(length(aq_ts_info_target) == 1)
      psprintf("read_bundle_join_data(outfile = target_name, site = I('%s')," = site,
                                  "bundle = '%s'," = bundle_target,
                                 "timeseries_info = `%s`," = aq_ts_info_target,
                                  "col_to_trim = I('value_adaps_edit'),
                                    aq_param_to_predict = aq_param_to_predict, %s)" = aq_download_targets
               )
    }
  )
  
  summarize_joined_data_step <- create_task_step(
    step_name = "summarize_joined_data_step",
    target_name = function(task_name, ...) {
      sprintf('2_process/out/summary_%s.rds', task_name)
    },
    command = function(task_name, steps, ...) {
      psprintf("summarize_joined_data(outfile = target_name,
               joined_data_file = '%s')" = 
                 steps[['read_bundle_join_data']]$target_name)
    }
  )
  
  dygraph_step  <- create_task_step(
    step_name = 'dygraph_step',
    target_name = function(task_name, ...) {
      sprintf('2_process/out/dygraph_%s.html', task_name)
    },
    command = function(task_name, steps, ...) {
      site <- strsplit(task_name, split = '_')[[1]][1]
      parameter <- strsplit(task_name, split = '_')[[1]][2]
      psprintf("dygraph_edited_raw(outfile = target_name,
               joined_data_file = '%s'," = 
                 steps[['read_bundle_join_data']]$target_name,
               "summary_file = '%s'," = steps[['summarize_joined_data_step']]$target_name,
               "site_no = I('%s')," = site,
               "parameter = I('%s'))" = parameter)
    }
  )
  
  task_plan <- create_task_plan(
    task_names = task_names,
    task_steps = list(read_bundle_join_data_step, summarize_joined_data_step,
                      dygraph_step),
    add_complete = FALSE)
  
  create_task_makefile(
    task_plan = task_plan,
    makefile = "2_join_summarize_tasks.yml",
    include = c("remake.yml", "1_aquarius_download.yml", "1_bundles_download.yml"),
    sources = c(...),
    packages = c('tidyverse', 'lubridate', 'magrittr', 'dygraphs', 'xts', 'htmlwidgets'),
    final_targets = 'all_summaries',
    finalize_funs = 'combine_summaries')
  
  loop_tasks(task_plan = task_plan, task_makefile = '2_join_summarize_tasks.yml',
             num_tries = 1, n_cores = 1)
  #assemble all summaries into list
  return(remake::fetch('all_summaries_promise', remake_file = "2_join_summarize_tasks.yml"))
}
 
combine_summaries <- function(...) {
  dots <- list(...)
  summary_dots <- dots[purrr::map_lgl(dots, grepl, pattern = "summary")]
  file_names_split <- str_split(basename(unlist(summary_dots)), pattern = "_", simplify = TRUE)
  summary_tibble <- tibble(summary_file = c(summary_dots),
                           site_no = file_names_split[,2],
                           parameter = gsub(pattern = ".rds", replacement = "",
                                            x = file_names_split[,3])) %>% 
    rowwise() %>% 
    mutate(summary_object = list(readRDS(summary_file)))
  return(summary_tibble)
}

do_model_tasks <- function(joined_data_summary,
                           split_method = 'test_future', 
                           upper_bound_include,
                           lower_bound_include,
                           ...) {
  task_names <- paste(joined_data_summary$site_no, joined_data_summary$parameter,
                      sep = '_')
  split_data_step <- create_task_step(
    step_name = 'split_data',
    target_name = function(task_name, ...) {
      sprintf('3_model/out/%s_split_data.rds', task_name)
    },
    command = function(task_name, ...) {
      joined_data_file <- paste0('2_process/out/joined_data_', task_name, ".rds")
      
      psprintf("create_train_test(outfile = target_name, joined_aq_adaps_file = '%s'," = joined_data_file, 
               "split_method = I('test_future'), lower_bound_include = lower_bound,
               upper_bound_include = upper_bound)")
    }
  )
  
  #model and predict step
  model_log_reg_step <- create_task_step(
    step_name = 'logistic_regression',
    target_name = function(task_name, ...) {
      sprintf('3_model/out/%s_log_reg_output.rds', task_name)
    },
    command = function(steps, ...) {
      psprintf("model_and_predict_log_reg(outfile = target_name, 
               split_data_file = '%s')" = steps[['split_data']]$target_name)
    }
  )
  #eval model step ROC curve, maybe prec-recall curve too?
  roc_curve_step <- create_task_step(
    step_name = 'roc_curve',
    target_name = function(task_name, ...) {
      sprintf('3_model/out/%s_log_reg_roc_curve.png', task_name)
    },
    command = function(steps, ...) {
      psprintf("log_reg_roc_curve(outfile = target_name, 
               model_output_file = '%s')" = steps[['logistic_regression']]$target_name)
    }
  )
  
  eval_model_step <- create_task_step(
    step_name = 'model_eval',
    target_name = function(task_name, ...) {
      sprintf('%s_model_evalution', task_name)
    },
    command = function(steps, ...) {
      psprintf("eval_model(model_output_file = '%s')" = steps[['logistic_regression']]$target_name)
    }
  )
  
  output_timeseries_dygraph_step <- create_task_step(
    step_name = 'output_timeseries_dygraph',
    target_name = function(task_name, ...) {
      sprintf('3_model/out/dygraph_%s_model_output.html', task_name)
    },
    command = function(steps, ...) {
      psprintf("dygraph_model_output(outfile = target_name,", 
               "model_output_file = '%s'," = steps[['logistic_regression']]$target_name,
               "split_data_file = '%s'," = steps[['split_data']]$target_name,
               "model_eval_df = `%s`)" = steps[['model_eval']]$target_name)
    }
  )
  
  
  task_plan <- create_task_plan(
    task_names = task_names,
    task_steps = list(split_data_step, model_log_reg_step, 
                      roc_curve_step, eval_model_step, output_timeseries_dygraph_step),
    add_complete = FALSE
  )
  
  create_task_makefile(
    task_plan = task_plan,
    makefile = '3_models.yml',
    include = c('2_join_summarize_tasks.yml'),
    sources = c(...),
    packages = c('tidymodels', 'lubridate'),
    finalize_funs = c('generate_combine_model_metrics'),
    final_targets = c('model_metrics'),
    as_promises = TRUE
  )
  
  loop_tasks(task_plan = task_plan, task_makefile = '3_models.yml', num_tries = 1)
  model_metrics <- remake::fetch('model_metrics_promise', remake_file = '3_models.yml')
  #return(model_metrics)
}
