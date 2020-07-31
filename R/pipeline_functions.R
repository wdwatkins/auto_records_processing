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
                                          ...) {
  task_names <- paste(sites_parameters$site_no, sites_parameters$parameter, sep = "_")
  
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
      aquarius_data_target <- sprintf('1_fetch/out/aquarius_download_%s.rds', task_name)
      psprintf("read_bundle_join_data(outfile = target_name, site = I('%s')," = site,
                                  "bundle = '%s'," = bundle_target,
                                 "timeseries_info = `%s`," = timeseries_id_target,
                                  "aquarius_data = '%s', col_to_trim = I('value_adaps_edit'))" = aquarius_data_target
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
  
  task_plan <- create_task_plan(
    task_names = task_names,
    task_steps = list(read_bundle_join_data_step, summarize_joined_data_step),
    add_complete = FALSE)
  
  create_task_makefile(
    task_plan = task_plan,
    makefile = "2_join_summarize_tasks.yml",
    include = c("remake.yml", "1_aquarius_download.yml", "1_bundles_download.yml"),
    sources = c(...),
    packages = c('tidyverse', 'lubridate'),
    final_targets = 'all_summaries',
    finalize_funs = 'combine_summaries')
  
  loop_tasks(task_plan = task_plan, task_makefile = '2_join_summarize_tasks.yml',
             num_tries = 1)
  #assemble all summaries into list
  return(remake::fetch('all_summaries_promise', remake_file = "2_join_summarize_tasks.yml"))
}
 
combine_summaries <- function(...) {
  dots <- list(...)
  summary_dots <- dots[purrr::map_lgl(dots, grepl, pattern = "summary")]
  file_names_split <- str_split(basename(unlist(summary_dots)), pattern = "_", simplify = TRUE)
  summary_tibble <- tibble(summary_file = c(summary_dots),
                           site_no = file_names_split[,2],
                           parameter = file_names_split[,3]) %>% 
    rowwise() %>% 
    mutate(summary_object = list(readRDS(summary_file)))
  return(summary_tibble)
}