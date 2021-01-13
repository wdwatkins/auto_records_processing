get_aq_ts_ids <- function(site_id, param, host) {
  refresh_aq_token(host)
  desc <- GetTimeSeriesDescriptionList(LocationIdentifier = site_id,
                                       Parameter = param,
                                       Publish = TRUE,
                                       ComputationIdentifier = "Instantaneous",
                                       host = host)
  ts_description_row <- desc$TimeSeriesDescriptions %>% 
    filter(CorrectedStartTime < '2007-10-01 00:00:00',
           CorrectedEndTime > '2017-10-01 00:00:00') %>% 
    slice_min(CorrectedStartTime)
  if(nrow(ts_description_row) > 1) {
    ts_description_row <- ts_description_row %>% 
      filter(grepl(pattern = 'PUBLISHED|PRIMARY', ignore.case = TRUE, x = Identifier))
  }
  uniqueId <- ts_description_row$UniqueId
  
  assert_that(length(uniqueId) == 1) #could grep for "primary" if >1?
  extended_attributes <- ts_description_row$ExtendedAttributes
  assert_that(length(extended_attributes) == 1)
  adaps_dd <- extended_attributes[[1]] %>% 
    filter(Name == 'ADAPS_DD') %>% 
    pull(Value)
  return(list(site_id = site_id,
              param = param,
              unique_id = uniqueId,
              adaps_dd = adaps_dd
  ))
}

download_adaps_site_bundle <- function(site, bundle_wsc, outfile) {
  base_url <- 'http://nwis.usgs.gov/nwists/ExportAllBundles_rollout'
  wsc <- paste0("nwis", tolower(bundle_wsc))
  bundle_name <- paste0("EX.db01.USGS.", site, ".zip")
  final_url <- file.path(base_url, wsc, bundle_name)
  bundle_output_location <- sprintf("bundles/bundle_%s.zip", site)
  download.file(url = final_url,
                destfile = outfile)
  invisible(bundle_output_location)
}

get_bundle_trycatch <- function(site, outfile) {
  site_info <- readNWISsite(site)
  site_state <- stateCdLookup(site_info$state_cd) 
  site_title_state <- tolower(str_sub(site_info$station_nm, start = -2, end = -1))
  #trycatch and try state in site title, if doesn't work
  bundle_zip <- tryCatch(download_adaps_site_bundle(site = site, bundle_wsc = site_state, outfile = outfile),
                         error = function(e) {
                           message("State from site service didn't work, attempting state in name")
                           download_adaps_site_bundle(site = site, 
                                                      bundle_wsc = site_title_state,
                                                      outfile = outfile)})
}

offset_to_ISO <- function(offset) {
  stopifnot(is.numeric(offset))
  stopifnot(offset < 0 && offset > -10)
  offset_absolute <- abs(offset)
  sprintf("-0%s:00", offset_absolute)
}

parse_bundle_rdb <- function(file) {
  #TODO: don't hard-code America/New_York
  print(file)
  if(file.exists(file)) {
    df <- importRDB1(file,
                     convertType = FALSE) %>% 
      mutate(offset_hours = nwislocal2utc_offset_hours(TZCD),
             DATETIME_tz = paste0(DATETIME, offset_to_ISO(offset_hours)),
             datetime_parsed = parse_date_time(DATETIME_tz, orders = 'YmdHMSz!*', tz = "America/New_York"),
             VALUE = as.numeric(VALUE), PRECISION = as.numeric(PRECISION))
  } else {
    warning("File doesn't exist, returning empty tibble instead")
    df <- tibble(datetime_parsed = NA_POSIXct_,
                 value = NA_real_)
  }

  return(df)
}


read_and_merge_files <- function(files, value_col_names, start_date) {
  all_files_list <- list()
  for(i in seq_along(files)) {
    df <- parse_bundle_rdb(files[i]) %>% 
      rename_with(.fn = paste, 
                  .cols = !contains('date', ignore.case = TRUE),
                  value_col_names[i], sep = "_") %>% 
      filter(across(any_of("datetime_parsed"), ~.x >= start_date)) 
    all_files_list[[i]] <- df 
  }
  all_files_df <- all_files_list %>% reduce(full_join) %>% 
    arrange(datetime_parsed)
  return(all_files_df)
}

read_join_aquarius_rds <- function(aquarius_data_targets, param_to_predict) {
  all_data <- tibble()
  for(file in aquarius_data_targets) {
    file_param <- str_split(file, pattern = '_', simplify = TRUE)[5] %>% 
      tools::file_path_sans_ext()
    file_data <- readRDS(file)
    if(!grepl(pattern = param_to_predict, x = file)) {
      file_data <- file_data %>% rename_with(.fn = ~ paste(file_param, .x, sep = '_'),
                                             .cols = contains('value'))
    }
    #if else maybe needed
    if(nrow(all_data) == 0) {
      all_data <- file_data
    } else {
      all_data <- full_join(all_data, file_data, by = c('Timestamp', 'datetime_parsed'))
    }
  }
  return(all_data)
}

read_bundle_join_data <- function(outfile, bundle_zip, site, timeseries_info, 
                                  col_to_trim, aq_param_to_predict, ...) {
  
  aquarius_data_targets <- c(...)
  #identify file
  bundle_path <-tools::file_path_sans_ext(bundle_zip)
  formatted_adaps_dd <- paste0("dd", zeroPad(timeseries_info$adaps_dd, 3))
  meas_file <- paste("UV.db01.USGS", site, formatted_adaps_dd, "meas.S", 
                     "rdb", sep = ".")
  edit_file <- paste("UV.db01.USGS", site, 
                     formatted_adaps_dd, "edit", "rdb", sep = ".")
  corrected_file <- paste("UV.db01.USGS", site, 
                          formatted_adaps_dd, "corr", "rdb", sep = ".")
  bundle_files <- c(meas_file, edit_file, corrected_file)
  #unzip bundle to temp
  unzip_dir <- tempdir()
  unzip(bundle_zip, files = ,
        exdir = unzip_dir, junkpaths = TRUE)
  files_to_read <- file.path(unzip_dir, bundle_files)
  all_bundle_data <- read_and_merge_files(files_to_read, 
                                          value_col_names = c("adaps_meas",
                                                              "adaps_edit",
                                                              "adaps_corr"),
                                          start_date = as_datetime('2007-10-01 00:00:00', 
                                                                   tz = "America/New_York"))
  assert_that(!anyDuplicated(all_bundle_data$datetime_parsed))
  #TODO: read all aquarius data files, join
  aquarius_data <- read_join_aquarius_rds(aquarius_data_targets, aq_param_to_predict)
  joined_adaps_aquarius <- full_join(all_bundle_data, aquarius_data,
                                     by = "datetime_parsed") %>% 
    select(datetime_parsed, contains('value', ignore.case = TRUE), everything()) %>% 
    rename_with(.fn = tolower)
  if(!all(is.na(joined_adaps_aquarius[[col_to_trim]]))) {
    joined_adaps_aquarius <- joined_adaps_aquarius %>% 
      trim_post_adaps_data(col_to_trim = col_to_trim)
  }
  unlink(x = files_to_read)
  saveRDS(joined_adaps_aquarius, file = outfile)
}

#' @param col_to_use will remove rows following the last non-NA value in this
#' column (ordered by date)
trim_post_adaps_data <- function(df, col_to_trim) {
  df_ordered <- arrange(df, datetime_parsed)
  #based on edit value; may want to make 
 max_non_na_index <- df_ordered %>% 
    pull(!!col_to_trim) %>% is.na() %>% not() %>% which() %>% 
    tail(n = 1) 
  df_ordered %>% slice(1:max_non_na_index)
}

summarize_joined_data <- function(outfile, joined_data_file) {
  joined_data <- readRDS(joined_data_file) %>% 
    mutate(diff_raw_edit = abs(value_adaps_edit - aq_raw_value)) 
  summary_info_list <- list(diff_quartiles = summary(joined_data$diff_raw_edit),
                            n_diff_gt_0 = sum(joined_data$diff_raw_edit > 0, na.rm = TRUE),
                            n_edit_na = sum(is.na(joined_data$value_adaps_edit)),
                            n_dropped_in_edit = sum(is.na(joined_data$value_adaps_edit) & !is.na(joined_data$aq_raw_value)))
  saveRDS(summary_info_list, file = outfile)
}

#' @param sample_size Sample size for each pcode
#' @param states char two-letter state codes for states to look for sites in
pull_sites <- function(states, pcodes, sample_size) {
  all_state_data <- tibble()
  for(state in states) {
    state_sites <- readNWISdata(service = 'site',
                                stateCd = state,
                                startDt = '2007-10-01',
                                parameterCd = pcodes,
                                seriesCatalogOutput = TRUE,
                                outputDataTypeCd = 'iv') %>% 
    filter_to_4_param_sites()
      
    all_state_data <- bind_rows(all_state_data, state_sites)
  }
  unique_sites <- length(unique(all_state_data$site_no))
  message(unique_sites, ' unique sites meet criteria')
  return(all_state_data)
}

join_to_aq_param_names <- function(df, aq_file_name) {
  aq_params_json <- jsonlite::fromJSON(aq_file_name, flatten = TRUE,
                                  simplifyDataFrame = TRUE)
  aq_params_tbl <- tibble(usgs_pcode = names(aq_params_json),
                          parameter = unlist(aq_params_json))
  joined_df <- left_join(df, aq_params_tbl, by = c(parm_cd = 'usgs_pcode')) 
  return(joined_df)
}

get_site_sample <- function(states, pcodes, sample_size, aq_file_name) {
  sites <- pull_sites(states = states, pcodes = pcodes, sample_size = sample_size)
  sites_aq_names <- join_to_aq_param_names(df = sites, aq_file_name = aq_file_name)
  return(sites_aq_names)
}

filter_to_4_param_sites <- function(site_service_output) {
  sites_to_skip <- c('02334480') #no data from bundle?  was all NAs
  state_4_param_sites <- site_service_output %>% 
    filter(begin_date <= '2007-10-01',
           end_date >= '2017-10-01',
           parm_cd != '00045',
           !site_no %in% sites_to_skip) %>% 
    group_by(site_no) %>% 
    filter('00010' %in% parm_cd,
           length(unique(parm_cd)) >= 4) 
}