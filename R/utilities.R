get_aq_ts_ids <- function(site_id, param, host) {
  refresh_aq_token(host)
  desc <- GetTimeSeriesDescriptionList(LocationIdentifier = site_id,
                                       Parameter = param,
                                       Publish = TRUE,
                                       ComputationIdentifier = "Instantaneous",
                                       host = host)
  uniqueId <- desc$TimeSeriesDescriptions$UniqueId
  assert_that(length(uniqueId) == 1) #could grep for "primary" if >1?
  extended_attributes <- desc$TimeSeriesDescriptions$ExtendedAttributes
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
  df <- importRDB1(file,
                   convertType = FALSE) %>% 
    mutate(offset_hours = nwislocal2utc_offset_hours(TZCD),
           DATETIME_tz = paste0(DATETIME, offset_to_ISO(offset_hours)),
           datetime_parsed = parse_date_time(DATETIME_tz, orders = 'YmdHMSz!*', tz = "America/New_York"),
           VALUE = as.numeric(VALUE), PRECISION = as.numeric(PRECISION))
  return(df)
}


read_and_merge_files <- function(files, value_col_names, start_date) {
  all_files_list <- list()
  for(i in seq_along(files)) {
    df <- parse_bundle_rdb(files[i]) %>% 
      rename_with(.fn = paste, 
                  .cols = !contains('date', ignore.case = TRUE),
                  value_col_names[i], sep = "_") %>% 
      filter(datetime_parsed >= start_date) 
    all_files_list[[i]] <- df 
  }
  all_files_df <- all_files_list %>% reduce(full_join) %>% 
    arrange(datetime_parsed)
  return(all_files_df)
}

read_bundle_join_data <- function(outfile, bundle_zip, site, timeseries_info, 
                                  aquarius_data_file, col_to_trim) {
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
                                          start_date = "2007-10-01")
  assert_that(!anyDuplicated(all_bundle_data$datetime_parsed))
  aquarius_data <- readRDS(aquarius_data_file)
  joined_adaps_aquarius <- full_join(all_bundle_data, aquarius_data,
                                     by = "datetime_parsed") %>% 
    select(datetime_parsed, contains('value', ignore.case = TRUE), everything()) %>% 
    rename_with(.fn = tolower) %>% 
    trim_post_adaps_data(col_to_trim = col_to_trim)
  unlink(x = files_to_read)
  saveRDS(joined_adaps_aquarius, file = outfile)
}
#' @param col_to_use will remove rows following the last non-NA value in this
#' column (ordered by date)
trim_post_adaps_data <- function(df, col_to_trim) {
  #based on edit value; may want to make 
  max_non_na_index <- df %>% arrange(datetime_parsed) %>% 
    pull(!!col_to_trim) %>% is.na() %>% not() %>% which() %>% 
    tail(n = 1)
  df %>% slice(1:max_non_na_index)
}

summarize_joined_data <- function(outfile, joined_data_file) {
  joined_data <- readRDS(joined_data_file) %>% 
    mutate(diff_raw_edit = abs(value_adaps_edit - aq_raw_value)) 
  summary_info_list <- list(quartiles = summary(joined_data$diff_raw_edit),
                            n_diff_gt_0 = sum(joined_data$diff_raw_edit > 0, na.rm = TRUE),
                            n_edit_na = sum(is.na(joined_data$value_adaps_edit)))
  saveRDS(summary_info_list, file = outfile)
}