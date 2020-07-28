get_aq_ts_ids <- function(site_id, param, host) {
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

download_adaps_site_bundle <- function(site, bundle_wsc) {
  base_url <- 'http://nwis.usgs.gov/nwists/ExportAllBundles_rollout'
  wsc <- paste0("nwis", tolower(bundle_wsc))
  bundle_name <- paste0("EX.db01.USGS.", site, ".zip")
  final_url <- file.path(base_url, wsc, bundle_name)
  bundle_output_location <- sprintf("bundles/bundle_%s.zip", site)
  download.file(url = final_url,
                destfile = sprintf("bundles/bundle_%s.zip", site))
  invisible(bundle_output_location)
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
