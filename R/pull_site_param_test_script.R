library(tidyverse)
library(assertthat)
library(aqapi)
library(dataRetrieval)
library(utc2nwislocal)
library(lubridate)

source('R/functions.R')
aquarius_host <- 'tsqa-api.nwis.usgs.gov'
getToken(id = 'apiwwatkins', pw = rstudioapi::askForPassword(),
         host = aquarius_host)
site <- '01425000'
param_aq <- 'Temperature, water'
param_nwis <- '00065'
timeseries_info <- get_aq_ts_ids(site = site,
                                 param = param_aq,
                                 host = aquarius_host)
raw_timeseries <- GetTimeSeriesRawData(TimeSeriesUniqueId = timeseries_info$unique_id,
                                       host = aquarius_host)
raw_data <- raw_timeseries$Points %>% 
  mutate(datetime_parsed = parse_date_time(Timestamp, orders = 'Y m d H M OSz', 
                                           tz = 'America/New_York'))
#get corrected from AQ
aq_corr_timeseries <- GetTimeSeriesCorrectedData(TimeSeriesUniqueId = timeseries_info$unique_id,
                                           host = aquarius_host) 
aq_corr_data <- aq_corr_timeseries$Points %>% 
  mutate(datetime_parsed = parse_date_time(Timestamp, orders = 'Y m d H M OSz', 
                                           tz = 'America/New_York')) %>% 
  rename(value_aq_cor = Value)


#grab appropriate data bundle
#bundles are stored by state..
site_info <- readNWISsite(site)
site_state <- stateCdLookup(site_info$state_cd) 
site_title_state <- tolower(str_sub(site_info$station_nm, start = -2, end = -1))
#trycatch and try state in site title, if doesn't work
bundle_zip <- tryCatch(download_adaps_site_bundle(site = site, bundle_wsc = site_state),
                       error = function(e) {
                         message("State from site service didn't work, attempting state in name")
                         download_adaps_site_bundle(site = site, 
                                                          bundle_wsc = site_title_state)})
unzip(bundle_zip, exdir = tools::file_path_sans_ext(bundle_zip),
      junkpaths = TRUE)

#identify correct file in bundle, parse using existing code
formatted_adaps_dd <- paste0("dd", zeroPad(timeseries_info$adaps_dd, 3))
bundle_edit_file_path <- file.path(tools::file_path_sans_ext(bundle_zip),
                              paste("UV.db01.USGS", site, 
                                    formatted_adaps_dd, "edit", "rdb", sep = ".")) 
bundle_meas_file_path <- file.path(tools::file_path_sans_ext(bundle_zip),
          paste("UV.db01.USGS", site, 
                formatted_adaps_dd, "meas.S", "rdb", sep = ".")) 
bundle_corr_file_path <- file.path(tools::file_path_sans_ext(bundle_zip),
                                   paste("UV.db01.USGS", site, 
                                   formatted_adaps_dd, "corr", "rdb", sep = "."))
parsed_bundle_df <- parse_bundle_rdb(bundle_edit_file_path) %>% 
  rename(adaps_edit_value = VALUE)

parsed_bundle_df_meas <- parse_bundle_rdb(bundle_meas_file_path) %>% 
  rename(adaps_meas_value = VALUE)
parsed_bundle_df_corr <- parse_bundle_rdb(bundle_meas_file_path) %>% 
  rename(adaps_corr_value = VALUE)

#join
joined_raw_corrected <- left_join(raw_data, parsed_bundle_df, 
                                  by = "datetime_parsed") %>% 
  mutate(diff = abs(Value - adaps_edit_value)) %>% 
  filter(datetime_parsed > '2007-01-01')
sum(joined_raw_corrected$Value != joined_raw_corrected$adaps_edit_value, na.rm = TRUE) 
diff_df <- slice(joined_raw_corrected, which(joined_raw_corrected$Value != joined_raw_corrected$adaps_edit_value)) 
message("Difference between raw and ADAPs edit")
summary(joined_raw_corrected$diff)

joined_meas_edit <- left_join(parsed_bundle_df_meas, parsed_bundle_df, 
                                                      by = "datetime_parsed") %>% 
  mutate(diff = abs(adaps_meas_value - adaps_edit_value)) 
diff_df_meas <- slice(joined_meas_edit, which(joined_meas_edit$Value != joined_meas_edit$adaps_edit_value))
message("Difference between ADAPS measured and edit")
summary(joined_meas_edit$diff)

nwis_data <- readNWISdata(site = site, parameterCd = param_nwis, service = "iv",
                          startDate = "1990-01-01", endDate = "2020-01-01",
                          tz = "America/New_York")
joined_nwis_raw <- left_join(raw_data, nwis_data, by = c(datetime_parsed = "dateTime"))
plot(x = joined_nwis_raw$datetime_parsed[378293:378993], y= joined_nwis_raw$Value[378293:378993, 1], ylim = c(5, 16))
lines(x = joined_nwis_raw$datetime_parsed[378293:378993], y= joined_nwis_raw$X_00065_00000[378293:378993])

aq_raw_corr <- full_join(raw_data, aq_corr_data, by = "datetime_parsed") %>% 
  mutate(diff = abs(value_aq_cor - Value))
message("Difference between AQ corrected and raw")
summary(aq_raw_corr$diff)

joined_corr_raw <- left_join(raw_data, parsed_bundle_df_corr, by = "datetime_parsed") %>% 
  mutate(diff = abs(Value - adaps_corr_value))
message("Difference between ADAPs corrected and raw")
summary(joined_corr_raw$diff)
