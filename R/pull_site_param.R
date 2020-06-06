library(tidyverse)
library(assertthat)
library(aqapi)
library(dataRetrieval)
library(utc2nwislocal)
library(lubridate)

source('R/functions.R')

#getToken....
site <- '03292494'
param_aq <- 'Temperature, water'
param_nwis <- '00065'
timeseries_info <- get_aq_ts_ids(site = site,
                                 param = param_aq)
raw_timeseries <- GetTimeSeriesRawData(TimeSeriesUniqueId = timeseries_info$unique_id,
                                       host = "tsqa.nwis.usgs.gov")
raw_data <- raw_timeseries$Points %>% 
  mutate(datetime_parsed = parse_date_time(Timestamp, orders = 'Y m d H M OSz', 
#TODO: get corrected from AQ?  
                                           
                                                                                      tz = 'America/New_York'))
#grab appropriate data bundle
#bundles are stored by state..
site_info <- readNWISsite(site)
site_state <- stateCdLookup(site_info$state_cd) 
bundle_zip <- download_adaps_site_bundle(site = site, bundle_wsc = site_state)
unzip(bundle_zip, exdir = tools::file_path_sans_ext(bundle_zip),
      junkpaths = TRUE)

#identify correct file in bundle, parse using existing code
formatted_adaps_dd <- paste0("dd", zeroPad(timeseries_info$adaps_dd, 3))
bundle_file_path <- file.path(tools::file_path_sans_ext(bundle_zip),
                              paste("UV.db01.USGS", site, 
                                    formatted_adaps_dd, "edit", "rdb", sep = ".")) 
parsed_bundle_df <- parse_bundle_rdb(bundle_file_path)

#join
joined_raw_corrected <- left_join(raw_data, parsed_bundle_df, 
                                  by = "datetime_parsed")

