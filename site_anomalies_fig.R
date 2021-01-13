library(tidyverse)

summary_files_temp <- list.files('2_process/out/', pattern = 'summary.*Temperature', full.names = TRUE)
summary_files_gh <- list.files('2_process/out/', pattern = 'summary.*Gage', full.names = TRUE)

extract_num_anomalies <- function(file_vec) {
  all_df <- tibble()
  for(file in file_vec) {
    file_contents <- readRDS(file)
    site_no <- str_extract(string = file, pattern = '[0-9]{8,}')
    file_tibble <- tibble(site_no = site_no, 
                          n_dropped_edit = file_contents$n_dropped_in_edit)
    all_df <- bind_rows(all_df, file_tibble)
  }
  site_info <- dataRetrieval::readNWISsite(all_df$site_no)
  joined_df <- left_join(all_df, site_info, by = 'site_no')
  return(joined_df)
}

temp_df <- extract_num_anomalies(summary_files_temp)
gh_df <- extract_num_anomalies(summary_files_gh)

ggplot(temp_df, aes(x = n_dropped_edit)) + geom_histogram(bins = 30) +
  scale_x_log10(labels = scales::comma) +
  labs(y = 'Number of sites', x = 'Anomalies removed in corrected data (log scale)',
       title = 'Temperature anomalies', subtitle = 'Sites with data spanning 2007-2017 (~350k data points each)')
ggsave('temp_anomalies.png')
ggplot(gh_df, aes(x = n_dropped_edit)) + geom_histogram(bins = 30) +
  scale_x_log10(labels = scales::comma) +
  labs(y = 'Number of sites', x = 'Anomalies removed in corrected data (log scale)',
       title = 'Gage height anomalies', subtitle = 'Sites with data spanning 2007-2017 (~350k data points each)')
ggsave('gageheight_anomalies.png')


states <- map_data("state")
ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group = group),
                        fill = 'tan') +
  geom_point(data = temp_df, aes(x = dec_long_va, y = dec_lat_va, color = n_dropped_edit)) +
  theme_minimal() +
  scale_color_gradient(trans = 'log')

