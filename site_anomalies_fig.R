library(tidyverse)

summary_files_temp <- list.files('2_process/out/', pattern = 'summary.*Temperature', full.names = TRUE)
summary_files_gh <- list.files('2_process/out/', pattern = 'summary.*Gage', full.names = TRUE)

extract_num_anomalies <- function(file_vec) {
  all_df <- tibble()
  for(file in file_vec) {
    file_contents <- readRDS(file)
    site_no <- str_extract(string = file, pattern = '[0-9]{8,}')
    site_info <- dataRetrieval::readNWISsite(site_no) %>% 
      mutate(n_dropped_edit = file_contents$n_dropped_in_edit,
             project_no = as.character(project_no))
    all_df <- bind_rows(all_df, site_info)
  }
  return(all_df)
}

temp_df <- extract_num_anomalies(summary_files_temp)
gh_df <- extract_num_anomalies(summary_files_gh)

ggplot(temp_df, aes(x = n_dropped_edit)) + geom_histogram(bins = 30) +
  scale_x_log10(labels = scales::comma)

ggplot(gh_df, aes(x = n_dropped_edit)) + geom_histogram(bins = 30) +
  scale_x_log10(labels = scales::comma)
states <- map_data("state")
ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group = group),
                        fill = 'tan') +
  geom_point(data = temp_df, aes(x = dec_long_va, y = dec_lat_va, color = n_dropped_edit)) +
  theme_minimal() +
  scale_color_gradient(trans = 'log')

