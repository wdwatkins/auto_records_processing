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