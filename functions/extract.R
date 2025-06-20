extract_top_results = function(cfg, metric, n){
  #'
  #'
  #'
  
  cfg = as.character(cfg)
  version = strsplit(cfg, "\\.")[[1]][1]
  cfg_path = sprintf("data/versions/%s/%s/results_summary.csv", version, cfg)
  
  df = read.table(cfg_path, header = TRUE, sep = ",") %>% 
    slice_max(order_by = .data[[metric]], n = top_n, with_ties = FALSE)
  
  return(df)
}


extract_summary = function(version = "v0"){
  #'
  #'
  #'  
  
  dir_path = file.path("data/versions", version)
  sub_dir_path = list.dirs(dir_path, full.names = FALSE, recursive = FALSE)
  
  metrics = c("maxent_roc", "maxent_accuracy", "rf_roc", "rf_accuracy")
  summary_list = list()
  
  for (v in sub_dir_path){
    summary_path = file.path(dir_path, v, "results_summary.csv")
    if (file.exists(summary_path)){
      df = read.table(summary_path, header = TRUE, sep = ",", , stringsAsFactors = FALSE) %>%
        select(all_of(metrics)) %>% 
        mutate(across(everything(), as.numeric))
      
      for (m in metrics) {
        col_vals = as.numeric(df[[m]])
        
        if (all(is.na(col_vals))) {
          message(sprintf("Skipping %s for %s (all NA)", m, v))
          next
        }
        
        s = summary(col_vals)
        
        stat_row = data.frame(
          version = v,
          metric = m,
          Min = s[["Min."]],
          Q1 = s[["1st Qu."]],
          Median = s[["Median"]],
          Mean = s[["Mean"]],
          Q3 = s[["3rd Qu."]],
          Max = s[["Max."]]
        )
        
        summary_list[[paste(v, m, sep = "_")]] = stat_row
      }
    }
  } 
  
  summary_df = bind_rows(summary_list)
  
  output_path = file.path(dir_path, "summary_statistics.csv")
  write.csv(summary_df, output_path, row.names = FALSE)
  
  message(sprintf("Summary statistics written to: %s", output_path))
  return(summary_df)
}