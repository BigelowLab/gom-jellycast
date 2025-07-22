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
  library(dplyr)
  library(yaml)
  
  dir_path = file.path("data/versions", version)
  sub_dir_path = list.dirs(dir_path, full.names = FALSE, recursive = FALSE)
  
  metrics = c("maxent_roc", "maxent_accuracy", "rf_roc", "rf_accuracy")
  summary_list = list()
  
  for (v in sub_dir_path){
    summary_path = file.path(dir_path, v, "results_summary.csv")
    config_path = file.path(dir_path, v, paste0(v, ".yaml"))
    
    # Extract species from config
    species_name = NA
    if (file.exists(config_path)){
      config = yaml::read_yaml(config_path)
      species_name = config$type
    }
    
    if (file.exists(summary_path)){
      df = read.table(summary_path, header = TRUE, sep = ",", stringsAsFactors = FALSE) %>%
        dplyr::select(any_of(metrics)) %>%
        dplyr::mutate(across(everything(), as.numeric))
      
      for (m in metrics) {
        col_vals = df[[m]]
        
        if (all(is.na(col_vals))) {
          message(sprintf("Skipping %s for %s (all NA)", m, v))
          next
        }
        
        s = summary(col_vals)
        
        stat_row = data.frame(
          version = v,
          species = species_name,
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
  
  message(sprintf("summary statistics written to: %s", output_path))
  return(summary_df)
}




library(png)
library(grid)

extract_im = function(version = "v0", 
                      v = "v0.001",
                      date = Sys.Date(),
                      model = "rf",
                      width = 800,
                      height = 600) {
  
  date_str = format(date, "%Y-%m-%d")
  path = file.path(
    "data", "versions", version, v, "results", date_str, model,
    "predicted_distribution.png"
  )
  
  img = png::readPNG(path)
  
  # Display with fixed size using grid graphics
  grid::grid.newpage()
  grid::grid.raster(
    img, 
    width = grid::unit(1, "npc"), 
    height = grid::unit(1, "npc")
  )
  
  return(invisible(img)) 
}




extract_accuracies = function(model = "maxent", 
                              date = as.Date("2023-07-01"),
                              version = "v0",
                              v = "v0.013"){
  model_path = file.path("data", "versions", version, v, "results", 
                         format(day, "%Y-%m-%d"), model, paste0(model_type, ".rds"))
  model = readRDS(model_path)
  
  
}


extract_top_model = function(version = "v0",
                             sp = "lionsmane"){
  # returns the highest performing model and metric for a given species in a list
  
  summary_path = file.path("data", "versions", version, "summary_statistics.csv")
  df = read.csv2(summary_path, header = TRUE, sep = ",") %>% 
    filter(species == sp,
           stringr::str_detect(metric, "_acc")) %>% 
    arrange(desc(Max))
  
  top_version = df$version[1]
  top_model = sub("_.*", "", df$metric[1])

  return(list(top_version, top_model))
}
  
  
  
  
  
  
  