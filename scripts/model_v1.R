# author: Logan Ngai
# date: 2025-07-03

# use this script to build and write models from the terminal
# usage: 
# $ Rscript /path/to/script.R /path/to/config.yaml YYYY-MM-DD
# example:
# Rscript scripts/model_v1.R data/versions/v0/v0.015/v0.015.yaml 2025-06-01
# 
# params: 
#   @param script to be run
#   @param configuration to be read
# return:
#   @return 0 success
#   @return non-zero for error

##########################################

#' main function that models each yaml
#'
#' @param cfg list of configuration values
#' @return model 

suppressPackageStartupMessages({
  library(copernicus)
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(yaml)
})

main = function(cfg, target_date){
  root = cfg[["root"]]
  region = cfg[["region"]]
  poly = read_coastline_buffer(buffer = 100000, path = "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/data/polygons")
  species = cfg[["type"]]
  duration = cfg[["duration"]]
  variables = unlist(cfg[["predictors"]][["phy"]])
  version = cfg[["version"]]
  product = cfg[["product"]]
  model = cfg[["model"]]
  
  charlier::info(sprintf("running config %s for %s", version, format(target_date, "%Y-%m-%d")))
  
  target_month = lubridate::month(target_date)
  
  # create directories to hold results
  charlier::info("making directories")
  summary_path = file.path(root, version, "results_summary.csv")
  csv_exists = file.exists(summary_path)
  results_dir = file.path(root, version, "results", format(target_date, "%Y-%m-%d"))
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  maxent_dir = file.path(results_dir, "maxent")
  rf_dir = file.path(results_dir, "rf")
  dir.create(maxent_dir, showWarnings = FALSE)
  dir.create(rf_dir, showWarnings = FALSE)
  
  # observation and background points 
  charlier::info("filtering observation/background points")
  pts = make_obs_bkg(sp = species, random = TRUE)
  obs = pts$obs
  bkg = pts$bkg
  
  charlier::info("filtering points for target date")
  day_obs = obs %>% 
    filter(date == target_date)
  
  day_bkg = bkg %>% 
    filter(date == target_date)
  
  
  summary_row = data.frame(
    version = version,
    species = species,
    date = format(target_date, "%Y-%m-%d"),
    obs_count = nrow(day_obs),
    bkg_count = nrow(day_bkg),
    maxent_roc = NA_real_,
    maxent_accuracy = NA_real_,
    rf_roc = NA_real_,
    rf_accuracy = NA_real_
  )
  
  if(nrow(day_obs) > 0 & nrow(day_bkg) > 0){
    charlier::info("original distribution plot")
    dist_plot = original_dist(obs, bkg, target_date)  
    ggplot2::ggsave(
      filename = file.path(results_dir, "original_distribution.png"),
      plot = dist_plot,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
  
  month_obs = obs %>% filter(lubridate::month(date) == lubridate::month(target_date))
  month_bkg = bkg %>% filter(lubridate::month(date) == lubridate::month(target_date))
  
  # covariate data
  has_deptho = "deptho" %in% variables
  if(has_deptho){
    charlier::info("reading deptho raster")  
    # remove deptho from variable list temporarily
    variables = setdiff(variables, "deptho")
    
    # path to deptho raster
    deptho_path = file.path("/mnt/s1/projects/ecocast/coredata/copernicus", region, product, "static", "deptho.tif")
    
    deptho_rast = read_stars(deptho_path)
    names(deptho_rast) = "deptho"
  }
  
  charlier::info("pulling covariates")
  x = write_covs(region = region, product = product, day = target_date, vars = variables)
  
  if (has_deptho) {
    deptho_rast = st_warp(deptho_rast, x)
    x = c(x, deptho_rast, along = NA_integer_, tolerance = 1e-6)
  }
  
  # train and test data
  charlier::info("splitting train and test data")
  df = write_df(x, month_obs, month_bkg)
  prep = prep_split(seed = cfg$seed %||% 123, data = df)
  train = prep$train
  test = prep$test
  
  if (isTRUE(model$maxent)) {
    charlier::info("running maxent model")
    maxent_res = model_maxent(train, test)
    
    charlier::info("predicting over covariate raster")
    maxent_pred = predict(maxent_res$model, newdata = x, type = "cloglog")
    maxent_res$prediction_raster = maxent_pred
    
    if (!is.null(cfg[["polygons"]])) {
      charlier::info("cropping and masking to polygon")
      maxent_res$prediction_raster = st_crop(maxent_res$prediction_raster, sf::st_bbox(poly))
      maxent_res$prediction_raster = maxent_res$prediction_raster[poly]
    }
    
    stars::write_stars(maxent_res$prediction_raster, file.path(maxent_dir, "predicted_distribution.tif"))
    
    dist_plot = predicted_dist(maxent_res$prediction_raster, the_date = target_date, add_points = "all", day_obs = day_obs, day_bkg = day_bkg)
    
    
    ggplot2::ggsave(
      filename = file.path(maxent_dir, "predicted_distribution.png"),
      plot = dist_plot,
      width = 8,
      height = 6,
      dpi = 300
    )
    
    charlier::info("saving maxent results")
    saveRDS(maxent_res, file = file.path(maxent_dir, "maxent.rds"))
    save_model_plots(maxent_res, maxent_res$test_data, maxent_dir)
    
    summary_row$maxent_roc = as.numeric(maxent_res$roc_auc$.estimate)
    summary_row$maxent_accuracy = as.numeric(maxent_res$accuracy$.estimate)
  }
  
  if (isTRUE(model$rf)) {
    charlier::info("running random forest model")
    rf_res = model_rf(train, test)
    
    charlier::info("predicting over covariate raster")
    df_covs = as.data.frame(x, xy = TRUE, na.rm = FALSE)
    df_covs$pred = predict(rf_res$model, newdata = df_covs, type = "prob")[, "1"]  # probability of presence
    
    rf_pred = x[1]  # get structure from first covariate
    rf_pred[[1]] = matrix(df_covs$pred, nrow = dim(x)[1], ncol = dim(x)[2])
    names(rf_pred) = "predicted_distribution"
    rf_res$prediction_raster = rf_pred
    
    if (!is.null(cfg[["polygons"]])) {
      charlier::info("cropping and masking to polygon")
      rf_res$prediction_raster = st_crop(rf_res$prediction_raster, sf::st_bbox(poly))
      rf_res$prediction_raster = rf_res$prediction_raster[poly]
    }
    
    stars::write_stars(rf_res$prediction_raster, file.path(rf_dir, "predicted_distribution.tif"))
    
    dist_plot = predicted_dist(rf_res$prediction_raster, the_date = target_date, add_points = "all", day_obs = day_obs, day_bkg = day_bkg)
    ggplot2::ggsave(
      filename = file.path(rf_dir, "predicted_distribution.png"),
      plot = dist_plot,
      width = 8,
      height = 6,
      dpi = 300
    )
    
    charlier::info("saving random forest results")
    saveRDS(rf_res, file = file.path(rf_dir, "rf.rds"))
    save_model_plots(rf_res, rf_res$test_data, rf_dir)
    
    summary_row$rf_roc = as.numeric(rf_res$roc_auc$.estimate)
    summary_row$rf_accuracy = as.numeric(rf_res$accuracy$.estimate)
  }
  
  if (file.exists(summary_path)) {
    summary_df = read.csv(summary_path, stringsAsFactors = FALSE)
    summary_df = summary_df[summary_df$date != summary_row$date, ]
    summary_df = rbind(summary_df, summary_row)
    write.csv(summary_df, file = summary_path, row.names = FALSE)
  } else {
    write.csv(summary_row, file = summary_path, row.names = FALSE)
  }
  
}

# Source environment setup
source("/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/setup.R")

Args = argparser::arg_parser("Monthly model builder and predictor") |>
  add_argument("config", help = "path to config file") |>
  add_argument("date", help = "forecast date (YYYY-MM-DD)") |>
  parse_args()
cfg = yaml::read_yaml(Args$config)
target_date = as.Date(Args$date)

# Set working directory to root path from config
setwd(cfg[["root"]])

# Run main function
main(cfg, target_date)
