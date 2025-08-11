# author: Logan Ngai
# date: 2025-07-22

# use this script to build and write models from the terminal
# usage: 
# $ Rscript /path/to/script.R /path/to/config.yaml YYYY-MM-DD
# example:
# Rscript scripts/model_v2.R data/versions/v2/v0.001/v0.001.yaml 2025-06-01
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

main <- function(cfg, target_date){
  root = cfg[["root"]]
  region = cfg[["region"]]
  poly = read_coastline_buffer(buffer = 100000, path = "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/data/polygons")
  species = cfg[["obs"]][["type"]]
  duration = cfg[["duration"]]
  variables = unlist(cfg[["predictors"]][["phy"]])
  version = cfg[["version"]]
  product = cfg[["product"]]
  model = cfg[["model"]]
  random_bkg = isTRUE(cfg[["random"]])
  
  charlier::info(sprintf("running config %s for %s", version, format(target_date, "%Y-%m-%d")))
  
  # create directories
  summary_path = file.path(root, version, "results_summary.csv")
  results_dir = file.path(root, version, "results", format(target_date, "%Y-%m-%d"))
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  
  # get background and observations
  charlier::info("filtering observation/background points")
  pts = if(random_bkg){
    make_obs_bkg(sp = species, random = TRUE)
  } else{
    make_obs_bkg(sp = species, random = FALSE)
  }
  
  
  target_yday = lubridate::yday(target_date)
  obs = pts$obs
  bkg = pts$bkg
  
  day_obs = obs %>% filter(date == target_date)
  day_bkg = bkg %>% filter(date == target_date)

  # for 30 day range across all years
  obs$yday = lubridate::yday(obs$date)
  bkg$yday = lubridate::yday(bkg$date)
  
  day_window = 30
  month_obs = obs %>% filter(abs(yday - target_yday) <= day_window / 2)
  month_bkg = bkg %>% filter(abs(yday - target_yday) <= day_window / 2)

  
  # 30 day range
  # month_obs = obs %>% filter(date >= target_date - 29 & date <= target_date)
  # month_bkg = bkg %>% filter(date >= target_date - 29 & date <= target_date)
  
  # original
  # month_obs = obs %>% filter(lubridate::month(date) == lubridate::month(target_date))
  # month_bkg = bkg %>% filter(lubridate::month(date) == lubridate::month(target_date))
  
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
    dist_plot = original_dist(obs, bkg, target_date)
    ggsave(file.path(results_dir, "original_distribution.png"), dist_plot, width = 8, height = 6, dpi = 300)
  }
  
  has_deptho = "deptho" %in% variables
  if(has_deptho){
    variables = setdiff(variables, "deptho")
    deptho_path = file.path("/mnt/s1/projects/ecocast/coredata/copernicus", region, product, "static", "deptho.tif")
    deptho_rast = read_stars(deptho_path)
    names(deptho_rast) = "deptho"
  }
  
  x = write_covs(region, product, target_date, variables)
  if(has_deptho){
    deptho_rast = st_warp(deptho_rast, x)
    x = c(x, deptho_rast, along = NA_integer_, tolerance = 1e-6)
  }
  
  df = write_df(x, month_obs, month_bkg)
  prep = prep_split(seed = cfg$seed %||% 123, data = df)
  train = prep$train
  test = prep$test
  
  if (isTRUE(model$maxent)) {
    charlier::info("running maxent model")
    dir.create(file.path(results_dir, "maxent"), showWarnings = FALSE)
    maxent_res = model_maxent(train, test)
    maxent_pred = predict(maxent_res$model, newdata = x, type = "cloglog")
    if (!is.null(cfg[["polygons"]])) {
      maxent_pred = st_crop(maxent_pred, st_bbox(poly))
      maxent_pred = maxent_pred[poly]
    }
    write_stars(maxent_pred, file.path(results_dir, "maxent", "predicted_distribution.tif"))
    dist_plot = predicted_dist(maxent_pred, the_date = target_date, species = species, add_points = "all", day_obs, day_bkg)
    ggsave(file.path(results_dir, "maxent", "predicted_distribution.png"), dist_plot, width = 8, height = 6, dpi = 300)
    saveRDS(maxent_res, file = file.path(results_dir, "maxent", "maxent.rds"))
    save_model_plots(maxent_res, maxent_res$test_data, file.path(results_dir, "maxent"))
    summary_row$maxent_roc = as.numeric(maxent_res$roc_auc$.estimate)
    summary_row$maxent_accuracy = as.numeric(maxent_res$accuracy$.estimate)
  }
  
  if (isTRUE(model$rf)) {
    charlier::info("running random forest model")
    dir.create(file.path(results_dir, "rf"), showWarnings = FALSE)
    rf_res = model_rf(train, test)
    df_covs = as.data.frame(x, xy = TRUE, na.rm = FALSE)
    df_covs$pred = predict(rf_res$model, newdata = df_covs, type = "prob")[, "1"]
    rf_pred = x[1]; rf_pred[[1]] = matrix(df_covs$pred, nrow = dim(x)[1], ncol = dim(x)[2]); names(rf_pred) = "predicted_distribution"
    if (!is.null(cfg[["polygons"]])) {
      rf_pred = st_crop(rf_pred, st_bbox(poly))
      rf_pred = rf_pred[poly]
    }
    write_stars(rf_pred, file.path(results_dir, "rf", "predicted_distribution.tif"))
    dist_plot = predicted_dist(rf_pred, the_date = target_date, species = species, add_points = "all", day_obs, day_bkg)
    ggsave(file.path(results_dir, "rf", "predicted_distribution.png"), dist_plot, width = 8, height = 6, dpi = 300)
    saveRDS(rf_res, file = file.path(results_dir, "rf", "rf.rds"))
    save_model_plots(rf_res, rf_res$test_data, file.path(results_dir, "rf"))
    summary_row$rf_roc = as.numeric(rf_res$roc_auc$.estimate)
    summary_row$rf_accuracy = as.numeric(rf_res$accuracy$.estimate)
  }
  
  if (isTRUE(model$brt)) {
    charlier::info("running boosted regression trees model")
    dir.create(file.path(results_dir, "brt"), showWarnings = FALSE)
    brt_res = model_brt(train, test)
    df_covs = as.data.frame(x, xy = TRUE, na.rm = FALSE)
    df_covs$pred = predict(brt_res$model, newdata = df_covs, type = "response")
    brt_pred = x[1]; brt_pred[[1]] = matrix(df_covs$pred, nrow = dim(x)[1], ncol = dim(x)[2]); names(brt_pred) = "predicted_distribution"
    if (!is.null(cfg[["polygons"]])) {
      brt_pred = st_crop(brt_pred, st_bbox(poly))
      brt_pred = brt_pred[poly]
    }
    write_stars(brt_pred, file.path(results_dir, "brt", "predicted_distribution.tif"))
    dist_plot = predicted_dist(brt_pred, the_date = target_date, species = species, add_points = "all", day_obs, day_bkg)
    ggsave(file.path(results_dir, "brt", "predicted_distribution.png"), dist_plot, width = 8, height = 6, dpi = 300)
    saveRDS(brt_res, file = file.path(results_dir, "brt", "brt.rds"))
    save_model_plots(brt_res, brt_res$test_data, file.path(results_dir, "brt"))
    summary_row$brt_roc = as.numeric(brt_res$roc_auc$.estimate)
    summary_row$brt_accuracy = as.numeric(brt_res$accuracy$.estimate)
  }
  
  if (isTRUE(model$glm)) {
    charlier::info("running general linear model")
    dir.create(file.path(results_dir, "glm"), showWarnings = FALSE)
    glm_res = model_glm(train, test)
    df_covs = as.data.frame(x, xy = TRUE, na.rm = FALSE)
    df_covs$pred = predict(glm_res$model, newdata = df_covs, type = "response")
    glm_pred = x[1]; glm_pred[[1]] = matrix(df_covs$pred, nrow = dim(x)[1], ncol = dim(x)[2]); names(glm_pred) = "predicted_distribution"
    if (!is.null(cfg[["polygons"]])) {
      glm_pred = st_crop(glm_pred, st_bbox(poly))
      glm_pred = glm_pred[poly]
    }
    write_stars(glm_pred, file.path(results_dir, "glm", "predicted_distribution.tif"))
    dist_plot = predicted_dist(glm_pred, the_date = target_date, species = species, add_points = "all", day_obs, day_bkg)
    ggsave(file.path(results_dir, "glm", "predicted_distribution.png"), dist_plot, width = 8, height = 6, dpi = 300)
    saveRDS(glm_res, file = file.path(results_dir, "glm", "glm.rds"))
    save_model_plots(glm_res, glm_res$test_data, file.path(results_dir, "glm"))
    summary_row$glm_roc = as.numeric(glm_res$roc_auc$.estimate)
    summary_row$glm_accuracy = as.numeric(glm_res$accuracy$.estimate)
  }
  
  if (isTRUE(model$nn)) {
    charlier::info("running neural network model")
    dir.create(file.path(results_dir, "nn"), showWarnings = FALSE)
    nn_res = model_nn(train, test)
    df_covs = as.data.frame(x, xy = TRUE, na.rm = FALSE)
    df_covs$pred = predict(nn_res$model, newdata = df_covs, type = "raw")[, 1]
    nn_pred = x[1]; nn_pred[[1]] = matrix(df_covs$pred, nrow = dim(x)[1], ncol = dim(x)[2]); names(nn_pred) = "predicted_distribution"
    if (!is.null(cfg[["polygons"]])) {
      nn_pred = st_crop(nn_pred, st_bbox(poly))
      nn_pred = nn_pred[poly]
    }
    write_stars(nn_pred, file.path(results_dir, "nn", "predicted_distribution.tif"))
    dist_plot = predicted_dist(nn_pred, the_date = target_date, species = species, add_points = "all", day_obs, day_bkg)
    ggsave(file.path(results_dir, "nn", "predicted_distribution.png"), dist_plot, width = 8, height = 6, dpi = 300)
    saveRDS(nn_res, file = file.path(results_dir, "nn", "nn.rds"))
    save_model_plots(nn_res, nn_res$test_data, file.path(results_dir, "nn"))
    summary_row$nn_roc = as.numeric(nn_res$roc_auc$.estimate)
    summary_row$nn_accuracy = as.numeric(nn_res$accuracy$.estimate)
  }
  

  if (file.exists(summary_path)) {
    summary_df = read.csv(summary_path, stringsAsFactors = FALSE)
    summary_df = summary_df[summary_df$date != summary_row$date, ]
    summary_df = rbind(summary_df, summary_row)
    write.csv(summary_df, summary_path, row.names = FALSE)
  } else {
    write.csv(summary_row, summary_path, row.names = FALSE)
  }
  
}


# Source environment setup
source("/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/setup.R")

Args = argparser::arg_parser("Monthly model builder and predictor", name = "model_v2.R") |>
  argparser::add_argument("config", help = "path to config file") |>
  argparser::add_argument("date", help = "forecast date (YYYY-MM-DD)") |>
  parse_args()
cfg = yaml::read_yaml(Args$config)
target_date = as.Date(Args$date)

# Set working directory to root path from config
setwd(cfg[["root"]])

# Run main function
main(cfg, target_date)