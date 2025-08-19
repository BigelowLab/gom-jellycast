#' read_coast = function(scale = "medium", form = "sf",
#'                       bb = sf::st_bbox(c(xmin = -77,
#'                                          ymin = 34,
#'                                          xmax = -63,
#'                                          ymax = 45.5), crs = 4326)){
#' 
#'   #' Read the coastline
#'   #'
#'   #' @param scale chr the scale of map as "small", "medium" (default) or "large"
#'   #' @param form chr one of 'sp' or 'sf' (default)
#'   #' @return geometry of the coast
#' 
#'   rnaturalearth::ne_coastline(scale = scale[1], returnclass = form[1]) |>
#'     sf::st_geometry() |>
#'     sf::st_crop(bb)
#' }

save_model_plots = function(results, 
                            test_data, 
                            output_dir, 
                            poly_path = "data/polygons/coastline_nwa_medium.gpkg"){
  #' Save ROC and prediction histogram plots
  #'
  #' @param results list returned from model_maxent or model_rf
  #' @param test_data dataframe used to evaluate the model
  #' @param polygon_path string, path to coastline polygon for masking
  #' @param output_dir directory to save plots to
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ROC 
  roc_obj = pROC::roc(test_data$presence, test_data$.pred)
  png(file.path(output_dir, "roc.png"), width = 800, height = 600)
  plot(roc_obj, main = sprintf("ROC Curve (AUC = %.2f)", pROC::auc(roc_obj)))
  dev.off()
  
  # Histogram of predicted probabilities
  png(file.path(output_dir, "hist.png"), width = 800, height = 600)
  hist(test_data$.pred,
       breaks = 20,
       col = "skyblue",
       main = "Histogram of Predicted Probabilities",
       xlab = "Predicted Probability",
       ylab = "Frequency")
  dev.off()
}

original_dist = function(obs, bkg, the_date){
  #' 
  #' 
  #' 
  #' 
  #' 
  #' 
  
  obs = obs %>% 
    filter(date == the_date) %>% 
    mutate(presence = 1)
  
  bkg = bkg %>% 
    filter(date == the_date) %>% 
    mutate(presence = 0)
  
  all = bind_rows(obs, bkg) %>% 
    mutate(presence = as.factor(presence))
  
  coast = read_coast()
  coast_bbox = st_bbox(coast)
  
  plot_data = ggplot() +
    geom_sf(data = coast, color = "black") +
    coord_sf(
      xlim = c(coast_bbox["xmin"], coast_bbox["xmax"]),
      ylim = c(coast_bbox["ymin"], coast_bbox["ymax"]),
      expand = FALSE
    ) +
    theme_minimal() +
    labs(title = paste("Observations on", format(the_date, "%Y-%m-%d"))) +
    theme(axis.title = element_blank())
  
  if (nrow(bkg) == 0) {
    plot_data = plot_data +
      geom_sf(data = obs, aes(color = factor(1)), size = 1.5)
  } else if (nrow(obs) == 0) {
    plot_data = plot_data +
      geom_sf(data = bkg, aes(color = factor(0)), size = 1.5)
  } else {
    plot_data = plot_data +
      geom_sf(data = all, aes(color = factor(presence)), size = 1.5)
  }
}

predicted_dist = function(raster, 
                          the_date,
                          species = "lionsmane",
                          add_points = "none",
                          day_obs, day_bkg){
  
  coast_buffer = read_coastline_buffer()
  coast_bbox = st_bbox(coast_buffer)
  
  labels = data.frame( 
    lon = c(-70.2553, -71.0589, -63.5752, -69.6337), 
    lat = c(43.6615, 42.3601, 44.6488, 43.8757),
    city = c("Portland", "Boston", "Halifax", "Boothbay")
  ) %>%
    mutate(
      nudge_x = ifelse(city %in% c("Portland", "Boston"), -0.2, -0.2),
      nudge_y = ifelse(city %in% c("Portland", "Boston"), 0, 0.2),
      hjust = 1
    )
  
  species_title = switch(species,
                         "lionsmane" = "Lion's Mane Jellyfish",
                         "moon" = "Moon Jellyfish",
                         "comb" = "Comb Jellyfish",
                         "whitecross" = "White Cross Jellyfish",
                         "Jellyfish")
  
  p = ggplot() +
    geom_stars(data = raster, na.rm = TRUE) +
    scale_fill_viridis_c(
      option = "C",
      name = "Jellyfish\nIndex",
      na.value = "transparent",
      limits = c(0, 1)
    ) +
    geom_sf(data = read_coastline(), color = "black", fill = NA, linewidth = 1.0) +
    coord_sf(
      xlim = c(coast_bbox["xmin"], coast_bbox["xmax"]),
      ylim = c(coast_bbox["ymin"], coast_bbox["ymax"]),
      expand = FALSE
    ) +
    labs(
      title = paste(species_title, "Sighting Forecast for", format(the_date, "%Y-%m-%d")),
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "gray80", linewidth = 0.2)) +
    
    geom_point(data = labels, aes(x = lon, y = lat),
               shape = 8, size = 5, color = "red", alpha = 0.4) +
    geom_point(data = labels, aes(x = lon, y = lat),
               shape = 8, size = 3, color = "red") +
    
    geom_text(data = labels,
              aes(x = lon, y = lat, label = city, hjust = hjust),
              nudge_x = labels$nudge_x,
              nudge_y = labels$nudge_y,
              size = 3.5, color = "black")
  
  if ((add_points %in% c("obs", "all")) && nrow(day_obs) > 0) {
    p = p + geom_sf(data = day_obs %>% filter(type == species), 
                    color = "red", size = 1, shape = 21, fill = "red", alpha = 0.6)
  }
  
  if ((add_points %in% c("bkg", "all")) && nrow(day_bkg) > 0) {
    p = p + geom_sf(data = day_bkg %>% filter(type == species), 
                    color = "blue", size = 1, shape = 21, fill = "blue", alpha = 0.4)
  }
  
  return(p)
}





mask_to_polygon = function(raster, poly, crs_proj = 3857){
  #'
  #'
  #'
  
  poly_proj = sf::st_transform(poly, crs_proj)
  raster_proj = st_transform(raster, crs_proj)
  cropped = st_crop(raster_proj, sf::st_bbox(poly_proj))
  masked = cropped[poly_proj]
  return(st_transform(masked, 4326))
}

plot_hist = function(df = df,
                     target_date = target_date,
                     vars = c("thetao")){
  long_df = df %>%
    dplyr::select(all_of(c(vars, "presence"))) %>%
    pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value")
  
  p = ggplot(long_df, aes(x = value, fill = factor(presence))) +
    geom_histogram(bins = 30, alpha = 0.8, color = "black") +
    scale_fill_manual(values = c("0" = "grey70", "1" = "dodgerblue"),
                      labels = c("Absence", "Presence")) +
    facet_grid(variable ~ presence,
               labeller = labeller(
                 presence = c(`0` = "Absence", `1` = "Presence")
               )) +
    labs(
      title = paste("Stacked Histograms on", format(target_date, "%Y-%m-%d")),
      x = "Value",
      y = "Count",
      fill = "Observation"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(p)
  
}

plot_ridge = function(df, target_date, vars = c("thetao", "deptho")) {
  df$presence = factor(df$presence, levels = c(1, 0), labels = c("Presence", "Absence"))
  
  # reshape to long format for ggplot
  long_df = df %>%
    dplyr::select(all_of(c(vars, "presence"))) %>%
    pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value")
  
  # build the ridge plot
  p = ggplot(long_df, aes(x = value, y = presence, fill = presence)) +
    geom_density_ridges(alpha = 0.7, scale = 1.2) +
    scale_fill_manual(values = c("Presence" = "dodgerblue", "Absence" = "grey70")) +
    facet_wrap(~ variable, scales = "free_x") +
    labs(
      title = paste("Ridgeline Plots on", format(target_date, "%Y-%m-%d")),
      x = "Value",
      y = "",
      fill = "Observation"
    ) +
    theme_minimal()
  
  return(p)
}

