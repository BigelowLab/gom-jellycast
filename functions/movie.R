extract_pngs = function(version = "v0",
                        v = "v0.015",
                        start_date = as.Date("2025-07-01"),
                        end_date = as.Date("2025-07-08"),
                        model = "maxent"){
  
  dates = seq.Date(start_date, end_date, by = "day")
  
  # path to pngs  
  files = vapply(dates, function(date) {
    date_str = format(date, "%Y-%m-%d")
    file.path("data", "versions", version, v, "results", date_str, model, "predicted_distribution.png")
  }, FUN.VALUE = character(1))
  
  
  df = data.frame(
    date = dates,
    file = files,
    stringsAsFactors = FALSE
  )
  
  return(df)
}







make_movie = function(version = "v0",
                          v = "v0.015",
                          start_date = as.Date("2025-07-01"),
                          end_date = as.Date("2025-07-08"),
                          model = "maxent",
                          width = 800,
                          height = 600,
                          fps = 2) {
  
  png_df <- extract_pngs(version, v, start_date, end_date, model)
  
  if (nrow(png_df) == 0) {
    stop("No PNG files found for the specified parameters.")
  }
  
  output_gif <- sprintf("animation_%s_to_%s.gif",
                        format(min(png_df$date), "%Y%m%d"),
                        format(max(png_df$date), "%Y%m%d"))
  
  gifski::gifski_png(
    png_df$file,
    gif_file = output_gif,
    width = width,
    height = height,
    delay = 1/fps
  )
  
  message("✅ GIF saved as: ", output_gif)
  browseURL(output_gif)
}
