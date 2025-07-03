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
                      model = "rf",
                      width = 800,
                      height = 600,
                      fps = 2) {
  
  png_df = extract_pngs(version, v, start_date, end_date, model)
  pngs = image_read(png_df$file)
  animation = image_animate(pngs, fps = 2)
  # image_write(animation, "rf_animation.gif")
  
  out_dir = file.path("data", "versions", version, v, "results",
                      format(start_date, "%Y-%m-%d"), model)
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_file = sprintf("%s_%s.gif",
                     format(start_date, "%Y-%m-%d"),
                     format(end_date, "%Y-%m-%d"))
  
  out_path = file.path(out_dir, out_file)
  
  image_write(animation, out_path)
  
  return(animation)
}
