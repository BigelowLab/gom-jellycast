#' Retrieve the relative path to a prediction product
#' 
#' @param version str, such as `v0`
#' @param v str, the specific version ala `v0.001`
#' @param date Date class object
#' @param model str, model type, one of `rf`, `glm`, `maxent`, `nn`, or `brt`
#' @param what str, item to retrieve
#' \itemize{
#' \item{model to retrieve the model object}
#' \item{image to retrieve the pretty image of the prediction map (default)}
#' \item{data to retrieve a stars object of the prediciton map data}
#' \item{hist to retreive a pretty image of the prediction histogram}
#' }
#' @return character path
get_prediction_path = function(version = "v0", 
                               v = "v0.001",
                               date = Sys.Date(),
                               model = "rf",
                               what = c("model", "image", "data", "hist")[2]){
  
  date_str = format(date, "%Y-%m-%d")
  
  filename = switch(tolower(what[1]),
                    "model" = paste0(model[1], ".rds"),
                    "image" = "predicted_distribution.png",
                    "data" = "predicted_distribution.tif",
                    "hist" = "hist.png")
  
  file.path("data", "versions", version, v, "results", date_str, model, filename)
}


#' Retrieve a vector of valid julian days for jellyfish modeling
#' 
#' @param form chr either "numeric" or "string"
#' @param start,end num, str or Date The starting and ending day (inclusive)
#' @param clip num, the number of days to clip at the end... useful when 
#'   forecasting toward the end of the season
#' @return numeric or string vector identifying the valid days for forecasting jellies
jelly_days = function(form = c("numeric", "string")[2],
                      start = 150, 
                      end = 250,
                      clip = 0){
  
  if (inherits(start, "Date")) start = format(start[1], "%j")
  if (inherits(end, "Date")) end = format(end[1], "%j")
  
  fini = as.numeric(end[1]) - clip
  
  x = seq(from = as.numeric(start[1]), to = fini, by = 1)
  
  if (tolower(form[1]) == "string") x = sprintf(x, "%0.3i")
  x
}

#' Test if a julian day is within the valid jellyfish modeling days
#' 
#' @param x Date, num or str the dates to test
#' @param win str, vector of julian days that are valid 
within_jelly_window = function(x = Sys.Date(),
                               win = jelly_days()){
  if (inherits(x, "Date")) x = format(x, "%j")
  if (inherits(x, "numeric")) x = sprintf("0.3i", x)
  
  x %in% win
}


#' Retrieve the relative path to a prediction product
#' 
#' @param version str, such as `v0`
#' @param v str, the specific version ala `v0.001`
#' @param date Date class object
#' @param model str, model type, one of `rf`, `glm`, `maxent`, `nn`, or `brt`
#' @param what str, item to retrieve
#' \itemize{
#' \item{model to retrieve the model object}
#' \item{image to retrieve the pretty image of the prediction map (default)}
#' \item{data to retrieve a stars object of the prediciton map data}
#' \item{hist to retreive a pretty image of the prediction histogram}
#' }
#' @return character path
get_prediction_path = function(version = "v0", 
                               v = "v0.001",
                               date = Sys.Date(),
                               model = "rf",
                               what = c("model", "image", "data", "hist")[2]){
  
  date_str = format(date, "%Y-%m-%d")
  
  filename = switch(tolower(what[1]),
                    "model" = paste0(model[1], ".rds"),
                    "image" = "predicted_distribution.png",
                    "data" = "predicted_distribution.tif",
                    "hist" = "hist.png")
  
  file.path("data", "versions", version, v, "results", date_str, model, filename)
}