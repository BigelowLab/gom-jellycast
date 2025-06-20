# author: Logan Ngai
# date: 2025-06-13

# use this script to build and write models from the terminal
# usage: 
# $ Rscript /path/to/script.R /path/to/config.yaml start_date end_date
# example:
# Rscript scripts/batch_v0.R data/versions/v0/v0.001/v0.001.yaml start end
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
#' @param start_date
#' @param end_date
#' @return model

suppressPackageStartupMessages({
  library(yaml)
  library(argparser)
})

main <- function(config, start_date, end_date) {
  cfg = yaml::read_yaml(config)
  version = cfg[["version"]]
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  
  model_script = "scripts/model_v0.R"
  
  dates = as.Date(seq.Date(start_date, end_date, by = "day"))
  
  for (d in dates) {
    # cmd = sprintf("Rscript %s %s %s", model_script, config, format(d, "%Y-%m-%d"))
    cmd = sprintf("Rscript %s %s %s", 
                  as.character(model_script), 
                  as.character(config), 
                  format(as.Date(d), "%Y-%m-%d"))
    
    exit_code = system(cmd)
    if (exit_code != 0) {
      warning(sprintf("batch_v0.R returned non-zero exit code for date %s", format(d, "%Y-%m-%d")))
    }
  }
  
  extract_summary()
  
  invisible(0)
}

source("/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/setup.R")

Args = argparser::arg_parser("Batch model runner for a range of dates") |>
  add_argument("config", help = "path to config file") |>
  add_argument("start_date", help = "start date (YYYY-MM-DD)") |>
  add_argument("end_date", help = "end date (YYYY-MM-DD)") |>
  parse_args()

cfg = yaml::read_yaml(Args$config)
start_date = as.Date(Args$start_date)
end_date = as.Date(Args$end_date)

main(Args$config, start_date, end_date)