# author: Logan Ngai
# date: 2025-07-14

# use this script to build and write models from the terminal for one date
# usage: 
# $ Rscript /path/to/script.R /path/to/start_config.yaml /path/to/end_config.yaml date
# example:
# Rscript scripts/batch_v02.R 2025-07-09 \
# --configs data/versions/v1/v1.015/v1.015.yaml \
# data/versions/v1/v1.021/v1.021.yaml \
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
#' @param date
#' @return model
#' 

suppressPackageStartupMessages({
  library(yaml)
  library(argparser)
})

main <- function(date, start_config, end_config) {
  date = as.Date(date)
  model_script = "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/scripts/model_v0.R"
  
  config_dir = dirname(dirname(start_config))
  all_configs = list.files(config_dir, pattern = "\\.yaml$", recursive = TRUE, full.names = TRUE)
  all_configs = sort(all_configs)
  
  start_idx = which(normalizePath(all_configs) == normalizePath(start_config))
  end_idx   = which(normalizePath(all_configs) == normalizePath(end_config))
  
  if (start_idx <= end_idx) {
    configs = all_configs[start_idx:end_idx]
  } else {
    configs = all_configs[end_idx:start_idx]
  }
  
  for (config in configs) {
    cfg = yaml::read_yaml(config)
    version = cfg[["version"]]
    
    cat(sprintf("Running config %s for date %s\n", config, format(date, "%Y-%m-%d")))
    
    cmd = sprintf("Rscript %s %s %s",
                  as.character(model_script),
                  as.character(config),
                  format(date, "%Y-%m-%d"))
    
    exit_code = system(cmd)
    if (exit_code != 0) {
      warning(sprintf("multi_config_single_date.R returned non-zero exit code for config %s on date %s",
                      config, format(date, "%Y-%m-%d")))
    }
  }
  
  extract_summary(version = "v0")
  invisible(0)
}

source("/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/setup.R")

Args = arg_parser("Run multiple configs for a single date") %>%
  add_argument("date", help = "single date (YYYY-MM-DD)") %>%
  add_argument("--configs", nargs=2, help = "start and end config YAML files") %>%
  parse_args()

main(Args$date, Args$configs[1], Args$configs[2])