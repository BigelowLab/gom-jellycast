# author: Logan Ngai
# date: 2025-07-03

# use this script to build and write models from the terminal for one date
# usage: 
# $ Rscript /path/to/script.R /path/to/start_config.yaml /path/to/end_config.yaml date
# example:
# Rscript scripts/batch_v11.R 2025-07-09 \
# --configs data/versions/v1/v1.015/v1.015.yaml \
# data/versions/v1/v1.021/v1.021.yaml \
# data/versions/v1/v1.028/v1.028.yaml \
# data/versions/v1/v1.033/v1.033.yaml
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

main <- function(date, configs) {
  date = as.Date(date)
  model_script = "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/scripts/model_v1.R"
  
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
  
  extract_summary()
  invisible(0)
}

source("/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/setup.R")

Args = argparser::arg_parser("Run multiple configs for a single date") %>% 
  add_argument("date", help = "single date (YYYY-MM-DD)") %>% 
  add_argument("--configs", help = "list of config YAML files", nargs = Inf) %>% 
  parse_args()

main(Args$date, Args$configs)