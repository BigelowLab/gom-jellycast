# author: Logan Ngai
# date: 2025-07-22
# usage:
# Rscript scripts/batch_v23.R \
# --dates 2025-07-22 2025-07-30 \
# --configs data/versions/v2/v2.001/v2.001.yaml data/versions/v2/v2.002/v2.002.yaml

suppressPackageStartupMessages({
  library(yaml)
  library(argparser)
})

main <- function(start_date, end_date, start_config, end_config) {
  model_script <- "/mnt/s1/projects/ecocast/projects/gom-jellycast/scripts/model_v2.R"
  config_dir <- dirname(dirname(start_config))
  
  all_configs <- list.files(config_dir, pattern = "\\.yaml$", recursive = TRUE, full.names = TRUE)
  all_configs <- sort(normalizePath(all_configs))
  
  start_idx <- which(all_configs == normalizePath(start_config))
  end_idx <- which(all_configs == normalizePath(end_config))
  
  if (start_idx <= end_idx) {
    configs <- all_configs[start_idx:end_idx]
  } else {
    configs <- all_configs[end_idx:start_idx]
  }
  
  dates <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")
  
  for (config in configs) {
    cfg <- yaml::read_yaml(config)
    version <- cfg[["version"]]
    
    for (d in dates) {
      cmd <- sprintf("Rscript %s %s %s", 
                    as.character(model_script), 
                    as.character(config), 
                    format(as.Date(d), "%Y-%m-%d"))
      
      exit_code = system(cmd)
      if (exit_code != 0) {
        warning(sprintf("batch_v0.R returned non-zero exit code for date %s", format(d, "%Y-%m-%d")))
      }
    }
  }
  
  extract_summary(version = "v2")
  invisible(0)
}

source("/mnt/s1/projects/ecocast/projects/gom-jellycast/setup.R")

Args <- arg_parser("Run multiple configs over multiple dates") %>%
  add_argument("--dates", nargs = 2, help = "start and end date (YYYY-MM-DD YYYY-MM-DD)") %>%
  add_argument("--configs", nargs = 2, help = "start and end config YAML paths") %>%
  parse_args()

main(
  start_date = as.Date(Args$dates[1]),
  end_date = as.Date(Args$dates[2]),
  start_config = Args$configs[1],
  end_config = Args$configs[2]
)

