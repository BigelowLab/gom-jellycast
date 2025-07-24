# Run this script before working with this project.  It handles package installation 
# and loading, establishes the working directory and sources local functions.
#
# A. check for CRAN available packages - install missing and load all
# B. check for GITHUB available packages - install missing and load all
# C. set the working directory
# D. source each file in subdir 'functions'

#' Here we list the packages required for the project divided into group sources.
#' 
#' `cran` packages can be installed with `utils::install.packages()`
#' `github` packages can be installed with `remotes::install_github()`
packages = list(
  cran = c("here", "sf", "stars", "dplyr","tidyr","tidymodels","maxnet", 
           "yardstick", "randomForest", "copernicus", "andreas", "charlier", 
           "lubridate", "rnaturalearth", "raster", "magick", "ggridges", "png", "grid", "patchwork"),
  github = c(jellydata = "BigelowLab")
)

#' Installation and loading from CRAN
installed = installed.packages() |> dplyr::as_tibble()
for (package in packages$cran){
  if (!package %in% installed$Package) {
    cat("installing", package, "from CRAN\n")
    install.packages(package)
  }
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

#' Installation and loading from github
for (package in names(packages$github)){
  pkg = sprintf("%s/%s", packages$github[package], package)
  if (!package %in% installed$Package) {
    cat("installing", pkg, "from github\n")
    remotes::install_github(pkg)
  }
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

# define the project root
# here::i_am("setup.R")
setwd("/mnt/s1/projects/ecocast/projects/gom-jellycast-dev")

# source ancillary functions
files = list.files("functions", pattern = "^.*\\.R$", full.names = TRUE)
for (file in files) source(file)
