#' returns the coastline for a given region and scale and saves it to a gpkg
#' 
#' @param region string, name of the region
#' @param scale string, resolution 
#' @param path string, path to the directory
#' @return sf object for the coastline

make_coastline = function(region = "nwa", scale = "medium", path = "data/polygons"){
  coast = rnaturalearth::ne_coastline(scale = scale, returnclass = c("sf"))
  bb = cofbb::get_bb(region, form = "sf")
  coast = sf::st_crop(coast, bb)
  filename = file.path(path, sprintf("coastline_%s_%s.gpkg", region, scale))
  sf::write_sf(coast, filename)
  return(coast)
}

#' read in coastline
#' 
#' @param region string, name of the region
#' @param scale string, resolution 
#' @param path string, path to the directory
#' @return sf object for the coastline

read_coastline = function(region = "gom", scale = "medium", path = "data/polygons"){
  filename = file.path(path, sprintf("coastline_%s_%s.gpkg", region, scale))
  coast = read_sf(filename)
  return(coast)
}

#' creates a buffer around the coastline
#' 
#' @param x sf object, coastline
#' @param buffer number, size of buffer in meters
#' @param file string/NULL, name of output file to create or NULL to not write a file
#' @return sf object of a buffered coastline

coastline_buffer = function(x = read_coastline(), buffer = 50000, file = NULL){
  sf::sf_use_s2(FALSE) #don't use s2
  eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  p = sf::st_transform(x, crs = eckertIV) |> #go to eckertIV projection
    sf::st_buffer(buffer) |> 
    sf::st_union() |> #create multipolygon around points
    st_cast("POLYGON") |>
    st_as_sf() |>
    st_transform(crs = 4326) #back to lat-lon 
  if(!is.null(file)){
    write_sf(p, file)
  }
  return(p)
}

#' reads in coastline buffer
#' 
#' @param region string, name of the region
#' @param scale string, resolution 
#' @param buffer number, size of buffer in meters
#' @param path string, path to the directory
#' @return sf object 

read_coastline_buffer = function(region = "nwa", scale = "medium", path = "data/polygons"){
  filename = file.path(path, sprintf("coastline_%s_%s.gpkg", region, scale))
  buffer = read_sf(filename)
  return(buffer)
}