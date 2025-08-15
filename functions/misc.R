#' make_obs_bkg <- function(sp = "lionsmane", 
#'                          bbox = read_coastline_buffer(), 
#'                          n = 500, 
#'                          random = "true abs") {
#'   #' Create list of two sf objects: observation and background points
#'   #'
#'   #' @param sp species name
#'   #' @param bbox bounding box (sf object or NULL)
#'   #' @param n number of background points if random = TRUE
#'   #' @param random logical: generate random background points?
#'   #'
#'   #' @return list(obs = sf, bkg = sf)
#'   
#'   df <- read_obs() %>% 
#'     filter(source == "record") %>% 
#'     st_as_sf(coords = c("lon", "lat"), crs = 4326) 
#'   
#'   obs <- df %>%
#'     filter(type == sp)
#'   
#'   if(!is.null(bbox)){
#'     obs <- st_crop(obs, bbox)
#'   }
#' 
#'   if (!is.null(bbox)){ 
#'     obs = st_crop(obs, bbox) 
#'     
#'     if(random == "true abs"){ 
#'       bkg = df %>% filter(type != sp) 
#'       bkg = st_crop(bkg, bbox) 
#'     } 
#'     
#'     if(random == "random"){ 
#'       # generate random geometries 
#'       bkg_points = st_sample(bbox, size = n, type = "random") 
#'       
#'       # sample attributes from obs to match the count 
#'       attr_sample = obs %>% st_drop_geometry() %>% 
#'         slice_sample(n = length(bkg_points), replace = TRUE) 
#'       
#'       # create new sf object with same columns 
#'       bkg = st_sf(attr_sample, geometry = bkg_points) 
#'     }
#'     
#'     
#'     if (random == "both") {
#'       # true absences
#'       bkg_abs <- df %>% filter(type != sp)
#'       bkg_abs <- st_crop(bkg_abs, bbox)
#'       
#'       # random background
#'       bkg_points <- st_sample(bbox, size = n, type = "random") 
#'       attr_sample <- obs %>% st_drop_geometry() %>% 
#'         slice_sample(n = length(bkg_points), replace = TRUE) 
#'       bkg_rand <- st_sf(attr_sample, geometry = bkg_points) 
#'       
#'       # combine both
#'       bkg <- rbind(bkg_abs, bkg_rand)
#'     }
#'     
#'   }
#'   
#'   return(list(obs = obs, bkg = bkg))
#' }
#' 
#' make_obs_bkg <- function(sp = "lionsmane", 
#'                          bbox = read_coastline_buffer(), 
#'                          n = 500, 
#'                          random = FALSE) {
#'   #' Create list of two sf objects: observation and background points
#'   #'
#'   #' @param sp species name
#'   #' @param bbox bounding box (sf object or NULL)
#'   #' @param n number of background points if random = TRUE
#'   #' @param random logical: generate random background points?
#'   #'
#'   #' @return list(obs = sf, bkg = sf)
#'   
#'   df <- read_obs() %>% 
#'     filter(source == "record") %>% 
#'     st_as_sf(coords = c("lon", "lat"), crs = 4326) 
#'   
#'   obs <- df %>%
#'     filter(type == sp)
#'   
#'   if(!is.null(bbox)){
#'     obs <- st_crop(obs, bbox)
#'   }
#' 
#'   if (!is.null(bbox)){ 
#'     obs = st_crop(obs, bbox) 
#'     
#'     if (random){ 
#'       # generate random geometries 
#'       bkg_points = st_sample(bbox, size = n, type = "random") 
#'       
#'       # sample attributes from obs to match the count 
#'       attr_sample = obs %>% st_drop_geometry() %>% 
#'         slice_sample(n = length(bkg_points), replace = TRUE) 
#'       
#'       # create new sf object with same columns 
#'       bkg = st_sf(attr_sample, geometry = bkg_points) 
#'     } else { 
#'         bkg = df %>% filter(type != sp) 
#'         bkg = st_crop(bkg, bbox) 
#'     } 
#'     
#'   }
#'   
#'   return(list(obs = obs, bkg = bkg))
#' }


make_obs_bkg <- function(sp = "lionsmane", 
                         bbox = read_coastline_buffer(), 
                         n = 500, 
                         random = FALSE) {
  #' Create list of two sf objects: observation and background points
  #'
  #' @param sp species name
  #' @param bbox bounding box (sf object or NULL)
  #' @param n number of background points if random = TRUE
  #' @param random logical: generate random background points?
  #'
  #' @return list(obs = sf, bkg = sf)
  
  df <- read_obs() %>% 
    filter(source == "record") %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) 
  
  obs <- df %>%
    filter(type == sp)
  
  if(!is.null(bbox)){
    obs <- st_crop(obs, bbox)
  }
  
  if (!is.null(bbox)){ 
    obs = st_crop(obs, bbox) 
    
    if (random){ 
      # generate random geometries 
      bkg_points = st_sample(bbox, size = n, type = "random") 
      
      # sample attributes from obs to match the count 
      attr_sample = obs %>% st_drop_geometry() %>% 
        slice_sample(n = length(bkg_points), replace = TRUE) 
      
      # create new sf object with same columns 
      bkg = st_sf(attr_sample, geometry = bkg_points) 
    } else { 
      bkg = df %>% filter(type != sp) 
      bkg = st_crop(bkg, bbox) 
    } 
    
  }
  
  return(list(obs = obs, bkg = bkg))
}


csv_make_obs_bkg = function(sp = "lionsmane"){
  df = read.csv("/mnt/s1/projects/ecocast/projects/gom-jellycast-dev/jellyfish.csv") %>% 
    dplyr::select(Source, Year, Month, Day, Type, Lat, Lon) %>% 
    drop_na() %>%
    mutate(Date = as.Date(sprintf("%04d-%02d-%02d", Year, Month, Day))) %>% 
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
    dplyr::select(-Year, -Month, -Day) %>%
    rename(
      date = Date,
      source = Source,
      type = Type
    ) %>% 
    filter(source == "record")
  
  obs = df %>% 
    dplyr::filter(type == sp) 
  
  bkg = df %>% 
    dplyr::filter(type != sp)
  
  return(list(obs = obs, bkg = bkg))
  
}

write_covs = function(region = "nwa", 
                      product = "GLOBAL_MULTIYEAR_PHY_001_030",
                      day = as.Date("2015-05-29"),
                      vars = c("vo")){
  #' Create copernicus stars object 
  #'
  #' @param region
  #' @param  
  #' @param vars 
  #' @return stars object
  
  nwapath = copernicus::copernicus_path(region, product)
  DB = andreas::read_database(nwapath)
  # print(str(DB))
  
  db = DB %>% 
    dplyr::filter(date == day,
                  as.character(variable) %in% vars)
  
  if (nrow(db) == 0) {
    message("No covariate data for ", as.character(day))
    return(NULL)
  }
  
  x = andreas::read_andreas(db, nwapath)
  
  return(x)
}

write_df = function(x, obs, bkg){
  #' Create data for training and testing
  #'
  #' @param x
  #' @param obs 
  #' @param bkg 
  #' @return 
  
  v = extract_points(x, obs, form = "wide") %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(presence = 1)
  
  z = extract_points(x, bkg, form = "wide") %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(presence = 0)
  
  df = dplyr::bind_rows(v, z) %>% 
    dplyr::select(-point)
  
  return(df)
}

prep_split = function(seed = 123, data = df){
  #' Create data for training and testing
  #'
  #' @param seed
  #' @param data 
  #' @return training and testing data
  
  set.seed(seed)
  split = rsample::initial_split(data, strata = presence)
  train = rsample::training(split)
  test = rsample::testing(split)
  
  return(list(train = train, test = test))
}

