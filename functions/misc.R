make_obs_bkg = function(sp = "lionsmane"){
  #' Create list of two sf objects: observation and background points
  #'
  #' @param sp 
  #' @return observation and background points
  
  df = read_obs() %>% 
    dplyr::filter(source == "record") %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  obs = df %>% 
    dplyr::filter(type == sp) 
      
  bkg = df %>% 
    dplyr::filter(type != sp)
    
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
  
  # v = extract_points(x, obs, form = "wide") %>% 
  #   tidyr::drop_na() %>% 
  #   dplyr::mutate(presence = as.factor(1))
  # 
  # z = extract_points(x, bkg, form = "wide") %>% 
  #   tidyr::drop_na() %>% 
  #   dplyr::mutate(presence = as.factor(0))
  
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

model_maxent = function(train, test, roc = TRUE, acc = TRUE, conf = TRUE){
  #' Perform maxent modeling
  #'
  #' @param training data
  #' @param testing data
  #' @param roc
  #' @param acc
  #' @param conf
  #' @return 

  model = maxnet::maxnet(p = train$presence,
                         data = dplyr::select(train, -presence),
                         f = maxnet::maxnet.formula(train$presence, 
                         dplyr::select(train, -presence)))

  test$prediction = stats::predict(model,
                             newdata = test,
                             type = "logistic")

  eval_df = test %>%
    dplyr::mutate(presence = as.factor(presence),
           .pred = as.numeric(prediction),
           pred_class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))) %>%
    dplyr::select(presence, .pred, pred_class)

  results = list(model = model)
  
  results$test_data = eval_df
  
  if(roc){
    results$roc_auc = yardstick::roc_auc(eval_df, truth = presence, .pred)
  }
  
  if(acc){
    results$accuracy = yardstick::accuracy(eval_df, truth = presence, estimate = pred_class)
  }
  
  if(conf){
    results$confusion_matrix = yardstick::conf_mat(eval_df, truth = presence, estimate = pred_class)
  }
  
  return(results)
}
  
  
model_rf = function(train, test, ntree = 500, roc = TRUE, acc = TRUE, conf = TRUE){
  #' Perform random forest modeling
  #'
  #' @param training data
  #' @param testing data
  #' @param roc
  #' @param acc
  #' @param conf
  #' @return 

  train$presence <- factor(train$presence, levels = c(0, 1))
  test$presence <- factor(test$presence, levels = c(0, 1))
  
  model = randomForest::randomForest(
    presence ~ .,
    data = train,
    ntree = ntree,
    importance = TRUE,
    probability = TRUE
  )
  
  test$prediction = predict(
    model,
    newdata = test,
    type = "prob"
  )[,"1"]
  
  eval_df = test %>%
    dplyr::mutate(
      presence = factor(presence, levels = c(0, 1)),
      .pred = as.numeric(prediction),
      class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, class)
  
  results = list(model = model)
  
  results$test_data = eval_df
  
  if (roc) {
    results$roc_auc = yardstick::roc_auc(eval_df, truth = presence, .pred)
  }
  if (acc) {
    results$accuracy = yardstick::accuracy(eval_df, truth = presence, estimate = class)
  }
  if (conf) {
    results$confusion_matrix = yardstick::conf_mat(eval_df, truth = presence, estimate = class)
  }
  
  return(results)

}
  
  
  
  
