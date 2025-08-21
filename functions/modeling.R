model_results <- function(eval_df, auc = TRUE, acc = TRUE, tss = TRUE, conf = TRUE){
  #' Calculate and save model results
  #' 
  #' @param eval_df data frame with columns: presence (factor 0/1), .pred (numeric), pred_class (factor 0/1)
  #' @param auc logical, compute AUC
  #' @param acc logical, compute accuracy
  #' @param tss logical, compute True Skill Statistic
  #' @param conf logical, compute confusion matrix
  #' 
  #' @return list of requested performance metrics

  results <- list()
  
  if (auc) {
    results$auc <- yardstick::roc_auc(eval_df, truth = presence, .pred)
  }
  
  if (acc) {
    results$accuracy <- yardstick::accuracy(eval_df, truth = presence, estimate = pred_class)
  }
  
  if (tss) {
    sens <- yardstick::sens(eval_df, truth = presence, estimate = pred_class)$.estimate
    spec <- yardstick::spec(eval_df, truth = presence, estimate = pred_class)$.estimate
    results$tss <- tibble::tibble(.metric = "tss", .estimate = sens + spec - 1)
  }
  
  if (conf) {
    results$confusion_matrix <- yardstick::conf_mat(eval_df, truth = presence, estimate = pred_class)
  }
  
  return(results)
}

model_maxent <- function(train, test, auc = TRUE, acc = TRUE, tss = TRUE, conf = TRUE){
  #' Perform maxent modeling
  #'
  #' @param train training data
  #' @param test testing data
  #' @param auc compute AUC
  #' @param acc compute accuracy
  #' @param conf compute confusion matrix
  #' @return list with model and performance
  
 model <- maxnet::maxnet(
    p = train$presence,
    data = dplyr::select(train, -presence),
    f = maxnet::maxnet.formula(train$presence, dplyr::select(train, -presence))
  )

 test$prediction <- stats::predict(model, newdata = test, type = "logistic")
 
 eval_df <- test %>%
   dplyr::mutate(
     presence = factor(presence, levels = c(0, 1)),
     .pred = as.numeric(prediction),
     pred_class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
   ) %>%
   dplyr::select(presence, .pred, pred_class)
 
 results <- list(model = model, test_data = eval_df)
 results <- c(results, model_results(eval_df, auc, acc, tss, conf))
 
 return(results)
}


model_rf <- function(train, test, ntree = 500, auc = TRUE, acc = TRUE, tss = TRUE, conf = TRUE){
  #' Perform random forest modeling
  #'
  #' @param train training data
  #' @param test testing data
  #' @param auc compute AUC
  #' @param acc compute accuracy
  #' @param tss compute True Skill Statistic
  #' @param conf compute confusion matrix
  #' @return list with model and performance
  
  train$presence <- factor(train$presence, levels = c(0, 1))
  test$presence <- factor(test$presence, levels = c(0, 1))
  
  model <- randomForest::randomForest(
    presence ~ .,
    data = train,
    ntree = ntree,
    importance = TRUE,
    probability = TRUE
  )
  
  test$prediction <- predict(
    model,
    newdata = test,
    type = "prob"
  )[,"1"]
  
  eval_df <- test %>%
    dplyr::mutate(
      presence = factor(presence, levels = c(0, 1)),
      .pred = as.numeric(prediction),
      pred_class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, pred_class)
  
  results <- list(model = model, test_data = eval_df)
  results <- c(results, model_results(eval_df, auc, acc, tss, conf))
  
  return(results)
}



model_brt <- function(train, test, n.trees = 100, interaction.depth = 3,
                      shrinkage = 0.01, auc = TRUE, acc = TRUE, tss = TRUE, conf = TRUE) {
  
  model <- gbm::gbm(
    presence ~ .,
    data = train,
    distribution = "bernoulli",
    n.trees = n.trees,
    interaction.depth = interaction.depth,
    shrinkage = shrinkage,
    verbose = FALSE
  )
  
  test$prediction <- stats::predict(model, newdata = test, n.trees = n.trees, type = "response")
  
  eval_df <- test %>%
    dplyr::mutate(
      presence = factor(presence, levels = c(0, 1)),
      .pred = as.numeric(prediction),
      pred_class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, pred_class)
  
  results <- list(model = model, test_data = eval_df)
  results <- c(results, model_results(eval_df, auc, acc, tss, conf))
  
  return(results)
}


model_glm <- function(train, test, auc = TRUE, acc = TRUE, tss = TRUE, conf = TRUE) {
  
  model <- stats::glm(presence ~ ., data = train, family = binomial)
  
  test$prediction <- stats::predict(model, newdata = test, type = "response")
  
  eval_df <- test %>%
    dplyr::mutate(
      presence = factor(presence, levels = c(0, 1)),
      .pred = as.numeric(prediction),
      pred_class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, pred_class)
  
  results <- list(model = model, test_data = eval_df)
  results <- c(results, model_results(eval_df, auc, acc, tss, conf))
  
  return(results)
}


model_nn <- function(train, test, size = 5, decay = 5e-4, maxit = 200,
                     auc = TRUE, acc = TRUE, tss = TRUE, conf = TRUE) {
  
  model <- nnet::nnet(
    presence ~ .,
    data = train,
    size = size,
    decay = decay,
    maxit = maxit,
    trace = FALSE
  )
  
  test$prediction <- predict(model, newdata = test, type = "raw")
  
  eval_df <- test %>%
    dplyr::mutate(
      presence = factor(presence, levels = c(0, 1)),
      .pred = as.numeric(prediction),
      pred_class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, pred_class)
  
  results <- list(model = model, test_data = eval_df)
  results <- c(results, model_results(eval_df, auc, acc, tss, conf))
  
  return(results)
}

