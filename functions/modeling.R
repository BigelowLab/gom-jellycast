model_maxent <- function(train, test, roc = TRUE, acc = TRUE, conf = TRUE){
  #' Perform maxent modeling
  #'
  #' @param train training data
  #' @param test testing data
  #' @param roc compute AUC
  #' @param acc compute accuracy
  #' @param conf compute confusion matrix
  #' @return list with model and performance
  
  model <- maxnet::maxnet(p = train$presence,
                         data = dplyr::select(train, -presence),
                         f = maxnet::maxnet.formula(train$presence, 
                                                    dplyr::select(train, -presence)))
  
  test$prediction <- stats::predict(model,
                                   newdata = test,
                                   type = "logistic")
  
  eval_df <- test %>%
    dplyr::mutate(presence = as.factor(presence),
                  .pred = as.numeric(prediction),
                  pred_class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))) %>%
    dplyr::select(presence, .pred, pred_class)
  
  results <- list(model = model)
  
  results$test_data <- eval_df
  
  if(roc){
    results$roc_auc <- yardstick::roc_auc(eval_df, truth = presence, .pred)
  }
  
  if(acc){
    results$accuracy <- yardstick::accuracy(eval_df, truth = presence, estimate = pred_class)
  }
  
  if(conf){
    results$confusion_matrix <- yardstick::conf_mat(eval_df, truth = presence, estimate = pred_class)
  }
  
  return(results)
}


model_rf <- function(train, test, ntree = 500, roc = TRUE, acc = TRUE, conf = TRUE){
  #' Perform random forest modeling
  #'
  #' @param train training data
  #' @param test testing data
  #' @param roc compute AUC
  #' @param acc compute accuracy
  #' @param conf compute confusion matrix
  #' @return list with model and performance
  
  train$presence <- factor(train$presence, levels = c(0, 1))
  test$presence <- factor(test$presence, levels = c(0, 1))
  
  model = randomForest::randomForest(
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
      class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, class)
  
  results <- list(model = model)
  
  results$test_data <- eval_df
  
  if (roc) {
    results$roc_auc <- yardstick::roc_auc(eval_df, truth = presence, .pred)
  }
  if (acc) {
    results$accuracy <- yardstick::accuracy(eval_df, truth = presence, estimate = class)
  }
  if (conf) {
    results$confusion_matrix <- yardstick::conf_mat(eval_df, truth = presence, estimate = class)
  }
  
  return(results)
  
}


model_brt <- function(train, test, n.trees = 100, interaction.depth = 3,
                     shrinkage = 0.01, roc = TRUE, acc = TRUE, conf = TRUE) {
  #' Perform boosted regression tree modeling
  #'
  #' @param train training data
  #' @param test testing data
  #' @param n.trees number of trees
  #' @param interaction.depth max depth of trees
  #' @param shrinkage learning rate
  #' @param roc compute AUC
  #' @param acc compute accuracy
  #' @param conf compute confusion matrix
  #' @return list with model and performance
  
  model <- gbm::gbm(
    formula = presence ~ .,
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
      class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, class)
  
  results <- list(model = model)
  results$test_data <- eval_df
  
  if (roc) {
    results$roc_auc <- yardstick::roc_auc(eval_df, truth = presence, .pred)
  }
  if (acc) {
    results$accuracy <- yardstick::accuracy(eval_df, truth = presence, estimate = class)
  }
  if (conf) {
    results$confusion_matrix <- yardstick::conf_mat(eval_df, truth = presence, estimate = class)
  }
  
  return(results)
}

model_glm <- function(train, test, roc = TRUE, acc = TRUE, conf = TRUE) {
  #' Perform GLM (logistic regression) modeling
  #'
  #' @param train training data
  #' @param test testing data
  #' @param roc compute AUC
  #' @param acc compute accuracy
  #' @param conf compute confusion matrix
  #' @return list with model and performance
  
  model <- stats::glm(presence ~ ., data = train, family = binomial)
  
  test$prediction <- stats::predict(model, newdata = test, type = "response")
  
  eval_df <- test %>%
    dplyr::mutate(
      presence = factor(presence, levels = c(0, 1)),
      .pred = as.numeric(prediction),
      class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, class)
  
  results <- list(model = model)
  results$test_data <- eval_df
  
  if (roc) {
    results$roc_auc <- yardstick::roc_auc(eval_df, truth = presence, .pred)
  }
  if (acc) {
    results$accuracy <- yardstick::accuracy(eval_df, truth = presence, estimate = class)
  }
  if (conf) {
    results$confusion_matrix <- yardstick::conf_mat(eval_df, truth = presence, estimate = class)
  }
  
  return(results)
}

model_nn = function(train, test, size = 5, decay = 5e-4, maxit = 200, roc = TRUE, acc = TRUE, conf = TRUE) {
  #' Perform artificial neural network modeling
  #'
  #' @param train training data
  #' @param test testing data
  #' @param size number of hidden units
  #' @param decay regularization parameter
  #' @param maxit max iterations
  #' @param roc compute AUC
  #' @param acc compute accuracy
  #' @param conf compute confusion matrix
  #' @return list with model and performance
  
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
      class = factor(ifelse(.pred >= 0.5, 1, 0), levels = c(0, 1))
    ) %>%
    dplyr::select(presence, .pred, class)
  
  results <- list(model = model)
  results$test_data <- eval_df
  
  if (roc) {
    results$roc_auc <- yardstick::roc_auc(eval_df, truth = presence, .pred)
  }
  if (acc) {
    results$accuracy <- yardstick::accuracy(eval_df, truth = presence, estimate = class)
  }
  if (conf) {
    results$confusion_matrix <-  yardstick::conf_mat(eval_df, truth = presence, estimate = class)
  }
  
  return(results)
}

