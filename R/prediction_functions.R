predict_default <- function(model, test_data) {
  if("glm" %in% class(model)) {
    predicted_lp <- predict(model, newdata = test_data, type = "response")
  } else {
    predicted_lp <- predict(model, newdata = test_data)
  }
  return(predicted_lp)
}

predict_with_new_intercept_data <- function(model, test_data) {
  intercept_data <- test_data |> dplyr::filter(int_est == TRUE)
  intercepts <- predict_intercepts(model,intercept_data , cluster_var = "studyid")
  # merge intercepts onto test data
  test_data_test_only <- test_data |> dplyr::filter(int_est == FALSE)
  test_data_test_only <- dplyr::left_join(test_data_test_only, intercepts, by = "studyid")
  predicted_lp <- predict_fixed(model, test_data = test_data_test_only)
  predicted_lp <- predicted_lp + test_data_test_only$pred_intercept
  return(predicted_lp)
}

predict_average_intercept <- function(model, test_data) {

  if(!is.null(test_data$int_est)){
    test_data <- test_data |> dplyr::filter(!int_est)
  }


  if(class(model)[1]== "lmerMod"){
    predicted_lp <- predict_fixed(model, test_data = test_data)
  } else if(class(model)[1]== "lm"){
    predicted_fixed <- predict_fixed(model, test_data)
    model_intercepts <- model$coefficients[grep("studyid\\d+", names(model$coefficients))]
    average_intercept <- mean(c(0, model_intercepts))
    predicted_lp <- predicted_fixed + average_intercept

  }
  return(predicted_lp)
}




predict_intercepts <- function(model, newdata, cluster_var = "studyid") {
  if(class(model)[1]== "lmerMod"){
    intercepts <- get_rand_int(model, newdata)
  } else if(class(model)[1]== "lm"){
    intercepts <- get_fixed_int_offset(model, newdata, cluster_var)
  }
  return(intercepts)
}

predict_fixed <- function(model, test_data) {
  if(class(model)[1]== "lmerMod"){
    pred  <- predict(model, newdata = test_data, re.form = NA, allow.new.levels = TRUE)
  } else if(class(model)[1]== "lm"){
    pred <- get_x_prediction(model, test_data) +  model$coefficients["(Intercept)"]
    pred <- pred[,1]
  }
  return(pred)
}

get_rand_int <- function(model, newdata) {

  outcome <- names(stats::model.frame(model))[1]

  cluster_var <- names(model@cnms)
  varCorr <- lme4::VarCorr(model) |> as.data.frame()
  var_u <-  varCorr[1,4]
  var_e <- varCorr[2,4]

  fixed_pred <- predict(model, newdata = newdata, re.form = NA)
  total_error = newdata[,outcome] - fixed_pred
  by_cluster <-  stats::aggregate(total_error, list(newdata[,cluster_var]), FUN=mean)
  counts<- stats::aggregate(newdata$studyid, list(newdata[,cluster_var]), FUN=length)
  by_cluster$n <- counts[[2]]
  colnames(by_cluster) <-  c(cluster_var, "mean_error", "n")

    R  <-  var_u/(var_u + var_e/by_cluster$n)

  by_cluster$blup = by_cluster$mean_error*R
  prediction <- by_cluster[,c(cluster_var, "blup")]
  colnames(prediction) <- c(cluster_var, "pred_intercept")


  return(prediction)
}

# This implements A framework for developing, implementing, and evaluating clinical prediction models in an individual participant data meta-analysis section 2.2.4 intercept estimation from new data
# This will probably need to be different for binary data



get_fixed_int_offset <- function(model, newdata, cluster_var) {
  outcome <- names(stats::model.frame(model))[1]

  pred <- get_x_prediction(model, newdata)
  total_error = newdata[,outcome] - pred
  by_cluster = stats::aggregate(total_error, list(newdata[,cluster_var]), FUN=mean)
  colnames(by_cluster) <-  c(cluster_var, "mean_error")

  prediction <-  by_cluster[,c(cluster_var, "mean_error")]
  colnames(prediction) <- c(cluster_var, "pred_intercept")

  return(prediction)
}

get_x_prediction <- function(model, newdata) {
  predictors <- newdata |> dplyr::select(starts_with("x"))
  predictor_names <- colnames(predictors)
  betas <- model$coef[predictor_names]
  pred_fixed <- as.matrix(predictors)%*%betas
  return(pred_fixed)
}
