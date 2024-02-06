metric_calib_slope <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  fit <- stats::glm(observed_outcome ~ predicted_lp, data=data, family="binomial")
  coef <- fit$coef[2]
  se <- sqrt(diag(vcov(fit)))[2]
  return(data.frame(metric = "calib_slope", coef = coef, se = se))
}

metric_calib_itl <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  fit <- stats::glm(observed_outcome ~ 1, offset=predicted_lp, data=data, family="binomial")
  coef <- fit$coef[1]
  se <- sqrt(diag(vcov(fit)))[1]
  return(data.frame(metric = "calib_itl", coef = coef, se = se))
}

metric_cstat <- function(predicted_lp, observed_outcome) {
  predicted_prob <- exp(predicted_lp)/(1+exp(predicted_lp))
  roc_ci <- pROC::roc(observed_outcome, predicted_prob,ci=T)$ci[1:3]
  coef <- roc_ci[1]
  se <- (roc_ci[3] - coef)/qnorm(0.975)
  return(data.frame(metric = "cstat", coef = coef, se = se))
}


evaluate_performance_binary <- function(test_data, model) {
  predicted_lp <- predict(model, newdata = test_data)
  outcome <- names(stats::model.frame(model))[1]
  observed_outcome <- test_data[, outcome]

  rbind(metric_calib_slope(predicted_lp, observed_outcome),
        metric_calib_itl(predicted_lp, observed_outcome),
        metric_cstat(predicted_lp, observed_outcome))
}

metric_calib_slope_cont <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  fit <- lm(observed_outcome ~ predicted_lp, data=data)
  coef <- fit$coef[2]
  se <- sqrt(diag(vcov(fit)))[2]
  return(data.frame(metric = "calib_slope", coef = coef, se = se))
}

metric_calib_itl_cont <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  fit <- lm(observed_outcome ~ 1, offset=predicted_lp, data=data)
  coef <- fit$coef[1]
  se <- sqrt(diag(vcov(fit)))[1]
  return(data.frame(metric = "calib_itl", coef = coef, se = se))
}


rsq_oosse <- function(data) {
  predicted_lp <- data[,1]
  observed_outcome <- data[,2]

  mse = mean((predicted_lp-observed_outcome)^2)
  var_outcome <- var(observed_outcome)
  oosse::RsquaredSE(MSE = mse, margVar = var_outcome, SEMSE = 1, n = length(predicted_lp), corMSEMST = 0.5)["R2"]
}

mse <- function(data) {
  predicted_lp <- data[,1]
  observed_outcome <- data[,2]
  n <- length(observed_outcome)
  mse <- sum((predicted_lp - observed_outcome)^2)/n
  return(mse)
}

rsq2 <- function(data) {
  predicted_lp <- data[,1]
  observed_outcome <- data[,2]
  n <- length(observed_outcome)
  mse <- sum((predicted_lp - observed_outcome)^2)/n
  mst <- var(observed_outcome)*(n+1)/n
  rsq <- 1 - (mse/mst)
  return(rsq)
}

rsq <- function(data) {
  predicted_lp <- data[,1]
  observed_outcome <- data[,2]
  n <- length(observed_outcome)
  rss <- sum((predicted_lp - observed_outcome)^2)
  tss <- sum((observed_outcome - mean(observed_outcome)) ^ 2)
  rsq <- 1 - (rss/tss)
  return(rsq)
}


metric_rsqared <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  coef <- rsq(data)
  # Use a bootstrap to estimate the variance
  se <- sqrt(var(replicate(20, rsq(data[sample(1:nrow(data), replace = TRUE),]))))
  return(data.frame(metric = "r-squared", coef = coef, se = se))
}

metric_mse  <-  function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  coef <- mse(data)
  # Use a bootstrap to estimate the variance
  se <- sqrt(var(replicate(20, mse(data[sample(1:nrow(data), replace = TRUE),]))))
  return(data.frame(metric = "mse", coef = coef, se = se))
}

metric_rsqared_old <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  coef <- rsq(data)
  # Use a bootstrap to estimate the variance
  se <- sqrt(var(replicate(20, rsq(data[sample(1:nrow(data), replace = TRUE),]))))
  return(data.frame(metric = "r-squared old", coef = coef, se = se))
}




evaluate_performance_continuous <- function(test_data, model) {
  evaluate_performance_continuous_generic(test_data = test_data, model = model, predict_function = predict_default)
}



evaluate_performance_continuous_new_studies <- function(test_data, model){

  evaluate_performance_continuous_generic(
    test_data = test_data,
    model = model,
    predict_function = predict_with_new_intercept_data
    )

}


evaluate_performance_continuous_new_studies0 <- function(test_data, model) {
  evaluate_performance_continuous_generic(
    test_data = test_data,
    model = model,
    predict_function = predict_average_intercept
    )
}

evaluate_performance_continuous_generic <-  function(test_data, model, predict_function) {
  outcome <- names(stats::model.frame(model))[1]

  if(!is.null(test_data$int_est)){
    out_test_data <- test_data |> dplyr::filter(!int_est)
    observed_outcome <- out_test_data[, outcome]
  } else {
    observed_outcome <- test_data[, outcome]
  }


  predicted_lp <- predict_function(model = model, test_data = test_data)

  evaluate_performance_cont_obs_pred(observed_outcome, predicted_lp)

}

evaluate_performance_cont_obs_pred <- function(observed, predicted) {

  if(length(observed) != length(predicted)) stop("observed does not have the same number of observations as predicted")
  rbind(
    metric_calib_slope_cont(predicted, observed),
    metric_calib_itl_cont(predicted, observed),
    metric_rsqared(predicted, observed),
    metric_mse(predicted, observed),
    make.row.names = FALSE
  )
}



