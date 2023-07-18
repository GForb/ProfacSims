model_lm_cbcl_test <- function(data) {
  model <- stats::lm("cbcl ~ studyid + cbcl2", data = data)
  attr(model, "name") <- "lm1"
  return(model)

}

model_lm_cbcl_test2 <- function(data) {
  model <- stats::lm("cbcl ~ cbcl2", data = data)
  attr(model, "name") <- "lm2"
  return(model)

}

model_logistic_cbcl_test <- function(data) {
  model <- stats::glm("cbcl_bin ~ studyid + cbcl2", data = data, family = binomial)
  attr(model, "name") <- "logistic"
  return(model)

}

model_lm <- function(data) {
  model <- stats::lm("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12", data = data)
  attr(model, "name") <- "lm"
  return(model)

}

model_lm_fixed_int<- function(data) {
  model <- stats::lm("y ~ studyid + x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 ", data = data)
  attr(model, "name") <- "lm_fixed_int"
  return(model)
}

model_lmm_random_int_reml <- function(data) {
  model <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)", data = data)
  attr(model, "name") <- "lmm_random_int_reml"
  return(model)
}

model_lmm_random_int_ml <- function(data) {
  model <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)", data = data, REML = FALSE)
  attr(model, "name") <- "lmm_random_int_ml"
  return(model)
}

get_var_u <- function(model) {
  var_u <- NA
  try({
    varCorr <- lme4::VarCorr(model) |> as.data.frame()
    var_u <-  varCorr[1,4]
  },
  silent = TRUE
  )
  return(var_u)
}


predict_random_int_blup <- function(model, newdata) {
  outcome <-names(model@frame)[1]
  cluster_var <- names(model@cnms)
  varCorr <- lme4::VarCorr(model) |> as.data.frame()
  var_u <-  varCorr[1,4]
  var_e <- varCorr[2,4]

  fixed_pred <- predict(model, newdata = newdata, re.form = NA)
  total_error = newdata[,outcome] - fixed_pred

  by_cluster = stats::aggregate(total_error, list(newdata[,cluster_var]), FUN=mean)
  colnames(by_cluster) <-  c(cluster_var, "mean_error")
  by_cluster$n <- table(newdata[,cluster_var])

  R  <-  var_u/(var_u + var_e/by_cluster$n)

  by_cluster$blup = by_cluster$mean_error*R
  alldata <- dplyr::left_join(newdata, by_cluster, by = cluster_var)

  return(alldata$blup)
}
