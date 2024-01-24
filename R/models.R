model_lm_cbcl_test <- function(data) {
  model <- stats::lm("cbcl ~ studyid + cbcl2", data = data)
  attr(model, "name") <- "lm1"
  return(model)

}
attr(model_lm_cbcl_test, "name") <- "lm1"

model_lm_cbcl_test2 <- function(data) {
  model <- stats::lm("cbcl ~ cbcl2", data = data)
  attr(model, "name") <- "lm2"
  return(model)

}
attr(model_lm_cbcl_test2, "name") <- "lm2"

model_logistic_cbcl_test <- function(data) {
  model <- stats::glm("cbcl_bin ~ studyid + cbcl2", data = data, family = binomial)
  attr(model, "name") <- "logistic"
  return(model)

}
attr(model_logistic_cbcl_test, "name") <- "logistic"

model_lm <- function(data) {
  x_text = data |> get_x_formula_text()
  formula = paste("y ~", x_text) |> as.formula()

  model <- stats::lm(formula, data = data, weights = data$wt) # if data$wt is null lm defaults to ols
  attr(model, "name") <- "Not adjusting for study"
  return(model)

}
attr(model_lm, "name") <- "Not adjusting for study"

model_lm_fixed_int<- function(data) {
  x_text = data |> get_x_formula_text()
  formula = paste("y ~ studyid +", x_text) |> as.formula()
  model <- stats::lm(formula, data = data, weights = data$wt)
  attr(model, "name") <- "Fixed intercept"
  return(model)
}
attr(model_lm_fixed_int, "name") <- "Fixed intercept"

model_lmm_random_int_reml <- function(data) {
  x_text = data |> get_x_formula_text()
  formula = paste("y ~", x_text, "+ (1|studyid)") |> as.formula()
  model <- lme4::lmer(formula, data = data , weights = data$wt)
  attr(model, "name") <- "Random intercetp - REML"
  return(model)
}
attr(model_lmm_random_int_reml, "name") <- "Random intercetp - REML"

model_lmm_random_int_reml_weight <- function(data) {
  x_text = data |> get_x_formula_text()
  formula = paste("y ~", x_text, "+ (1|studyid)") |> as.formula()
  model <- lme4::lmer(formula, data = data, weights= data$weight)
  attr(model, "name") <- "Random intercetp - REML"
  return(model)
}
attr(model_lmm_random_int_reml_weight, "name") <- "Random intercetp - REML"


model_lmm_random_int_ml <- function(data) {
  x_text = data |> get_x_formula_text()
  formula = paste("y ~", x_text, "+ (1|studyid)") |> as.formula()
  model <- lme4::lmer(formula, data = data, REML = FALSE,  weights = data$wt)
  attr(model, "name") <- "Random intercetp - ML"
  return(model)
}
attr(model_lmm_random_int_ml, "name") <- "Random intercetp - ML"

get_x_formula_text <- function(data) {
  data |>
    dplyr::select(starts_with("x")) |>
    colnames() |>
    paste(collapse = " + ")
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

get_betas <- function(model) {
  betas = NA
  try({
    betas <- coef(model)
  })
  return(betas)
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
