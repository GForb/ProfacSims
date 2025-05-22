# Investigating level 2 endogeneity

sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.4, int_pred_corr = 0.5)

data <- generate_continuous(n_studies =  1000, 100, sigmas = sigmas)

check_beta_bias <- function(data) {

  fe <- model_lm_fixed_int(data)
  re <- model_lmm_random_int_ml(data)

  coefs_fe <- coef(fe)[grep("^[xX].*", names(coef(fe)), value=FALSE)]
  coefs_re <- coef(re)[[1]][1, 2:13] |> as.numeric()

  coefs_re
  coefs_fe
  sigmas$beta_x

  fe_bias <- mean(coefs_fe - sigmas$beta_x)/sigmas$beta_x*100
  re_bias <- mean(coefs_re - sigmas$beta_x)/sigmas$beta_x*100

  return(c(fe_bias = fe_bias, re_bias = re_bias))
}

data_numbers <- data[, 2:14]

cor(data_numbers)

sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.4, int_pred_corr = 0.5)
data <- generate_continuous(n_studies =  64, study_sample_size = 200, sigmas = sigmas)

data_numbers <- data[, 2:14]

cor(data_numbers)

run_bias_check_rep <- function(n) {
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.4, int_pred_corr = 0.5)
  data <- generate_continuous(n_studies =  64, study_sample_size = 1000, sigmas = sigmas)
  check_beta_bias(data)
}

checks <- lapply(1:100, run_bias_check_rep) |> bind_rows()

mean(checks$re_bias)
mean(checks$fe_bias)
