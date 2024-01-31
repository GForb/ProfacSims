test_that("get_error_vars", {
  R2 <- 0.7
  ICC <- 0
  vars <- get_error_vars(R2 = R2, ICC = ICC, int_pred_corr = 0)
  expect_equal(vars$sigma2_u + vars$sigma2_e + vars$pred_var, 1)
  expect_equal(vars$sigma2_u/(vars$sigma2_u + vars$sigma2_e), ICC)
  expect_equal(vars$sigma2_u+vars$pred_var/(vars$sigma2_u + vars$sigma2_e+vars$pred_var), R2)

  R2 <- 0.7
  ICC <- 0.3
  vars <- get_error_vars(R2 = R2, ICC = ICC, int_pred_corr = 0)
  expect_equal(vars$sigma2_u + vars$sigma2_e + vars$pred_var, 1)
  expect_equal(vars$sigma2_u/(vars$sigma2_u + vars$sigma2_e), ICC)
  expect_equal(vars$sigma2_u+vars$pred_var/(vars$sigma2_u + vars$sigma2_e+vars$pred_var), R2)

  R2 <- 0.7
  ICC <- 0.05
  vars <- get_error_vars(R2 = R2, ICC = ICC, int_pred_corr = 0)
  expect_equal(vars$sigma2_u + vars$sigma2_e + vars$pred_var, 1)
  expect_equal(vars$sigma2_u/(vars$sigma2_u + vars$sigma2_e), ICC)
  expect_equal(vars$sigma2_u+vars$pred_var/(vars$sigma2_u + vars$sigma2_e+vars$pred_var), R2)

  R2 <- 0.4
  ICC <- 0.2
  vars <- get_error_vars(R2 = R2, ICC = ICC, int_pred_corr = 0)
  expect_equal(vars$sigma2_u + vars$sigma2_e + vars$pred_var, 1)
  expect_equal(vars$sigma2_u/(vars$sigma2_u + vars$sigma2_e), ICC)
  expect_equal(vars$sigma2_u+vars$pred_var/(vars$sigma2_u + vars$sigma2_e+vars$pred_var), R2)

  R2 <- 0.4
  ICC <- 0.05
  vars <- get_error_vars(R2 = R2, ICC = ICC, int_pred_corr = 0)
  expect_equal(vars$sigma2_u + vars$sigma2_e + vars$pred_var, 1)
  expect_equal(vars$sigma2_u/(vars$sigma2_u + vars$sigma2_e), ICC)
  expect_equal(vars$sigma2_u+vars$pred_var/(vars$sigma2_u + vars$sigma2_e+vars$pred_var), R2)

  R2 <- 0.4
  ICC <- 0.05
  int_pred_corr <-  0.5
  vars <- get_error_vars(R2 = R2, ICC = ICC, int_pred_corr = int_pred_corr)
  expect_equal(vars$sigma2_u + vars$sigma2_e + vars$pred_var + 2*int_pred_corr*sqrt(vars$pred_var*vars$sigma2_u), 1)
  expect_equal(vars$sigma2_u/(vars$sigma2_u + vars$sigma2_e), ICC)
  expect_equal(vars$sigma2_u + vars$pred_var + 2*int_pred_corr*sqrt(vars$pred_var*vars$sigma2_u)/
                 (vars$sigma2_u + vars$sigma2_e + vars$pred_var + 2*int_pred_corr*sqrt(vars$pred_var*vars$sigma2_u))
               , R2)

  R2 <- 0.7
  ICC <- 0.3
  int_pred_corr <-  0.5
  vars <- get_error_vars(R2 = R2, ICC = ICC, int_pred_corr = int_pred_corr)
  expect_equal(vars$sigma2_u + vars$sigma2_e + vars$pred_var + 2*int_pred_corr*sqrt(vars$pred_var*vars$sigma2_u), 1)
  expect_equal(vars$sigma2_u/(vars$sigma2_u + vars$sigma2_e), ICC)
  expect_equal(vars$sigma2_u+vars$pred_var+ 2*int_pred_corr*sqrt(vars$pred_var*vars$sigma2_u)/(vars$sigma2_u + vars$sigma2_e+vars$pred_var+ 2*int_pred_corr*sqrt(vars$pred_var*vars$sigma2_u)), R2)



})


test_that("get_sigmas", {
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0, R2 = 0.7)
  expect_equal(sigmas$e^2 + 12*sigmas$beta_x^2, 1)
  expect_equal(sigmas$e^2 + 12*sigmas$beta_x^2+sigmas$u^2, 1)
  expect_equal(12*sigmas$beta_x^2, 0.7)

  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  expect_equal(sigmas$e^2 + 12*sigmas$beta_x^2+sigmas$u^2, 1)
  expect_equal(12*sigmas$beta_x^2+sigmas$u^2, 0.7)
  expect_equal(sigmas$u^2/(sigmas$u^2+sigmas$e^2), 0.3)

  # int_pred_corr <- 0.5
  # sigmas <- get_sigmas(n_predictors = 1, ICC = 0.3, R2 = 0.7, int_pred_corr = int_pred_corr)
  # expect_equal(sigmas$beta_x^2 + sigmas$u^2 + sigmas$e^2 + 2*int_pred_corr*(sigmas$u*sigmas$beta_x), 1)
  # expect_equal(sigmas$beta_x^2 + sigmas$u^2 + 2*int_pred_corr*(sigmas$u*sigmas$beta_x), 0.7)
  # expect_equal(sigmas$u^2/(sigmas$u^2+sigmas$e^2), 0.3)
  #

})

test_that("get_sigmas_single_x", {
  sigmas <- get_sigmas(ICC= 0.05, R2 = 0.4, int_pred_corr = 0, pred_icc = 0, single_x = TRUE, b_w_ratio = 1)

  expect_equal(sigmas$u^2/(sigmas$e^2+sigmas$u^2), 0.05)
  expect_equal((1+sigmas$x_w^2*sigmas$beta_w^2)/(1+sigmas$x_w^2*sigmas$beta_w^2+sigmas$e^2), 0.4)

  sigmas <- get_sigmas(ICC= 0.05, R2 = 0.4, int_pred_corr = 0, pred_icc = 0, single_x = TRUE, b_w_ratio = 2)

  expect_equal(sigmas$u^2/(sigmas$e^2+sigmas$u^2), 0.05)
  expect_equal((1+sigmas$x_w^2*sigmas$beta_w^2)/(1+sigmas$x_w^2*sigmas$beta_w^2+sigmas$e^2), 0.4)


  sigmas <- get_sigmas(ICC= 0.05, R2 = 0.4, int_pred_corr = 0.5, pred_icc = 0.25, single_x = TRUE, b_w_ratio = 3)
  expect_equal(sigmas$u^2/(sigmas$e^2+sigmas$u^2), 0.05)
  expect_equal(
    (1 + sigmas$x_w^2*sigmas$beta_w^2 + sigmas$beta_b^2*sigmas$beta_int^2)/(1+sigmas$x_w^2*sigmas$beta_w^2+ sigmas$beta_b^2*sigmas$beta_int^2+sigmas$e^2),
    0.4
  )

  sigmas <- get_sigmas(ICC= 0.05, R2 = 0.4, int_pred_corr = 0.5, pred_icc = 0.5, single_x = TRUE, b_w_ratio = 3)
  expect_equal(sigmas$u^2/(sigmas$e^2+sigmas$u^2), 0.05)
  expect_equal(
    (1 + sigmas$x_w^2*sigmas$beta_w^2 + sigmas$beta_b^2*(sigmas$beta_int^2+sigmas$x_b^2))/(1+sigmas$x_w^2*sigmas$beta_w^2+ sigmas$beta_b^2*(sigmas$beta_int^2++sigmas$x_b^2)+sigmas$e^2),
    0.4
  )
  expect_equal(0.5, (sigmas$beta_int^2 + sigmas$x_b^2)/(sigmas$beta_int^2 + sigmas$x_b^2 + sigmas$x_w^2))

})
