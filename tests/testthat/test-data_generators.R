test_that("generate_cbcl", {
  data <- generate_cbcl()
  expect_equal(nrow(data), 2768)
  expect_equal(ncol(data), 17)
})



test_that("generate_predictors", {
  preds <- generate_predictors(100, 1, intercepts = 1:100, beta_int = 0.5)
  expect_equal(nrow(preds), 100)
})

test_that("generate_continuous", {
  set.seed(2344)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0, R2 = 0.7)
  train_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas
  )

  expect_equal(nrow(train_data), 500)
  expect_equal(var(train_data$study_intercept)[1],0)

  sigmas <- get_sigmas(n_predictors = 12, ICC = 0, R2 = 0.7)
  test_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas,
    intercepts_data = train_data
  )

  expect_equal(nrow(train_data), 500)


  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  test_data <- generate_continuous(
    n_studies = 50,
    study_sample_size = 200,
    sigmas = sigmas
  )

  expect_equal(nrow(test_data), 10000)
  expect_equal(var(test_data$study_intercept),sigmas$u^2,tolerance = 0.1)

  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7, int_pred_corr = 0.25)

  test_data <- generate_continuous(
    n_studies = 50,
    study_sample_size = 200,
    sigmas = sigmas
  )

  expect_equal(nrow(test_data), 10000)
  expect_equal(var(test_data$study_intercept),sigmas$u^2,tolerance = 0.1)
  expect_equal(cor(test_data$study_intercept, test_data$x1),0.25, tolerance = 0.1)

  sigmas <- get_sigmas(n_predictors = 1, ICC = 0.3, R2 = 0.7, pred_icc = 0.5)
  test_data <- generate_continuous(
    n_studies = 50,
    study_sample_size = 200,
    sigmas = sigmas,
    n_predictors = 1,
    predictor_intercepts = "random"
  )
  var_corr <- lme4::lmer(x1 ~ 1 + (1 | studyid), data = test_data) |> lme4::VarCorr() |> as.data.frame()
  expect_equal(var_corr$vcov[1] /(var_corr$vcov[1] +var_corr$vcov[1] ), 0.5, tol = 0.05 )

})

test_that("generate_continuous_new_studies", {
  sigmas <-  get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

  test_data = generate_continuous_new_studies(
    n_studies = 10,
    intercept_est_sample_size = 10,
    study_sample_size = 50,
    sigmas= sigmas,
    intercepts_data = NULL,
    min_study_id = 11
  )
  expect_equal(nrow(test_data), 600)

  train_data <-  generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas,
  )
  test_data = generate_continuous_new_studies(
    n_studies = 10,
    intercept_est_sample_size = 10,
    study_sample_size = 50,
    sigmas= sigmas,
    intercepts_data = train_data,
    min_study_id = 11
  )
  expect_equal(nrow(test_data), 600)


})

test_that("generate_continuous_new_studies_random_pred_intercepts", {
  sigmas <-  get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7, pred_icc = 0.5)

  train_data <-  generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas,
    predictor_intercepts = "random",
    n_predictors = 2
  )

  test_data = generate_continuous_new_studies(
    n_studies = 10,
    intercept_est_sample_size = 10,
    study_sample_size = 50,
    sigmas= sigmas,
    intercepts_data = train_data,
    min_study_id = 11,
    predictor_intercepts = "random",
    n_predictors = 2
  )
  expect_equal(nrow(test_data), 600)

})

test_that("count_predictors", {
  sigmas <-  get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

  test_data = generate_continuous_new_studies(
    n_studies = 10,
    intercept_est_sample_size = 10,
    study_sample_size = 50,
    sigmas= sigmas,
    intercepts_data = NULL,
    min_study_id = 11,
    n_predictors = 5
  )
  expect_equal(count_predictors(test_data), 5)
})

test_that("generate_single_predictor",{
  set.seed(1234)
  n_studies <- 1000
  study_sample_size = 10
  sigma2_u <- 1
  icc_x <- 0.05
  ro_x <- 0.2

  intercepts <- rnorm(n_studies, sqrt(sigma2_u)) |> rep(study_sample_size)

  sigma2_Xb <- get_pred_between_var(int_pred_corr = ro_x, icc_x = icc_x)
  beta_int <- get_beta_int(int_pred_corr = ro_x, sigma2_u = sigma2_u, sigma2_Xb = sigma2_Xb)

  expect_equal(check_icc_x(beta_int =beta_int, sigma2_Xb =  sigma2_Xb,icc_x = icc_x ), 0)
  expect_equal(check_int_pred_corr(int_pred_corr = ro_x, beta_int = beta_int, sigma2_Xb = sigma2_Xb),0)

  pred_intercepts<- rnorm(n_studies, sd = sqrt(sigma2_Xb)) |> rep(study_sample_size)

  x_data <- generate_single_predictor(n = n_studies*study_sample_size,
                                      outcome_intercepts = intercepts,
                                      predictor_intercepts = pred_intercepts,
                                      beta_int = beta_int,
                                      sigma_Xw = 1)

  x_data$studyid <- rep(1:n_studies, study_sample_size)
  model <- lme4::lmer("x1~ 1 + (1|studyid)", data = x_data)
  icc <- performance::icc(model)[[1]]

  expect_equal(icc, icc_x, tol = 0.01)
  expect_equal(cor(x_data$x1, intercepts), 0.2, tol = 0.01)




})

test_that("generate_continuous_single_x", {
  set.seed(1234)
  sigmas <- get_sigmas(ICC= 0.05, R2 = 0.4, int_pred_corr = 0, pred_icc = 0, single_x = TRUE, b_w_ratio = 1)
  data <- generate_continuous(sigmas = sigmas, n_studies = 1000, study_sample_size = 10)
  rand_model <- model_lmm_random_int_reml(data)
  expect_equal(performance::icc(rand_model)[[1]], 0.05, tol = 0.01)
  expect_equal(summary(model_lm(data))$r.squared, 0.4, tol = 0.2)
  expect_equal(var(data$y), 1, tol = 0.1)


  sigmas <- get_sigmas(ICC= 0.05, R2 = 0.4, int_pred_corr = 0, pred_icc = 0.5, single_x = TRUE, b_w_ratio = 1)
  data <- generate_continuous(sigmas = sigmas, n_studies = 100, study_sample_size = 100)
  fixed_model <- model_lm_fixed_int(data)
  expect_equal(summary(fixed_model)$r.squared, 0.4, tol = 0.2)

  model <- lme4::lmer("x1~ 1 + (1|studyid)", data = data)
  icc <- performance::icc(model)[[1]]

  expect_equal(icc, 0.5, tol = 0.06)
  expect_equal(var(data$y), 1, tol = 0.1)

  sigmas <- get_sigmas(ICC= 0.05, R2 = 0.4, int_pred_corr = 0.5, pred_icc = 0.5, single_x = TRUE, b_w_ratio = 1)
  data <- generate_continuous(sigmas = sigmas, n_studies = 100, study_sample_size = 100)
  expect_equal(cor(data$x1, data$study_intercept), 0.5, tol = 0.05)
  expect_equal(var(data$y), 1, tol = 0.1)

})

test_that("generate_continuous_single_x_new_data", {
  set.seed(1234)
  sigmas <- get_sigmas(ICC= 0.2, R2 = 0.4, int_pred_corr = 0.5, pred_icc = 0.5, single_x = TRUE, b_w_ratio = 1)
  train_data <- generate_continuous(sigmas = sigmas, n_studies = 1000, study_sample_size = 10)
  intercepts_data <- generate_continuous(sigmas = sigmas, n_studies = 100, study_sample_size = 10, min_study_id = 11)
  data  <- generate_continuous(sigmas = sigmas, n_studies = 100, study_sample_size = 10, min_study_id = 11, intercepts_data = intercepts_data)

  fixed_model <- model_lm_fixed_int(data)
  summary(fixed_model)$r.squared
  expect_equal(summary(fixed_model)$r.squared, 0.4, tol = 0.1)

  model <- lme4::lmer("x1~ 1 + (1|studyid)", data = data)
  icc <- performance::icc(model)[[1]]

  expect_equal(icc, 0.5, tol = 0.06)
  expect_equal(var(data$y), 1, tol = 0.1)


})



