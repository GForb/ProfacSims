test_that("generate_cbcl", {
  data <- generate_cbcl()
  expect_equal(nrow(data), 2768)
  expect_equal(ncol(data), 17)
})


test_that("get_sigmas", {
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0, R2 = 0.7)
  expect_equal(sigmas$e^2 + sigmas$u^2, 1)
  expect_equal(12*sigmas$beta_x^2/(sigmas$e^2 + 12*sigmas$beta_x^2), 0.7)

  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  expect_equal(sigmas$e^2 + sigmas$u^2, 1)
  expect_equal((12*sigmas$beta_x^2+sigmas$u^2)/(sigmas$e^2 + 12*sigmas$beta_x^2+sigmas$u^2), 0.7)

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
    sigmas = sigmas,
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



})
