test_that("sim_rep_", {
  set.seed(1234)
  model_function_list <- list(model_lmm_random_int_reml, model_lm_fixed_int, model_lm)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.05, R2 = 0.5)
  train_data <- generate_continuous(n_studies = 10,  study_sample_size = 50, n_predictors = 12 ,sigmas = sigmas)
  model_list <-  lapply(model_function_list, function(model_function, data) model_function(data), data = train_data)

  test_data_existing <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas,
    n_predictors = 12,
    intercepts_data = train_data)

  results_existing <- sim_rep_existing(models_list = model_list, test_data = test_data_existing)
  expect_equal(nrow(results_existing), 12)

  test_data_new <- generate_continuous_new_studies(
    intercept_est_sample_size = 50,
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas,
    n_predictors = 12,
    min_study_id = 11)

  results_new_data0 <- sim_rep_new_data0(models_list = model_list, test_data = test_data_new)
  expect_equal(nrow(results_new_data0), 12)

  results_new <-  sim_rep_new_data(models_list = model_list, test_data = test_data_new)
  expect_equal(nrow(results_new), 12)


  results_dynamic <- sim_rep_dynamic(model_function_list = model_function_list, train_data = train_data, test_data = test_data_new)
  expect_equal(nrow(results_dynamic), 39)

  results_wt <- sim_rep_weights(model_function_list = model_function_list, train_data = train_data, test_data = test_data_new)
  expect_equal(nrow(results_wt), 12)


})

test_that("sim_rep_continuous_all_eval", {
  set.seed(12345)
  model_function_list <- list(model_lmm_random_int_reml, model_lm_fixed_int, model_lm)
  sigmas <- get_sigmas(n_predictors = 10, ICC = 0.3, R2 = 0.7)
  results <- sim_rep_continuous_all_eval(
    model_function_list = model_function_list,
    n_studies = 10,
    study_sample_size_train = 50,
    study_sample_size_test = 50,
    sigmas = sigmas,
    intercept_est_sample_size = c(10, 50),
    n_studies_test = 20,
    n_predictors = 10
  )

  expect_equal(nrow(results), 210)


})

test_that("sim_rep_continuous_new_data", {
  set.seed(1234)
  model_function_list <- list(model_lmm_random_int_reml, model_lm_fixed_int, model_lm)
  sigmas <- get_sigmas(n_predictors = 10, ICC = 0.3, R2 = 0.7)
  results <- sim_rep_continuous_new_data(
    model_function_list = model_function_list,
    n_studies = 10,
    study_sample_size_train = 50,
    study_sample_size_test = 50,
    sigmas = sigmas,
    intercept_est_sample_size = c(10, 50),
    n_studies_test = 20,
    n_predictors = 10
  )

  expect_equal(nrow(results), 186)
  var_u <- results |> dplyr::filter(metric == 'var_u', model == 'Random intercetp - REML', predict_method == "new0")
  expect_equal(var_u[1,1], var_u[2,1])


})
