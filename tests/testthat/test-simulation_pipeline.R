test_that("model_evaluate_pipeline", {
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  train_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas
  )
  test_data = generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas= sigmas,
    intercepts_data = train_data
  )
  cont_results <- model_evaluate_pipeline(
    train_data <- train_data,
    test_data_list = list(test_data),
    evaluate_performance = evaluate_performance_continuous,
    fit_model = model_lm_fixed_int)
  expect_equal(nrow(cont_results), 4)

  binary_results <- model_evaluate_pipeline(
    train_data = generate_cbcl(),
    test_data_list = list(generate_cbcl()),
    evaluate_performance = evaluate_performance_binary,
    fit_model = model_logistic_cbcl_test)
  expect_equal(nrow(cont_results), 4)


})


test_that("sim_rep", {
  sigmas <-  get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

  train_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas= sigmas
  )
  test_data = generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas= sigmas,
    intercepts_data = train_data
  )
  sim_results <- sim_rep(list(model_lm_fixed_int, model_lm),
                         train_data = train_data,
                         test_data = list(test_data),
                         evaluate_performance = evaluate_performance_continuous
  )
  expect_equal(nrow(sim_results), 8)
  expect_equal(ncol(sim_results), 10)


  test_data = generate_continuous_new_studies(
    n_studies = 10,
    intercept_est_sample_size = 10,
    study_sample_size = 50,
    sigmas= sigmas,
    intercepts_data = NULL,
    min_study_id = 11
  )

  sim_results <- sim_rep(list(model_lm_fixed_int, model_lmm_random_int_reml),
                         evaluate_performance = evaluate_performance_continuous_new_studies,
                         train_data = train_data, test_data = list(test_data))




  expect_equal(nrow(sim_results), 8)
  expect_equal(ncol(sim_results), 10)

})



test_that("sim_rep_continuous", {
  sigmas <-  get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

  sim_results <- sim_rep_continuous(
    list(model_lm_fixed_int, model_lm),
    n_studies = 10,
    study_sample_size_train = 500,
    study_sample_size_test = 5000,
    sigmas = sigmas
  )
  expect_equal(nrow(sim_results), 8)

})

test_that("sim_rep_continuous_new_test_studies", {
  sigmas <-  get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

  sim_results <- sim_rep_continuous_new_test_studies(
    list(model_lmm_random_int_ml, model_lm, model_lm_fixed_int),
    n_studies = 10,
    study_sample_size_train = 500,
    study_sample_size_test = 5000,
    sigmas = sigmas,
    intercept_est_sample_size = 10,
    n_studies_test = 10
  )
  expect_equal(nrow(sim_results), 12)

})



test_that("do_simulation", {
  sim_results_test <- do_simulation(nreps = 1,
                                    sim_rep_fun = sim_rep_continuous,
                                    n_studies = 5,
                                    model_function_list = list(model_lm_fixed_int, model_lm),
                                    study_sample_size_train = 50,
                                    study_sample_size_test = 500,
                                    ICC = 0.3,
                                    R2 = 0.7,
                                    int_pred_corr = 0)
  expect_equal(nrow(sim_results_test), 8)

})



test_that("ipdma_simulation", {
  set.seed(1234)
  sim_params <- list(
    nreps = 1,
    sim_rep_fun = list(sim_rep_continuous),
    n_studies = c(5, 10),
    model_function_list = list(list(model_lm_fixed_int, model_lmm_random_int_ml)),
    study_sample_size_train = c(50),
    study_sample_size_test = 500,
    ICC = c(0, 0.3),
    R2 = c(0.5, 0.7),
    int_pred_corr = c(0, 0.5)
  )

  sim_results_test <- do.call(ipdma_simulation, sim_params)


  expect_equal(nrow(sim_results_test), 128)
  expect_equal(ncol(sim_results_test), 23)

})
