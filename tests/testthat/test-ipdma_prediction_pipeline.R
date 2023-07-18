test_that("ipdma_prediction_pipeline", {
  sigmas <- get_sigmas(1, 1/12, 12, 0.3, 0.7)
  train_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigma_e = sigmas$e,
    sigma_u = sigmas$u
  )
  test_data = generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigma_e = sigmas$e,
    sigma_u = sigmas$u,
    train_data = train_data
  )
  cont_results <- ipdma_prediction_pipeline(
    train_data <- train_data,
    test_data = test_data,
    evaluate_performance = evaluate_performance_continuous,
    fit_model = model_lm_fixed_int)
  expect_equal(nrow(cont_results), 4)

  binary_results <- ipdma_prediction_pipeline(
    train_data = generate_cbcl(),
    test_data = generate_cbcl(),
    evaluate_performance = evaluate_performance_binary,
    fit_model = model_logistic_cbcl_test)
  expect_equal(nrow(cont_results), 4)


})


test_that("sim_rep", {
  sigmas <- get_sigmas(1, 1/12, 12, 0.3, 0.7)

  train_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigma_e = sigmas$e,
    sigma_u = sigmas$u
  )
  test_data = generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigma_e = sigmas$e,
    sigma_u = sigmas$u,
    train_data = train_data
  )
  sim_results <- sim_rep(list(model_lm_fixed_int, model_lm),
          train_data = train_data,
          test_data = test_data,
          evaluate_performance = evaluate_performance_continuous
  )
  expect_equal(nrow(sim_results), 8)
  expect_equal(ncol(sim_results), 9)

})



test_that("sim_rep_continuous", {
  sigmas <- get_sigmas(1, 1/12, 12, 0.3, 0.7)

  sim_results <- sim_rep_continuous(
    list(model_lm_fixed_int, model_lm),
    n_studies = 10,
    study_sample_size_train = 500,
    study_sample_size_test = 5000,
    sigma = sigmas
  )
  expect_equal(nrow(sim_results), 8)

})


test_that("do_simulation", {
  sim_results_test <- do_simulation(nreps = 1,
                sim_rep_fun = sim_rep_continuous,
                n_studies = 5,
                model_function_list = list(model_lm_fixed_int, model_lm),
                study_sample_size_train = 50,
                study_sample_size_test = 500,
                sigma = c(ICC = 0.3, R2 = 0.7))
  expect_equal(nrow(sim_results_test), 8)

})



test_that("ipdma_simulation", {
  sim_params <- list(
    nreps = 1,
    sim_rep_fun = list(sim_rep_continuous),
    n_studies = c(5, 10),
    model_function_list = list(list(model_lm_fixed_int, model_lm)),
    study_sample_size_train = c(50),
    study_sample_size_test = 500,
    sigma = list(c(ICC = 0.3, R2 = 0.7), c(c(ICC = 0, R2 = 0.7)))
  )

  sim_results_test <- do.call(ipdma_simulation, sim_params)


  expect_equal(nrow(sim_results_test), 32)

})

