test_that("ipdma_prediction_pipeline", {
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

  cont_model <- model_lm_fixed_int(train_data)
  cont_results <- ipdma_prediction_pipeline(
    test_data = test_data,
    evaluate_performance = evaluate_performance_continuous,
    model = cont_model)
  expect_equal(nrow(cont_results), 3)

  bin_model <- model_logistic_cbcl_test(generate_cbcl())
  binary_results <- ipdma_prediction_pipeline(
    test_data = generate_cbcl(),
    evaluate_performance = evaluate_performance_binary,
    model = bin_model)
  expect_equal(nrow(cont_results), 3)


})
