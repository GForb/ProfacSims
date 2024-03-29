test_that("model_evaluate_pipeline", {
  n_metrics <- 4 +1
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
  expect_equal(nrow(cont_results), n_metrics)
  expect_vector(dplyr::filter(cont_results, metric == "var_u")$betas[[1]])

  binary_results <- model_evaluate_pipeline(
    train_data = generate_cbcl(),
    test_data_list = list(generate_cbcl()),
    evaluate_performance = evaluate_performance_binary,
    fit_model = model_logistic_cbcl_test)
  expect_equal(nrow(binary_results), 4)


})

test_that("model_evaluate_pipeline_fitted_model", {
  n_metrics <- 4 +1

  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  train_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas
  )
  model = model_lm_fixed_int(train_data)
  test_data = generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas= sigmas,
    intercepts_data = train_data
  )
  cont_results <- model_evaluate_pipeline_fitted_model(
    test_data_list = list(test_data),
    evaluate_performance = evaluate_performance_continuous,
    model = model)
  expect_equal(nrow(cont_results), n_metrics)
  expect_vector(dplyr::filter(cont_results, metric == "var_u")$betas[[1]])


})


test_that("sim_rep", {
  n_metrics <- 4 +1

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
  expect_equal(nrow(sim_results), n_metrics*2)
  expect_equal(ncol(sim_results), 11)


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




  expect_equal(nrow(sim_results), n_metrics*2)
  expect_equal(ncol(sim_results), 11)

})

test_that("sim_rep_fitted_model",{
  n_metrics <- 4 + 1
  model_function_list <- list("model_lmm_random_int_reml", "model_lm_fixed_int", "model_lm")
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.05, R2 = 0.5)
  train_data <- generate_continuous(n_studies = 10,  study_sample_size = 50, n_predictors = 12 ,sigmas = sigmas)
  model_list <-  lapply(model_function_list, do.call, args = list(data = train_data))

  test_data_existing <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas,
    n_predictors = 12,
    intercepts_data = train_data)

  results <- sim_rep_fitted_model(fitted_model_list = model_list, test_data = test_data_existing, evaluate_performance = evaluate_performance_continuous)
  expect_equal(nrow(results), n_metrics*3)

  test_data1 <-test_data <-  generate_continuous_new_studies(
    n_studies = 30,
    study_sample_size = 1000,
    sigmas= sigmas,
    intercept_est_sample_size = 10,
    intercepts_data = train_data,
    min_study_id = 11,
    n_predictors = 12,
  )

  test_data2 <-test_data <-  generate_continuous_new_studies(
    n_studies = 30,
    study_sample_size = 1000,
    sigmas= sigmas,
    intercept_est_sample_size = 50,
    intercepts_data = train_data,
    min_study_id = 11,
    n_predictors = 12,
  )
  model_list <- list(model_lmm_random_int_reml(train_data))

  rep1 <- sim_rep_fitted_model(model_list, test_data= test_data1, evaluate_performance = evaluate_performance_continuous_new_studies0)
  rep2 <- sim_rep_fitted_model(model_list, test_data= test_data2, evaluate_performance = evaluate_performance_continuous_new_studies0)
  expect_equal(rep1[4,1], rep2[4,1])
})

test_that("sim_rep_continuous", {
  n_metrics <- 4+1
  sigmas <-  get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

  sim_results <- sim_rep_continuous(
    list(model_lm_fixed_int, model_lm),
    n_studies = 10,
    study_sample_size_train = 500,
    study_sample_size_test = 5000,
    sigmas = sigmas
  )
  expect_equal(nrow(sim_results), n_metrics*2)

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

  sigmas <-  get_sigmas(n_predictors = 1, ICC = 0.3, R2 = 0.7, pred_icc = 0.05, )

  sim_results <- sim_rep_continuous_new_test_studies(
    list(model_lmm_random_int_ml, model_lm, model_lm_fixed_int),
    n_studies = 10,
    study_sample_size_train = 500,
    study_sample_size_test = 5000,
    sigmas = sigmas,
    intercept_est_sample_size = 10,
    n_studies_test = 10,
    n_predictors = 1,
    predictor_intercepts = "random"
  )

  n_metrics <- 4 +1
  expect_equal(nrow(sim_results), n_metrics*3)

})



test_that("do_simulation", {
  n_metrics <- 4+1

  sim_results_test <- do_simulation(nreps = 1,
                                    sim_rep_fun = sim_rep_continuous,
                                    n_studies = 5,
                                    model_function_list = list(model_lm_fixed_int, model_lm),
                                    study_sample_size_train = 50,
                                    study_sample_size_test = 500,
                                    ICC = 0.3,
                                    R2 = 0.7,
                                    int_pred_corr = 0)
  expect_equal(nrow(sim_results_test), n_metrics*2)


  do_simulation(
    nreps = 1,
    sim_rep_fun = sim_rep_continuous_new_test_studies,
     n_studies = 4,
     model_function_list = list(model_lm_fixed_int,
                                model_lm,
                                model_lmm_random_int_reml,
                                model_lmm_random_int_ml),
     study_sample_size_train = 50,
     ICC = 0.05,
     R2 = 0.4,
     pred_icc = 0,
     int_pred_corr = 0,
     intercept_est_sample_size =100,
     n_studies_test = 30,
     study_sample_size_test = 5000,
     predictor_intercepts = "random",
     n_predictors = 1)

})



test_that("ipdma_simulation", {
  n_metrics <- 4+1

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


  expect_equal(nrow(sim_results_test), 32*n_metrics)
  expect_equal(ncol(sim_results_test), 26)

  set.seed(1234)
  sim_params <- list(
    nreps = 1,
    sim_rep_fun = list(sim_rep_continuous_new_data),
    n_studies = c(10),
    model_function_list = list(list(model_lmm_random_int_ml)),
    study_sample_size_train = c(50),
    study_sample_size_test = 500,
    ICC = c(0.3),
    R2 = c(0.7),
    intercept_est_sample_size = list(c(10, 50)),
    n_predictors = 12,
    n_studies_test = 5
  )

  sim_results_test <- do.call(ipdma_simulation, sim_params)
  var_u <- sim_results_test |> dplyr::filter(metric == 'var_u', predict_method == 'new0')
  expect_equal(var_u[1,2], var_u[2,2])



})
