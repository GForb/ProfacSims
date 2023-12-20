test_that("clean_betas", {
  sim_params <- list(
    nreps = 1,
    sim_rep_fun = list(ProfacSims:::sim_rep_continuous_model_only),
    n_studies = c(8),
    model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                    ProfacSims:::model_lm,
                                    ProfacSims:::model_lmm_random_int_reml,
                                    ProfacSims:::model_lmm_random_int_ml)),
    study_sample_size_train = c(200),
    ICC = c(0.3),
    R2 = c(0.7),
    pred_icc = 0,
    predictor_intercepts = "random",
    n_predictors = 1
  )
  set.seed(1234)

  sim_results <- results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
  cleaned_data <- sim_results |> clean_betas()
  cleaned_data$betas

  fixt_with_int <- cleaned_data |> dplyr::filter(model == "Fixed intercept")
  fixed_no_study_int <- cleaned_data |> dplyr::filter(model == "Not adjusting for study")
  mixed_model <- cleaned_data |> dplyr::filter(model == "Random intercetp - REML")

  expect_true("x1" %in% names( fixt_with_int$betas[[1]]))
  expect_true("x1" %in% names( fixed_no_study_int$betas[[1]]))
  expect_true("x1" %in% names( mixed_model$betas[[1]]))

})


test_that("process_betas", {
  sim_params <- list(
    nreps = 1,
    sim_rep_fun = list(ProfacSims:::sim_rep_continuous_model_only),
    n_studies = c(8),
    model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                    ProfacSims:::model_lm,
                                    ProfacSims:::model_lmm_random_int_reml,
                                    ProfacSims:::model_lmm_random_int_ml)),
    study_sample_size_train = c(200),
    ICC = c(0.3),
    R2 = c(0.7),
    pred_icc = 0,
    predictor_intercepts = "random",
    n_predictors = 1
  )
  set.seed(1234)

  sim_results <- results <- do.call(ProfacSims:::ipdma_simulation, sim_params)

  porcessed_data <- process_betas(sim_results)
  expect_false(anyNA(porcessed_data$beta_mse))
})

