sim_rep_continuous_model_only <- function(model_function_list, n_studies, study_sample_size_train, study_sample_size_test, sigmas, predictor_intercepts="study", n_predictors = 12) {
  seed <- .Random.seed
  train_data <- generate_continuous(
    n_studies = n_studies,
    study_sample_size = study_sample_size_train,
    sigmas = sigmas,
    predictor_intercepts = predictor_intercepts,
    n_predictors = n_predictors)

  results_list <- lapply(
    model_function_list,
    model_evaluate_pipeline_no_predict,
    train_data = train_data
  )
  results_df <- do.call(rbind, results_list) |>
    tibble::tibble() |>
    dplyr::mutate(rng_state = list(.Random.seed))

  return(results_df)
}


model_evaluate_pipeline_no_predict <- function(fit_model, train_data) {
  model <- fit_model(train_data)
  betas <- get_betas(model)
  results_df <- tibble::tibble(metric = "var_u", est = get_var_u(model), betas = list(betas))

  results_df$model <- attr(model, "name")
  return(results_df)
}
