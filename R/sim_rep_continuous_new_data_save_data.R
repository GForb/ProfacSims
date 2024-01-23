sim_rep_continuous_new_data_save_data <- function(
    model_function_list,
    n_studies,
    study_sample_size_train,
    study_sample_size_test,
    sigmas,
    intercept_est_sample_size,
    n_studies_test,
    n_predictors,
    predictor_intercepts = "study"
) {
  seed <- .Random.seed

  train_data <- generate_continuous(
    n_studies = n_studies,
    study_sample_size = study_sample_size_train,
    sigmas = sigmas,
    n_predictors = n_predictors,
    predictor_intercepts= predictor_intercepts)

  models_list <-  lapply(model_function_list, function(model_function, data) model_function(data), data = train_data)


  # Approaches that involve estimating intercepts from new data: 1. Predict new intercepts only, 2. Refit entire model (dynamic)
  results_list <- list()
  for(x in intercept_est_sample_size){

    test_data <-  generate_continuous_new_studies(
      n_studies = n_studies_test,
      study_sample_size = study_sample_size_test,
      sigmas= sigmas,
      intercept_est_sample_size = x,
      intercepts_data = NULL,
      min_study_id = n_studies+1,
      n_predictors = n_predictors,
      predictor_intercepts = predictor_intercepts
    )
    results_new0 <-  sim_rep_new_data0(models_list, test_data = test_data)

    # Method  3. Estimating new intercept using empirical bayes or mean for fixed effect models
    results_new <-  sim_rep_new_data(models_list, test_data = list(test_data))

    # Method 4. Taking a dynamic approach to intercept estiamtion - re-estimating the model then using the re-estiamted model to make predictions.
    results_dynamic <- sim_rep_dynamic(model_function_list, train_data, test_data)

    results_list <- c(results_list, list(dplyr::bind_rows(results_new0, results_new, results_dynamic)))
  }

  results <- results_list |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      rng_state = dplyr::case_when(dplyr::row_number() ==1 ~ list(.Random.seed),
                                   TRUE  ~ list(NA)),
      train_data = list(train_data))
  return(results)

}
