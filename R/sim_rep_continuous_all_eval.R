sim_rep_continuous_all_eval <- function(
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

  # Test data for predictions made on data from studies used in development
  test_data_existing  <- generate_continuous(
    n_studies = n_studies,
    study_sample_size = study_sample_size_test,
    sigmas = sigmas,
    n_predictors = n_predictors,
    predictor_intercepts = predictor_intercepts,
    intercepts_data = train_data)

  results <- sim_rep_existing(models_list, test_data = test_data_existing)

  test_data <-  generate_continuous_new_studies(
    n_studies = n_studies_test,
    study_sample_size = study_sample_size_test,
    sigmas= sigmas,
    intercept_est_sample_size = 0,
    intercepts_data = NULL,
    min_study_id = n_studies+1,
    n_predictors = n_predictors,
    predictor_intercepts = predictor_intercepts
  )
  # Method  2. Assuming zero (or average intercept)
  results_new0 <-  sim_rep_new_data0(models_list, test_data = test_data)
  results <- dplyr::bind_rows(results, results_new0)
  # Approaches that involve estimating intercepts from new data: 1. Predict new intercepts only, 2. Refit entire model (dynamic)
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

    # Method  3. Estimating new intercept using empirical bayes or mean for fixed effect models
    results_new <-  sim_rep_new_data(models_list, test_data = list(test_data))

    # Method 4. Taking a dynamic approach to intercept estiamtion - re-estimating the model then using the re-estiamted model to make predictions.
    results_dynamic <- sim_rep_dynamic(model_function_list, train_data, test_data)

    results_wt <- sim_rep_weights(model_function_list, train_data, test_data)

    results <- dplyr::bind_rows(results, results_new, results_dynamic, results_wt)
  }
  results <- results |>
    dplyr::mutate(
      rng_state = dplyr::case_when(dplyr::row_number() ==1 ~ list(.Random.seed),
                                   TRUE  ~ list(NA)))
  return(results)
}
