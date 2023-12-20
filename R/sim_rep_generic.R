sim_rep_continuous_all_eval <- function(
    model_function_list,
    n_studies,
    study_sample_size_train,
    study_sample_size_test,
    sigmas,
    intercept_est_sample_size,
    n_studies_test,
    n_predictors,
    predictor_intercepts,
    dynamic) {

  seed <- .Random.seed

  train_data <- generate_continuous(
    n_studies = n_studies,
    study_sample_size = study_sample_size_train,
    sigmas = sigmas,
    n_predictors = n_predictors,
    predictor_intercepts= predictor_intercepts)

  models_list <- lapply(model_function_list, function(model_function) do.call, train_data)

    test_data_existing  <- generate_continuous(
      n_studies = n_studies,
      study_sample_size = study_sample_size_test,
      sigmas = sigmas,
      n_predictors = n_predictors,
      predictor_intercepts= predictor_intercepts,
      intercepts_data = train_data)

    results <- sim_rep_existing(models_list, evaluate_performance = evaluate_performance_continuous, train_data = train_data, test_data = list(test_data))


    for(x in intercept_est_sample_size){
      test_data <-     generate_continuous_new_studies(
        n_studies = n_studies_test,
        study_sample_size = study_sample_size_test,
        sigmas= sigmas,
        intercept_est_sample_size = x,
        intercepts_data = NULL,
        min_study_id = 11,
        n_predictors = n_predictors,
        predictor_intercepts = predictor_intercepts
      )

      results_new <-  sim_rep_new(models_list, evaluate_performance = evaluate_performance_continuous, train_data = train_data, test_data = list(test_data))
      if (dynamic) {
        results_dynamic <- sim_rep_dynamic(models_list, train_data, test_data)
      }

      results <- bind_rows(results, results_new, results_dynamic)
    }
  results <- results |>
    dplyr::mutate(
      rng_state = dplyr::case_when(dplyr::row_number() ==1 ~ list(.Random.seed),
                                   TRUE  ~ list(NA)))
  return(results)
}


sim_rep_continuous <- function(model_function_list, n_studies, study_sample_size_train, study_sample_size_test, sigmas) {
  seed <- .Random.seed
  train_data <- generate_continuous(n_studies = n_studies, study_sample_size = study_sample_size_train, sigmas = sigmas)
  test_data <- generate_continuous(n_studies = n_studies, study_sample_size = study_sample_size_test, sigmas = sigmas, intercepts_data = train_data)
  results <- sim_rep(model_function_list, evaluate_performance = evaluate_performance_continuous, train_data = train_data, test_data = list(test_data))
  results <- results |>
    dplyr::mutate(
      rng_state = dplyr::case_when(dplyr::row_number() ==1 ~ list(.Random.seed),
                                   TRUE  ~ list(NA)))
  return(results)
}

sim_rep_continuous_new_test_studies  <- function(model_function_list, n_studies, study_sample_size_train, study_sample_size_test, sigmas,  intercept_est_sample_size, n_studies_test = n_studies, n_predictors = 12, predictor_intercepts = "study") {
  train_data <- generate_continuous(
    n_studies = n_studies,
    study_sample_size = study_sample_size_train,
    sigmas = sigmas,
    n_predictors = n_predictors,
    predictor_intercepts= predictor_intercepts)

  generate_test_data <- function(x) {

    generate_continuous_new_studies(
      n_studies = n_studies_test,
      study_sample_size = study_sample_size_test,
      sigmas= sigmas,
      intercept_est_sample_size = x,
      intercepts_data = NULL,
      min_study_id = 11,
      n_predictors = n_predictors,
      predictor_intercepts = predictor_intercepts
    )
  }



  test_data_list = lapply(intercept_est_sample_size, generate_test_data)
  results <- sim_rep(model_function_list,
                     evaluate_performance = evaluate_performance_continuous_new_studies,
                     train_data = train_data, test_data = test_data_list)

  results <- results |>
    dplyr::mutate(
      rng_state = dplyr::case_when(dplyr::row_number() ==1 ~ list(.Random.seed),
                                   TRUE  ~ list(NA)))
  return(results)


}
