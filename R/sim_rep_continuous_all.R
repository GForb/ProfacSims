# A function for running a single simualtion replication including predictions using the following methods:
#   1. Setting intercept to zero (mean)
#   2. Using development studies for prediction
#   3. Estimating new intercept using empirical bayes or mean for fixed effect models
#   4. Taking a dynamic approach to intercept estiamtion - re-estimating the model then using the re-estiamted model to make predictions.
#   5. perhaps add: Using a model estimted with weights and then making a prediciton on the whole data.




# sim rep using methods that work when there is new data to estimate intercepts only.
sim_rep_continuous_new_data <- function(
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
    results_new0$intercept_est_sample_size2 = x
    print(results_new0 |> dplyr::select(metric, est, predict_method, intercept_est_sample_size2))

    # Method  3. Estimating new intercept using empirical bayes or mean for fixed effect models
    results_new <-  sim_rep_new_data(models_list, test_data = list(test_data))
    results_new$intercept_est_sample_size2 = x
    print(results_new |> dplyr::select(metric, est, predict_method, intercept_est_sample_size2))

    # Method 4. Taking a dynamic approach to intercept estiamtion - re-estimating the model then using the re-estiamted model to make predictions.
    results_dynamic <- sim_rep_dynamic(model_function_list, train_data, test_data)
    results_dynamic$intercept_est_sample_size2 = x
    print(results_dynamic |> dplyr::select(metric, est, predict_method, intercept_est_sample_size2))
    results_list <- c(results_list, list(dplyr::bind_rows(results_new0, results_new, results_dynamic)))
  }

  results <- results_list |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      rng_state = dplyr::case_when(dplyr::row_number() ==1 ~ list(.Random.seed),
                                   TRUE  ~ list(NA)))
  return(results)

}

sim_rep_existing <- function(models_list, test_data) {
  results <- sim_rep_fitted_model(fitted_model_list =  models_list, evaluate_performance = evaluate_performance_continuous, test_data = test_data)
  results$predict_method = "existing_studies"
  return(results)
}

sim_rep_new_data0 <- function(models_list, test_data) {
  results <- sim_rep_fitted_model(models_list, evaluate_performance = evaluate_performance_continuous_new_studies0, test_data = test_data)
  # I think this is going to set intercepts to zero rather than average for fixed models. Check.
  results$predict_method = "new0"
  return(results)
}

sim_rep_new_data <- function(models_list, test_data) {
  results <- sim_rep_fitted_model(models_list, evaluate_performance = evaluate_performance_continuous_new_studies, test_data = test_data)
  results$predict_method = "new_studies"
  return(results)
}

sim_rep_dynamic <- function(model_function_list, train_data, test_data) {
  int_est_data <-  test_data |> dplyr::filter(int_est == TRUE)
  train_data <- dplyr::bind_rows(train_data, int_est_data)
  test_data <- test_data |> dplyr::filter(int_est == FALSE)
  results <- sim_rep(model_function_list = model_function_list,
          evaluate_performance = evaluate_performance_continuous,
          train_data = train_data,
          test_data = test_data)
  results$predict_method = "new_dynamic"
  results$test_ss = nrow(int_est_data)
  return(results)
}

sim_rep_weights <- function(model_function_list, train_data, test_data) {
  train_data |> dplyr::mutate(wt = 1)
  int_est_data <-  test_data |> dplyr::filter(int_est == TRUE) |> dplyr::mutate(wt = 0.0000001)
  train_data <- dplyr::bind_rows(train_data, int_est_data)
  test_data <- test_data |> dplyr::filter(int_est == FALSE)
  results <- sim_rep(model_function_list = model_function_list,
          evaluate_performance = evaluate_performance_continuous,
          train_data = train_data,
          test_data = test_data)
  results$predict_method = "new_studies_wt"
  return(results)

  # this may not work depending on how predict for lmmer works - if it predicts even when data is new as long as there is the right study ids I'll be fine.
  # I need to run through the predict methods and make sure they are doing what I want.
}

