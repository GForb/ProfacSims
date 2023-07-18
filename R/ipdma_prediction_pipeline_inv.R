sim_rep_continuous_inv <- function(model_function_list, n_studies, study_sample_size_train, study_sample_size_test, sigma_e, sigma_u, sim_rep) {
  .Random.seed <- sim_rep$rng_state[1]

  train_data <- generate_continuous(n_studies, study_sample_size_train, sigma_e, sigma_u)
  test_data <- generate_continuous(n_studies, study_sample_size_test, sigma_e, sigma_u, train_data = train_data)
  models_list <- list()
  for(model in model_function_list){
    new_model <- list(model(train_data))
    models_list <- c(models_list, new_model)
  }
  results <- sim_rep_inv(model_function_list, evaluate_performance = evaluate_performance_continuous, train_data = train_data, test_data = test_data)
  return(list(train_data = train_data, test_data = test_data, models = models_list, results = results))
}


sim_rep_inv <- function(model_function_list, evaluate_performance, train_data, test_data) {
  results_list <- lapply(
    model_function_list,
    ipdma_prediction_pipeline_inv,
    train_data = train_data,
    test_data = test_data,
    evaluate_performance = evaluate_performance
  )

  return(results_list)
}

ipdma_prediction_pipeline_inv <- function(fit_model, train_data, test_data, evaluate_performance) {
  model <- fit_model(train_data)
  by_study_performacne <- get_performance_by_study(test_data, model, evaluate_performance)
  metrics <- unique(by_study_performacne$metric)
  results_list <- lapply(metrics, meta_analyse_performance, by_study_performacne = by_study_performacne)
  results_df <- do.call(rbind, results_list)
  results_df$model <- attr(model, "name")
  return(list(df = results_df, by_study = by_study_performacne))
}
