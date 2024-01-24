

model_evaluate_pipeline_dynamic <- function(fit_model, train_data, test_data_list, evaluate_performance) {
  results_list <- lapply(test_data_list, ipdma_prediction_pipeline_dynamic, model_function = fit_model, evaluate_performance = evaluate_performance, train_data = train_data)
  results_df <- dplyr::bind_rows(results_list)

  return(results_df)
}


#' Pipeline for developing and validating a prediction model by study for an IPD Meta-analysis with training and test data.
#'
#' @param train_data data used to fit the model
#' @param test_data data used to validate the model
#' @param evaluate_performance a function that evaluates the performance of the model for a single study
#' @param model a fitted model
#'
#' @return
#' @export
#'
#' @examples
ipdma_prediction_pipeline_dynamic <- function(test_data, model_function, train_data,  evaluate_performance) {
  by_study_performacne <- get_performance_by_study_dynamic(test_data, model_function,train_data, evaluate_performance)
  var_u <- by_study_performacne |> dplyr::filter(metric == "var_u")
  model_text  <-  by_study_performacne$model[[1]]
  by_study_performacne <- by_study_performacne |> dplyr::filter(metric != "var_u")
  metrics <- unique(by_study_performacne$metric)
  results_list <- lapply(metrics, meta_analyse_performance, by_study_performacne = by_study_performacne)

  results_df <- dplyr::bind_rows(results_list, var_u)
  results_df$test_ss=sum(test_data$int_est)
  results_df$model <- model_text
  return(results_df)
}

get_performance_by_study_dynamic <- function(test_data, model_function, train_data, evaluate_performance) {
  if (!("studyid" %in% colnames(test_data))) {
    stop("test data must include varaible called studyid")
  }

  performance_by_study_dynamic <- function(study) {
    int_est_data <- test_data |> dplyr::filter(studyid == study, int_est)
    train_data_new <- dplyr::bind_rows(train_data, int_est_data)
    model <- model_function(train_data_new)
    test_data_study <- test_data |> dplyr::filter(!int_est, studyid == study)
    results <- evaluate_performance(test_data_study, model)
    results$studyid <- study

    betas <- get_betas(model)
    model_est <- tibble::tibble(metric = "var_u", est = get_var_u(model), betas = list(betas), studyid = study)
    results <- dplyr::bind_rows(results, model_est)
    results$model <- attr(model, "name")
    return(results)
  }
  studies <- unique(test_data$studyid)
  results_list <- lapply(studies, performance_by_study_dynamic)
  results <- do.call(rbind, results_list)

  return(results)
}






