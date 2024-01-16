
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
ipdma_prediction_pipeline <- function(test_data, model, evaluate_performance) {
  by_study_performacne <- get_performance_by_study(test_data, model, evaluate_performance)
  metrics <- unique(by_study_performacne$metric)
  results_list <- lapply(metrics, meta_analyse_performance, by_study_performacne = by_study_performacne)
  results_df <- dplyr::bind_rows(results_list)
  results_df$test_ss=sum(test_data$int_est)
  return(results_df)
}

get_performance_by_study <- function(test_data, model, evaluate_performance) {
  if (!("studyid" %in% colnames(test_data))) {
    stop("test data must include varaible called studyid")
  }

  performance_by_study <- function(study) {
    study_data <- test_data[test_data$studyid == study,]
    results <- evaluate_performance(study_data, model)
    results$studyid <- study
    return(results)
  }
  studies <- unique(test_data$studyid)
  results_list <- lapply(studies, performance_by_study)
  results <- do.call(rbind, results_list)
  rownames(results) <- NULL
  return(results)
}

meta_analyse_performance <- function(metric, by_study_performacne) {
  results_df <- data.frame(metric,
             est = NA,
             se = NA,
             tau2 = NA,
             ci.lb = NA,
             ci.ub = NA,
             pi.lb = NA,
             pi.ub = NA)
  try({
    data <- by_study_performacne[by_study_performacne$metric == metric,]
    coefs <- data$coef
    ses <- data$se

    if(metric == "cstat"){
        results <- metamisc::valmeta(cstat = coefs, cstat.se = ses)
        results_df <- data.frame(metric,
                   est = results$est,
                   se = results$fit$se,
                   tau2 = results$fit$tau2,
                   ci.lb = results$ci.lb,
                   ci.ub = results$ci.ub,
                   pi.lb = results$pi.lb,
                   pi.ub = results$pi.ub)
    } else {
        results <- metafor::rma(yi = coefs, sei = ses, method = "REML", test = "knha")
        results_vector <- predict(results)
        results_df <- data.frame(metric,
                   est = results_vector$pred,
                   se = results_vector$se,
                   tau2 = results$tau2,
                   ci.lb = results_vector$ci.lb,
                   ci.ub = results_vector$ci.ub,
                   pi.lb = results_vector$pi.lb,
                   pi.ub = results_vector$pi.ub)
    }
  }, silent = FALSE)

  return(results_df)
}




