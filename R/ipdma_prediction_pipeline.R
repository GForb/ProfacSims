
# IPDMA pipeline


# generic function for an ipdma prediciton modelling simulation

# or make not generic...

ipdma_simulation <- function(...) {
  params_list <- list(...)
  sim_params <- do.call(tidyr::expand_grid, params_list)

  results <- furrr::future_pmap(.l = sim_params, .f = do_simulation, .options = furrr::furrr_options(seed=TRUE), .progress = TRUE)

  results_df <- do.call(rbind, results)
  results_df <- results_df |>
    tidyr::unnest_wider(args) |>
    tidyr::unnest_wider(sigma) |>
    dplyr::rename(sigma_u = u, sigma_e = e) |>
    dplyr::mutate(error_var_u = case_when(metric == "var_u" ~ est - (sigma_u^2),
                                           TRUE ~ NA))



  class(results_df) <-  c("sim_results", class(results_df))
  return(results_df)
}


do_simulation <- function(nreps, sim_rep_fun, ...) {
  args <- list(...)
  if (!is.null(args$sigma)){
    ICC = args$sigma["ICC"]
    R2 = args$sigma["R2"]
    sigmas <- get_sigmas(
      sigma2_x = 1,
      beta = attr(generate_continuous, "beta"),
      n_predictors = attr(generate_continuous, "n_predictors"),
      ICC = ICC,
      R2 = R2)
     args$sigma <- sigmas
  }
  loop_fun <- function(rep_number, args){
    results <- do.call(sim_rep_fun, args)
    results |> dplyr::mutate(args = list(args), rep_number = rep_number)
  }
  results_list <- lapply(1:nreps, loop_fun, args = args)

  results_df <- do.call(rbind, results_list)
  if(exists("ICC")) results_df$ICC = ICC
  if(exists("R2")) results_df$R2 = R2

  return(results_df)
}

sim_rep_continuous <- function(model_function_list, n_studies, study_sample_size_train, study_sample_size_test, sigma) {
  seed <- .Random.seed
  sigma_e <- sigma$e
  sigma_u <- sigma$u
  train_data <- generate_continuous(n_studies, study_sample_size_train, sigma_e, sigma_u)
  test_data <- generate_continuous(n_studies, study_sample_size_test, sigma_e, sigma_u, train_data = train_data)
  results <- sim_rep(model_function_list, evaluate_performance = evaluate_performance_continuous, train_data = train_data, test_data = test_data)
  results <- results |>
    dplyr::mutate(
      rng_state = dplyr::case_when(dplyr::row_number() ==1 ~ list(.Random.seed),
                                     TRUE  ~ list(NA)))
  return(results)
}



sim_rep <- function(model_function_list, evaluate_performance, train_data, test_data) {
  results_list <- lapply(
    model_function_list,
    ipdma_prediction_pipeline,
    train_data = train_data,
    test_data = test_data,
    evaluate_performance = evaluate_performance
  )
  results_df <- do.call(rbind, results_list) |> tibble::tibble()

  return(results_df)
}

#' Pipeline for developing and validating a prediction model by study for an IPD Meta-analysis with training and test data.
#'
#' @param train_data data used to fit the model
#' @param test_data data used to validate the model
#' @param evaluate_performance a function that evaluates the performance of the model for a single study
#' @param fit_model a function that fits the model to be used
#'
#' @return
#' @export
#'
#' @examples
ipdma_prediction_pipeline <- function(fit_model, train_data, test_data, evaluate_performance) {
  model <- fit_model(train_data)
  by_study_performacne <- get_performance_by_study(test_data, model, evaluate_performance)
  metrics <- unique(by_study_performacne$metric)
  results_list <- lapply(metrics, meta_analyse_performance, by_study_performacne = by_study_performacne)
  results_df <- dplyr::bind_rows(results_list)

  var_u <- data.frame(metric = "var_u", est = get_var_u(model))
  results_df <- dplyr::bind_rows(results_df, var_u)

  results_df$model <- attr(model, "name")
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




