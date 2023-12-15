
# IPDMA pipeline


# generic function for an ipdma prediciton modelling simulation


ipdma_simulation <- function(...) {
  params_list <- list(...)
  sim_params <- do.call(tidyr::expand_grid, params_list)
  results <- furrr::future_pmap(.l = sim_params, .f = do_simulation, .options = furrr::furrr_options(seed=TRUE), .progress = TRUE)

  results_df <- do.call(rbind, results)
  results_df <- results_df |>
    tidyr::unnest_wider(args) |>
    tidyr::unnest_wider(sigmas) |>
    dplyr::rename(sigma_u = u, sigma_e = e) |>
    dplyr::mutate(error_var_u = dplyr::case_when(metric == "var_u" ~ est - (sigma_u^2),
                                                 TRUE ~ NA))



  class(results_df) <-  c("sim_results", class(results_df))
  return(results_df)
}


do_simulation <- function(nreps, sim_rep_fun, ...) {
  args <- list(...)
  ICC <-  args$ICC
  R2 <- args$R2


  if(!is.null(args$int_pred_corr)){
    int_pred_corr <- args$int_pred_corr
  } else {
    int_pred_corr <-  0
    args$int_pred_corr <- 0
  }

  if(!is.null(args$pred_icc)){
    pred_icc <-  args$pred_icc
  } else {
    pred_icc <- 0
    args$pred_icc <- 0
  }

  if(!is.null(args$n_predictors)){
    n_predictors <- args$n_predictors
  } else {
    n_predictors = attr(generate_continuous, "n_predictors")
  }

    sigmas <- get_sigmas(
      n_predictors = n_predictors,
      ICC = ICC,
      R2 = R2,
      int_pred_corr = int_pred_corr,
      pred_icc = pred_icc)
    args$sigmas <- sigmas
    args <- args[names(args) %in% c("ICC", "R2", "int_pred_corr", "pred_icc")==FALSE]


  loop_fun <- function(rep_number, args){
    results <- do.call(sim_rep_fun, args)
    results |> dplyr::mutate(args = list(args), rep_number = rep_number)
  }
  results_list <- lapply(1:nreps, loop_fun, args = args)

  results_df <- do.call(rbind, results_list)
  results_df$ICC = ICC
  results_df$R2 = R2
  results_df$int_pred_corr = int_pred_corr
  results_df$pred_icc = pred_icc

  return(results_df)
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
  train_data <- generate_continuous(n_studies = n_studies, study_sample_size = study_sample_size_train, sigmas = sigmas)

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


sim_rep <- function(model_function_list, evaluate_performance, train_data, test_data) {
  results_list <- lapply(
    model_function_list,
    model_evaluate_pipeline,
    train_data = train_data,
    test_data_list = test_data,
    evaluate_performance = evaluate_performance
  )
  results_df <- do.call(rbind, results_list) |> tibble::tibble()

  return(results_df)
}



model_evaluate_pipeline <- function(fit_model, train_data, test_data_list, evaluate_performance) {
  model <- fit_model(train_data)
  results_list <- lapply(test_data_list, ipdma_prediction_pipeline, model = model, evaluate_performance = evaluate_performance)
  results_df <- dplyr::bind_rows(results_list)
  betas <- get_betas(model)
  model_est <- tibble::tibble(metric = "var_u", est = get_var_u(model), betas = list(betas))
  results_df <- dplyr::bind_rows(results_df, model_est)

  results_df$model <- attr(model, "name")
  return(results_df)
}
