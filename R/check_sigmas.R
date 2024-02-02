
check_simulation_pipeline <- function(..., expand_intercept_est_sample_size = FALSE, use_furrr = FALSE) {
  params_list <- list(...)
  sim_params <- do.call(tidyr::expand_grid, params_list)

  if(use_furrr){
    results <- furrr::future_pmap(.l = sim_params, .f = check_simulation, .options = furrr::furrr_options(seed=TRUE), .progress = TRUE)
  } else {
    results <- purrr::pmap(.l = sim_params, .f = check_simulation, .progress = TRUE)
  }


  return(dplyr::bind_rows(results) |> tibble::tibble())
}


check_simulation <- function(nreps, sim_rep_fun, ...) {
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

  if(!is.null(args$single_x)){
    single_x <-  args$single_x
  } else {
    single_x <- FALSE
    args$single_x <- FALSE
  }


  b_w_ratio <-  args$b_w_ratio




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
    pred_icc = pred_icc,
    b_w_ratio = b_w_ratio,
    single_x = single_x)
  args$sigmas <- sigmas
  args <- args[names(args) %in% c("ICC", "R2", "int_pred_corr", "pred_icc", "single_x", "b_w_ratio")==FALSE]

  results_df <-  sigmas |> as.data.frame()
  results_df$ICC = ICC
  results_df$R2 = R2
  results_df$int_pred_corr = int_pred_corr
  results_df$pred_icc = pred_icc
  if(!is.null(b_w_ratio)) results_df$b_w_ratio = b_w_ratio

  return(results_df)
}
