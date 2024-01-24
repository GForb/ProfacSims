
box_plot_error_var_u <- function(sim_results, outliers = TRUE) {
  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Random intercetp - REML" ~ n_studies*2^-0.1,
                                                   model == "Random intercetp - ML" ~ n_studies*2^0.1)
    ) |> dplyr::filter(study_sample_size_train %in% c(50, 1000))

  facet_cols <-  ggplot2::vars(R2, study_sample_size_train)
  if(!is.null(sim_results$int_pred_corr)){
    facet_cols <-  ggplot2::vars(int_pred_corr, R2, study_sample_size_train)
  }
  facet_rows = ggplot2::vars(ICC)
  outlier_shape = NULL
  if(outliers ==FALSE) outlier_shape = NA
  plot <- sim_results_mod |>  ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = error_var_u, group = n_studies_mod, color = model )) +
    ggplot2::geom_boxplot(outlier.size = 0.1, outlier.shape = outlier_shape) +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Error in random intercept variance",
    )

  return(plot)
}

box_plot_est_by_model <- function(data, outliers = TRUE) {
  model_offset <- 0.14
  data <- data |> add_n_studies_mod(model_offset)

  facet_cols <-  ggplot2::vars(R2, study_sample_size_train)
  facet_rows = ggplot2::vars(ICC)
  outlier_shape = NULL
  if(outliers ==FALSE) outlier_shape = NA

  plot <- data |>  ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = est, group = n_studies_mod, color = model )) +
    ggplot2::geom_boxplot(outlier.size = 0.1, outlier.shape = outlier_shape) +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Calibration in the large",
    )

  return(plot)
}

box_plot_tau2_by_model <- function(data, outliers = TRUE) {
  model_offset <- 0.14
  data <- data |> add_n_studies_mod(model_offset)

  facet_cols <-  ggplot2::vars(R2, study_sample_size_train)
  facet_rows = ggplot2::vars(ICC)
  outlier_shape = NULL
  if(outliers ==FALSE) outlier_shape = NA

  plot <- data |>  ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = tau2, group = n_studies_mod, color = model )) +
    ggplot2::geom_boxplot(outlier.size = 0.1, outlier.shape = outlier_shape) +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Tau-squared",
    )

  return(plot)
}

