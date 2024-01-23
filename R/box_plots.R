
box_plot_error_var_u <- function(sim_results) {
  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Random intercetp - REML" ~ n_studies*2^-0.1,
                                                   model == "Random intercetp - ML" ~ n_studies*2^0.1)
    )

  facet_cols <-  ggplot2::vars(study_sample_size_train)
  if(!is.null(sim_results$int_pred_corr)){
    facet_cols <-  ggplot2::vars(int_pred_corr, study_sample_size_train)
  }
  facet_rows = ggplot2::vars(ICC)

  plot <- sim_results_mod |>  ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, group = n_studies_mod, color = model )) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Error in random intercept variance",
      caption ="Performance is pooled study level model performance \n
                 Tau-Squared is the between study variance in model performance"
    )

  return(plot)
}
