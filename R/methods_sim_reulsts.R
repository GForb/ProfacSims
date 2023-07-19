plot.sim_results <- function(sim_results) {
  sim_results$log_n_studies = log(sim_results$n_studies)

  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "lm" ~ n_studies*2^-0.21,
                                                   model == "lm_fixed_int" ~ n_studies*2^-0.07,
                                                   model == "lmm_random_int_reml" ~ n_studies*2^0.07,
                                                   model == "lmm_random_int_ml" ~ n_studies*2^0.21))
  sim_results_mod_est <- sim_results_mod |>
    dplyr::mutate(value = est, what = "Performance") |>
    dplyr::filter(metric!="var_u")
  sim_results_mod_tau2 <- sim_results_mod |>
    dplyr::mutate(value = tau2, what = "Tau-Squared") |>
    dplyr::filter(metric!="var_u")
  sim_results_mod_error_var_u <- sim_results_mod |>
    dplyr::mutate(value = error_var_u, what = "Error Var_u") |>
    dplyr::filter(metric=="var_u")
  sim_results_stacked <- dplyr::bind_rows(sim_results_mod_est, sim_results_mod_tau2, sim_results_mod_error_var_u)
  sim_results_stacked |>
    ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(ICC,study_sample_size_train), rows = ggplot2::vars(metric, what), scales = "free_y", switch = "y") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "",
      caption ="Performance is pooled study level model performance \n
                 Tau-Squared is the between study variance in model performance"
      )
}
