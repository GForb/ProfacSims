plot.sim_results <- function(sim_results) {
  sim_results$log_n_studies = log(sim_results$n_studies)

  sim_results_mod <- sim_results |>
    dplyr::mutate(log_n_studies_mod = dplyr::case_when(model == "lm" ~ log_n_studies -0.21,
                                                   model == "lm_fixed_int" ~ log_n_studies-0.07,
                                                   model == "lmm_random_int_reml" ~ log_n_studies+0.07,
                                                   model == "lmm_random_int_ml" ~ log_n_studies+0.21))
  sim_results_mod_est <- sim_results_mod |> dplyr::mutate(value = est, what = "Performance")
  sim_results_mod_tau2 <- sim_results_mod |>dplyr::mutate(value = tau2, what = "Tau-Squared")
  sim_results_mod_error_var_u <- sim_results_mod |>dplyr::mutate(value = error_var_u, what = "Error Var_u")
  sim_results_stacked <- dplyr::bind_rows(sim_results_mod_est, sim_results_mod_tau2)
  sim_results_stacked |>
    ggplot2::ggplot(ggplot2::aes(x = log_n_studies_mod, y = value, color = model)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(ICC,study_sample_size_train), rows = ggplot2::vars(metric, what), scales = "free_y", switch = "y") +
    ggplot2::labs(
      xlab = "Number of studies",
      ylab = "",
      caption ="Performance is the average of model performance in each study \n
                 Tau-Squared is the between study variance in model performance"
      )
}
