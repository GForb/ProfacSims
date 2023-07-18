plot.sim_results <- function(sim_results) {
  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "lm" ~ n_studies -1,
                                     model == "lm_fixed_int" ~ n_studies-0.5,
                                     model == "lmm_random_int_reml" ~ n_studies+0.5,
                                     model == "lmm_random_int_ml" ~ n_studies+1))


  sim_results_mod_est <- sim_results_mod |> dplyr::mutate(value = est, what = "Performance")
  sim_results_mod_tau2 <- sim_results_mod |>dplyr::mutate(value = tau2, what = "Tau-Squared")
  sim_results_stacked <- dplyr::bind_rows(sim_results_mod_est, sim_results_mod_tau2)
  sim_results_stacked |>
    ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(ICC,study_sample_size_train), rows = ggplot2::vars(metric, what), scales = "free_y", switch = "y") +
    ggplot2::labs(
      xlab = "Number of studies",
      ylab = "",
      caption ="Performance is the average of model performance in each study \n
                 Tau-Squared is the between study variance in model performance"
      )
}
