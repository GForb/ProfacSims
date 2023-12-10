plot.sim_results_old <- function(sim_results, stack = TRUE, CI = FALSE) {

  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "lm" ~ n_studies*2^-0.21,
                                                   model == "lm_fixed_int" ~ n_studies*2^-0.07,
                                                   model == "lmm_random_int_reml" ~ n_studies*2^0.07,
                                                   model == "lmm_random_int_ml" ~ n_studies*2^0.21),
                  int_est_n = test_ss/n_studies_test,
                  int_est_n_factor = factor(int_est_n, exclude = NULL),
                  model_est_method = paste(model, ", Est. n:", int_est_n)) |>
    mutate(model_est_method=factor(model_est_method),
    )

  if(stack){
    sim_results_stacked <- sim_results_lazy_stack(sim_results_mod)
  } else {
    sim_results_stacked <- sim_results_mod
  }
  print(sim_results_mod)
  plot <- sim_results_stacked |>
    ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(ICC, study_sample_size_train), rows = ggplot2::vars(metric, what), scales = "free_y", switch = "y") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "",
      caption ="Performance is pooled study level model performance \n
                 Tau-Squared is the between study variance in model performance"
    )
  if(CI){
    plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymin=ll, ymax=ul))
  }
  return(plot)
}
