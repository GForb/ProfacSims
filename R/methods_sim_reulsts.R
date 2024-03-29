plot_results_by_model <- function(data, model_offset = 0.14) {
  if(!"pred_icc" %in% names(data)){
    data$pred_icc = 0
  }
  data <- data |> add_n_studies_mod(model_offset)
  data <- data |> rename(study_n = study_sample_size_train)

  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(R2, study_n),
                        rows = ggplot2::vars(pred_icc, ICC),
                        scales = "free_y",
                        switch = "y",
                        labeller= "label_both") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      color = "Model:",
      x = "Number of studies (log scale)",
      y = ""
    ) +
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "top")

  return(plot)
}

plot_results_by_pred <- function(data, CI = FALSE, model_offset = 0.14) {
  data <- data |> add_n_studies_mod(model_offset)
  data <- data |> rename(study_n = study_sample_size_train)
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model, fill = intercept_est_sample_size)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(R2, study_n),
                        rows = ggplot2::vars(int_pred_corr, ICC),
                        scales = "free_y",
                        switch = "y",
                        labeller= "label_both") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      color = "Model:",
      x = "Number of studies (log scale)",
      y = "",
      caption ="Performance is pooled study level model performance \n
                 Tau-Squared is the between study variance in model performance"
    ) +
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "top")
  if(CI){
    plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymin=ll, ymax=ul))
  }
  return(plot)
}



add_n_studies_mod <- function(data, model_offset) {
  data |> dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Not adjusting for study" ~ n_studies*2^(-model_offset*3/2),
                                                 model == "Fixed intercept" ~ n_studies*2^(-model_offset*1/2),
                                                 model == "Random intercetp - REML" ~ n_studies*2^(model_offset*1/2),
                                                 model == "Random intercetp - ML" ~ n_studies*2^(model_offset*3/2)))
}





plot.sim_results <- function(sim_results, stack = TRUE, CI = TRUE, what = NULL) {
  plot_data <- sim_results |>
    prep_data_for_plot() |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Not adjusting for study" ~ n_studies*2^-0.21,
                                                   model == "Fixed intercept" ~ n_studies*2^-0.07,
                                                   model == "Random intercetp - REML" ~ n_studies*2^0.07,
                                                   model == "Random intercetp - ML" ~ n_studies*2^0.21))
  if(!is.null(what)){
    what_filter <- what
    plot_data <- plot_data |> filter(
      what == what_filter
    )
  }

  plot <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = mean, color = model, shape = int_est_n_factor)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(R2, study_sample_size_train),
                        rows = ggplot2::vars(what, ICC),
                        labeller = ggplot2::label_both) +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "",
      caption ="Performance is pooled study level model performance \n
                 Tau-Squared is the between study variance in model performance"
    ) +
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "top")
  if(CI){
    plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymin=ll, ymax=ul))
  }
  return(plot)
}

plot_error_var_u <- function(sim_results, CI = TRUE) {
  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Random intercetp - REML" ~ n_studies*2^-0.1,
                                                   model == "Random intercetp - ML" ~ n_studies*2^0.1)
    )

  facet_cols <-  ggplot2::vars(study_sample_size_train)
  if(!is.null(sim_results$int_pred_corr)){
    facet_cols <-  ggplot2::vars(int_pred_corr, study_sample_size_train)
  }
  facet_rows = ggplot2::vars(ICC)

  if(is.null(sim_results_mod$predict_method)){
    plot <- sim_results_mod |>
      ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model))
  } else {
    plot <- sim_results_mod |>
      ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model, shape = predict_method))
  }


  plot <- plot +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Error in random intercept variance",
      caption ="Performance is pooled study level model performance \n
                 Tau-Squared is the between study variance in model performance"
    )
  if(CI){
    plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymin=ll, ymax=ul))
  }
  return(plot)
}


prep_data_for_plot <- function(data) {
  sim_results_mod <- data |>
    dplyr::mutate(
      int_est_n = test_ss/n_studies_test,
      int_est_n_factor = factor(int_est_n, exclude = NULL),
      model_est_method = paste(model, ", Est. n:", int_est_n)
    ) |>
    dplyr::mutate(model_est_method=factor(model_est_method),
    ) |>
    sim_results_lazy_stack() |>
    get_summaries() |>
    dplyr::ungroup()
  return(sim_results_mod)

}


sim_results_lazy_stack <- function(sim_results) {
  sim_results_mod_est <- sim_results |>
    dplyr::mutate(value = .data$est, what = "Performance") |>
    dplyr::filter(metric!="var_u")
  sim_results_mod_tau2 <- sim_results |>
    dplyr::mutate(value = sqrt(.data$tau2), what = "Tau") |>
    dplyr::filter(metric!="var_u")

  sim_results_stacked <- dplyr::bind_rows(sim_results_mod_est, sim_results_mod_tau2)
  return(sim_results_stacked)
}







robust_t_test <- function(x) {
  CI <- c(NA, NA)
  try(CI <- t.test(x)$conf.int, silent = TRUE)
  return(CI)
}

get_summaries <- function(sim_results_stacked) {


  check <- try(sim_results_stacked |> dplyr::select(int_pred_corr), silent = TRUE)
  if (attr(check, "class")[1]=="try-error") {
    sim_results_stacked <- sim_results_stacked |> mutate(int_pred_corr = 0)
    print("int_pred_corr added")
  }


  summaries <- sim_results_stacked |>
    dplyr::select(metric, what, value, ICC, R2, study_sample_size_train, n_studies, model, sigma_u, sigma_e, int_est_n, int_pred_corr, int_est_n_factor, model_est_method, test_ss) |>
    dplyr::group_by(across(c(-value))) |>
    dplyr::summarise(
      mean = mean(value),
      ll = robust_t_test(value)[1],
      ul = robust_t_test(value)[2],
      n = sum(!is.na(value)),
      sigma_u = mean(sigma_u),
      sigma_e = mean(sigma_e))
  return(summaries)
}

