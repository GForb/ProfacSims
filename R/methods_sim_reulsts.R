plot_results_by_model <- function(data, CI = FALSE, model_offset = 0.14) {
  data <- data |> add_n_studies_mod(model_offset)
  data <- data |> rename(study_n = study_sample_size_train)
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(R2, study_n),
                        rows = ggplot2::vars(int_pred_corr, ICC),
                        scales = "free_y",
                        switch = "y",
                        labeller= "label_both") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "",
      caption ="Performance is pooled study level model performance \n
                 Tau-Squared is the between study variance in model performance"
    ) +
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
                                                 model == "Random intercetp - ML" ~ n_studies*2^(-model_offset*3/2)))
}



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

plot.sim_results <- function(sim_results, stack = TRUE, CI = FALSE) {

  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Not adjusting for study" ~ n_studies*2^-0.21,
                                                   model == "Fixed intercept" ~ n_studies*2^-0.07,
                                                   model == "Random intercetp - REML" ~ n_studies*2^0.07,
                                                   model == "Random intercetp - ML" ~ n_studies*2^0.21),
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
  plot <- sim_results_stacked |>
    ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = value, color = model, shape = int_est_n_factor)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(cols = ggplot2::vars(ICC,int_pred_corr, study_sample_size_train), rows = ggplot2::vars(metric, what), scales = "free_y", switch = "y") +
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


sim_results_lazy_stack <- function(sim_results) {
  sim_results_mod_est <- sim_results |>
    dplyr::mutate(value = .data$est, what = "Performance") |>
    dplyr::filter(metric!="var_u")
  sim_results_mod_tau2 <- sim_results |>
    dplyr::mutate(value = .data$tau2, what = "Tau-Squared") |>
    dplyr::filter(metric!="var_u")
  sim_results_mod_error_var_u <- sim_results |>
    dplyr::mutate(value = .data$error_var_u, what = "Error Var_u") |>
    dplyr::filter(metric=="var_u")
  sim_results_stacked <- dplyr::bind_rows(sim_results_mod_est, sim_results_mod_tau2, sim_results_mod_error_var_u)
  return(sim_results_stacked)
}

robust_t_test <- function(x) {
  CI <- c(NA, NA)
  try(CI <- t.test(x)$conf.int, silent = TRUE)
  return(CI)
}

get_summaries <- function(sim_results) {
  print("running checks")
  check <- try(sim_results |> dplyr::select(test_ss), silent = TRUE)
  if (attr(check, "class")[1]=="try-error") {
    sim_results <- sim_results |> mutate(test_ss = study_sample_size_test)
    print("test_ss added")
  }
  check <- try(sim_results |> dplyr::select(intercept_est_sample_size), silent = TRUE)
  if (attr(check, "class")[1]=="try-error") {
    sim_results <- sim_results |> mutate(intercept_est_sample_size = study_sample_size_train)
    print("intercept_est_sample_size added")
  }

  check <- try(sim_results |> dplyr::select(n_studies_test), silent = TRUE)
  if (attr(check, "class")[1]=="try-error") {
    sim_results <- sim_results |> mutate(n_studies_test = n_studies)
    print("n_studies_test added")
  }

  check <- try(sim_results |> dplyr::select(int_pred_corr), silent = TRUE)
  if (attr(check, "class")[1]=="try-error") {
    sim_results <- sim_results |> mutate(int_pred_corr = 0)
    print("int_pred_corr added")
  }



  summaries <- sim_results |>
    sim_results_lazy_stack() |>
    dplyr::select(metric, what, value, ICC, R2, study_sample_size_train, n_studies, model, sigma_u, sigma_e, intercept_est_sample_size, test_ss, n_studies_test, int_pred_corr) |>
    dplyr::group_by(across(c(-value))) |>
    dplyr::summarise(mean = mean(value), ll = robust_t_test(value)[1], ul = robust_t_test(value)[2],  n = sum(!is.na(value)), sigma_u = mean(sigma_u), sigma_e = mean(sigma_e))
  return(summaries)
}

