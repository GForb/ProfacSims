box_plot_error_var_u <- function(sim_results) {
  study_sample_size_train.lab <- c("Study sample size = 50", "Study sample size = 200",  "Study sample size = 1000")
  names(study_sample_size_train.lab)  <- c("50", "200", "1000")


  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Random intercept - REML" ~ n_studies*2^-0.1,
                                                   model == "Random intercept - ML" ~ n_studies*2^0.1),
                  sigma2_u = sigma_u^2,
                  study_sample_size_train = factor(study_sample_size_train, levels = c("50", "200", "1000"), labels = c("Study size: 50","Study size: 200", "Study size: 1000" )),
                  ICC = factor(ICC, levels = c("0", "0.05", "0.2"), labels = c("ICC: 0","ICC: 0.05", "ICC: 0.2"))

    )

  facet_cols <-  ggplot2::vars(study_sample_size_train)
  facet_rows = ggplot2::vars(ICC)

  plot <- sim_results_mod |>  ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = est, group = n_studies_mod, color = model )) +
    ggplot2::geom_boxplot(outlier.size = 0.1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = sigma2_u, linetype = "True random intercept variance"), linetype = "dashed") +
    ggplot2::facet_grid(
      cols = facet_cols,
      rows = facet_rows, switch = "y",
      scales = "free_y",
      labeller = label_value) +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Estimated Random intercept variance",
    ) +
    theme(legend.position = "top")

  return(plot)
}

box_plot_error_var_u_supp <- function(sim_results) {
  study_sample_size_train.lab <- c("Study sample size = 50", "Study sample size = 200",  "Study sample size = 1000")
  names(study_sample_size_train.lab)  <- c("50", "200", "1000")


  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Random intercept - REML" ~ n_studies*2^-0.1,
                                                   model == "Random intercept - ML" ~ n_studies*2^0.1),
                  sigma2_u = sigma_u^2,
                  study_sample_size_train = factor(study_sample_size_train, levels = c("50", "200", "1000"), labels = c("Study size: 50","Study size: 200", "Study size: 1000" )),
                  ICC = factor(ICC, levels = c("0", "0.05", "0.2"), labels = c("ICC: 0","ICC: 0.05", "ICC: 0.2")),
                  R2 = factor(R2, levels = c("0.4", "0.7"), labels = c("R2: 0.4","R2: 0.7"))
    )



  facet_cols <-  ggplot2::vars(study_sample_size_train)
  facet_rows = ggplot2::vars(ICC, R2)

  plot <- sim_results_mod |>  ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = est, group = n_studies_mod, color = model )) +
    ggplot2::geom_boxplot(outlier.size = 0.1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = sigma2_u, linetype = "True random intercept variance"), linetype = "dashed") +
    ggplot2::facet_grid(
      cols = facet_cols,
      rows = facet_rows, switch = "y",
      scales = "free_y",
      labeller = label_value) +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Estimated Random intercept variance",
    ) +
    theme(legend.position = "top")

  return(plot)
}

box_plot_error_var_u_no_facet_rows <- function(sim_results) {
  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Random intercept - REML" ~ n_studies*2^-0.1,
                                                   model == "Random intercept - ML" ~ n_studies*2^0.1),
                  sigma2_u = sigma_u^2
    )

  facet_cols <-  ggplot2::vars(study_sample_size_train)

  plot <- sim_results_mod |>  ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = est, group = n_studies_mod, color = model )) +
    ggplot2::geom_boxplot(outlier.size = 0.1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = sigma2_u, linetype = "True random intercept variance"), linetype = "dashed") +
    ggplot2::facet_grid(cols = facet_cols, switch = "y", scales = "free_y", labeller = label_both) +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Estimated Random intercept variance",
    ) +
    theme(legend.position = "top")

  return(plot)
}


box_plot_error_var_iscb <- function(sim_results) {
  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "REML" ~ n_studies*2^-0.1,
                                                   model ==  "Maximum Liklihood" ~ n_studies*2^0.1),
                  sigma2_u = sigma_u^2
    )
  
  plot <- sim_results_mod |>  ggplot2::ggplot(ggplot2::aes(x = n_studies_mod, y = est, group = n_studies_mod, color = model )) +
    ggplot2::geom_boxplot(outlier.size = 0.1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = sigma2_u, linetype = "True random intercept variance"), linetype = "dashed") +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Estimated Random intercept variance",
      model = ""
    ) +
    theme(legend.position = "top")
  
  return(plot)
}


var_u_zeros_plot <- function(sim_results) {
  sim_results_mod <- sim_results |>
    dplyr::mutate(n_studies_mod = dplyr::case_when(model == "Random intercept - REML" ~ n_studies*2^-0.1,
                                                   model == "Random intercept - ML" ~ n_studies*2^0.1),
                  sigma2_u = sigma_u^2,
                  n_zero = case_when(est ==0 ~0.001,
                                     est > 0 ~0)
    )

  facet_cols <-  ggplot2::vars(study_sample_size_train)

  plot <- sim_results_mod |>  ggplot2::ggplot(ggplot2::aes( y = n_zero, x = n_studies_mod, fill = model)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::facet_grid(cols = facet_cols, switch = "y", scales = "free_y", labeller = label_value) +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies (log scale)",
      y = "Proportion of results with random intercept variance = 0",
    ) +
    theme(legend.position = "top")

  return(plot)
}
