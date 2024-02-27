


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
  data <- data |> mutate(model_factor = factor(model), predict_method_number = factor(predict_method))

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

box_plot_by_model_predict_method <- function(data,
                                             what, ylab,
                                             facet_cols = NULL,
                                             facet_rows = ggplot2::vars(ICC)
) {
  data <- data |> mutate(
                         predict_method_number = case_when(predict_method == "new0" ~ 1,
                                                           predict_method == "new_studies" ~ 2,
                                                           predict_method == "new_dynamic" ~ 3),
                         model_factor = ordered(model, levels = c("Not adjusting for study", "Fixed intercept", "Random intercept - ML", "Random intercept - REML")),
                         model_number = model_factor |> as.numeric(),
                         model_predict_method = predict_method_number + (model_number - 2.5)/6,
                         tau = sqrt(tau2))



  data |>  ggplot2::ggplot(ggplot2::aes(x = model_predict_method, y = .data[[what]], group = model_predict_method, color = model_factor )) +
    ggplot2::geom_boxplot(outlier.size = 0.1,) +
    scale_x_continuous(breaks = c(1,2,3), labels = c("Average Intercept", "New studies", "Dynamic")) +
    ggplot2::labs(
      x = "Intercept prediction method",
      y = ylab,
      color = "Model:"
    ) +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    theme(legend.position = "top")
}

box_plot_by_predict_method <- function(data, what, ylab) {
  data <- data |> mutate(
    predict_method_number = case_when(predict_method == "new0" ~ 1,
                                      predict_method == "new_studies" ~ 2,
                                      predict_method == "new_dynamic" ~ 3),
    model_factor = ordered(model, levels = c("Not adjusting for study", "Fixed intercept", "Random intercept - ML", "Random intercept - REML")),
    model_number = model_factor |> as.numeric(),
    model_predict_method = predict_method_number + (model_number - 2.5)/6,
    tau = sqrt(tau2))

  facet_cols =  ggplot2::vars(model_factor)
  facet_rows = ggplot2::vars(intercept_est_sample_size)

  data |>  ggplot2::ggplot(ggplot2::aes(x = predict_method_number, y = .data[[what]], group = predict_method_number, color = predict_method )) +
    ggplot2::geom_boxplot(outlier.size = 0.1) +
    scale_x_continuous(breaks = c(1,2,3), labels = c("Average \n Intercept", "New \n studies", "Dynamic")) +
    ggplot2::labs(
      x = "Intercept prediction method",
      y = ylab,
      color = "Method for predicting intercepts:"
    ) +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    theme(legend.position = "top")
}





box_plot_ml_fixed<- function(data,
                             what, ylab,
                             facet_cols = NULL,
                             facet_rows = ggplot2::vars(ICC)
) {
  data <- data |>
    filter(predict_method %in% c("new0", "new_studies"),
           model %in% c("Fixed intercept", "Random intercept - REML", "Not adjusting for study")) |>
    mutate(
    predict_method_number = case_when(predict_method == "new0" ~ 1,
                                      predict_method == "new_studies" ~ 2),
    model_factor = ordered(model, levels = c("Not adjusting for study", "Fixed intercept", "Random intercept - REML")),
    model_number = model_factor |> as.numeric(),
    x = case_when(
      model_number ==1   ~ n_studies*2^(-0.1),
      model_number ==2  ~n_studies*2^(0),
      model_number ==3  ~n_studies*2^(0.1))
,
    tau = sqrt(tau2))

  data |>  ggplot2::ggplot(ggplot2::aes(x = x, y = .data[[what]], group = x, color = model_factor )) +
    ggplot2::geom_boxplot(outlier.size = 0.1,) +
    ggplot2::scale_x_continuous(trans='log2') +
    ggplot2::labs(
      x = "Number of studies",
      y = ylab,
      color = "Model:"
    ) +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    theme(legend.position = "top")
}

box_plot_by_pred_ss <- function(data, what) {
  plot_data <- data |> mutate(factor_int_est_ss = factor(intercept_est_sample_size),
                 x = factor_int_est_ss |> as.numeric) |>
                 dplyr::mutate(x = dplyr::case_when(
                   model == "Not adjusting for study" ~ x - 0.21,
                   model == "Fixed intercept" ~ x - 0.07,
                   model == "Random intercept - REML" ~ x + 0.07,
                   model == "Random intercept - ML" ~ x + 0.21)) |>

    plot_data |> ggplot2::ggplot(ggplot2::aes(x = x, y = .data[[what]], group = x, color = model )) +
    ggplot2::geom_boxplot(outlier.size = 0.1,) +
    ggplot2::labs(
      x = "Intercept estimation sample size",
      color = "Model:"
    ) +
    ggplot2::facet_grid(cols = facet_cols, rows = facet_rows, switch = "y", scales = "fixed") +
    theme(legend.position = "top")
}
