box_plot_lvl2_endo <- function(data) {
  plot_data <- data |>
    dplyr::mutate(
      pred_icc_factor = factor(pred_icc),
      pred_icc_factor_no = pred_icc_factor |> as.integer(),
      model_number = model |> factor() |> as.integer(),
      x = pred_icc_factor_no +(model_number-3)/10
    )

  plot_data |> ggplot2::ggplot(mapping = aes(x = x, y = plot_value, colour = model, group = x)) +
    geom_boxplot(outlier.size = 0.001) +
    scale_x_continuous(breaks = c("0.25" = 1, "0.5" = 2, "0.9" = 3)) +
    xlab("Predictor ICC") +
    facet_grid(rows = vars(study_sample_size_train), cols = vars(int_pred_corr), labeller = label_both) +
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "top")
}

