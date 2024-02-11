box_plot_cluster_x <- function(data) {
  plot_data <- data |>
    dplyr::mutate(
       pred_icc_factor = factor(pred_icc),
       pred_icc_factor_no = pred_icc_factor |> as.integer(),
       model_number = model |> factor() |> as.integer(),
       x = pred_icc_factor_no +(model_number-3)/10
    )

  plot_data |> ggplot2::ggplot(mapping = aes(x = x, y = plot_value, colour = model, group = x)) +
    geom_boxplot(outlier.size = 0.01) +
    scale_x_continuous(breaks = c("0" = 1, "0.05" = 2, "0.2" = 3, "0.5" = 4, "0.9" = 5)) +
    xlab("Predictor ICC") +
    facet_grid(rows = vars(n_studies), cols = vars(b_w_ratio)) +
    guides(color = guide_legend(nrow = 2))
    theme(legend.position = "top")
}
