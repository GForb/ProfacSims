box_plot_cluster_x_sup <- function(data) {
  plot_data <- data |>
    dplyr::mutate(
       pred_icc_factor = factor(pred_icc),
       pred_icc_factor_no = pred_icc_factor |> as.integer(),
       model = factor(model,
                      levels = c("Not adjusting for study",
                                  "Fixed intercept",
                                  "Hausman",
                                  "Random intercept - REML",
                                  "Random intercept - REML x1bar"),
                       labels = c("Not adjusting for study",
                                  "Fixed intercept",
                                  "Hausman",
                                  "Random intercept",
                                  "Within-between random intercept")),
       model_number = as.integer(model),
       x = pred_icc_factor_no +(model_number-3)/10,
       study_sample_size_train = factor(study_sample_size_train, levels = c("50", "200", "1000"), labels = c("Study size: 50","Study size: 200", "Study size: 1000" )),
       b_w_ratio = factor(b_w_ratio, levels = c("1", "1.25", "2"), labels = c("Between effects = Within Effects","Between effects 25% > Within", "Between effects double Within effects")),
       ICC = factor(ICC, levels = c("0", "0.05", "0.2"), labels = c("ICC: 0","ICC: 0.05", "ICC: 0.2")))

  plot_data |> ggplot2::ggplot(mapping = aes(x = x, y = plot_value, colour = model, group = x)) +
    geom_boxplot(outlier.size = 0.001) +
    scale_x_continuous(breaks = c("0" = 1, "0.05" = 2, "0.2" = 3, "0.5" = 4, "0.9" = 5)) +
    xlab("Predictor ICC") +
    facet_grid(rows = vars(study_sample_size_train, ICC), cols = vars(b_w_ratio), labeller = label_value) +
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "top") +
    scale_colour_viridis_d(option = "H", direction = -1)
}
