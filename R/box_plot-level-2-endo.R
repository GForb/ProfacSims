box_plot_lvl2_endo_no_facets <-  function(data) {
  cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#CC79A7", "#009E73")

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
      int_pred_corr = factor(int_pred_corr, levels = c("0", "0.2", "0.5"), labels = c("corr(u,x): 0","Corr(u,x): 0.2", "Corr(u,x): 0.5" )),
      ICC = factor(ICC, levels = c("0", "0.05", "0.2"), labels = c("ICC: 0","ICC: 0.05", "ICC: 0.2"))
    )

  plot_data |> ggplot2::ggplot(mapping = aes(x = x, y = plot_value, colour = model, group = x)) +
    geom_boxplot(outlier.size = 0.001) +
    scale_x_continuous(breaks = c("0.25" = 1, "0.5" = 2, "0.9" = 3)) +
    xlab("Predictor ICC") +
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "top") +
    scale_colour_manual(values=cbp1)
}

box_plot_lvl2_endo_main <- function(data) {
  box_plot_lvl2_endo_no_facets(data) +
    facet_grid(rows = vars(study_sample_size_train), cols = vars(int_pred_corr), labeller = label_value)
}

box_plot_lvl2_endo_sup <- function(data) {
  box_plot_lvl2_endo_no_facets(data) +
    facet_grid(rows = vars(study_sample_size_train, ICC), cols = vars(int_pred_corr), labeller = label_value)
}

