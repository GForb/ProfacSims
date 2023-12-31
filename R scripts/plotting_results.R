library(dplyr)

load("Results/sim_results_r2-70_nrep100_23-07-19_21-20")

sim_results_r2_70 <- sim_results_new
load("Results/sim_results_r2-25_nrep100_23-07-20_01-30")

sim_results_r2_25 <- sim_results_new



robust_t_test <- function(x) {
  CI <- c(NA, NA)
  try(CI <- t.test(x)$conf.int, silent = TRUE)
  return(CI)
}



summaries_r2_70 <- get_summaries(sim_results_r2_70) |>
  dplyr::rename(value = mean)

report_sigmas <- function(summaries) {
  summaries |> ungroup() |>  dplyr::select(ICC, R2, sigma_u, sigma_e) |>
    group_by(across(c(ICC, R2))) |>
    summarise(sigma_u = mean(sigma_u), sigma_e = mean(sigma_e)) |>
    mutate(sigma2_u = sigma_u^2, sigma2_e = sigma_e^2, var_y = sigma2_u + sigma2_e +1)
}
summaries_r2_70 |> report_sigmas()


summaries_r2_25 <- get_summaries(sim_results_r2_25) |>
  dplyr::rename(value = mean)


summaries_r2_70 |> dplyr::filter(ICC == 0) |>
  plot.sim_results_old(stack = FALSE, CI = TRUE)

summaries_r2_70 |> dplyr::filter(ICC == 0.3 | ICC ==0, metric == "calib_itl") |>
  plot.sim_results_old(stack = FALSE, CI = TRUE)

summaries_r2_70 |> dplyr::filter(ICC == 0.3 | ICC ==0, metric == "var_u") |>
  plot.sim_results_old(stack = FALSE, CI = TRUE)

summaries_r2_70 |> dplyr::filter(ICC == 0.3, model != "lm") |>
  plot.sim_results(stack = FALSE, CI = TRUE)

summaries_r2_25|> dplyr::filter(ICC == 0) |>
  plot.sim_results(stack = FALSE, CI = TRUE)

summaries_r2_25 |> dplyr::filter(ICC == 0.3) |>
  plot.sim_results(stack = FALSE, CI = TRUE)


# Plots for Daniel

summaries_r2_25 |> report_sigmas()


summaries_r2_25 |> dplyr::filter(ICC == 0.3 | ICC ==0, metric == "calib_itl") |>
  plot.sim_results_old(stack = FALSE, CI = TRUE)

summaries_r2_25 |> dplyr::filter(ICC == 0.3 | ICC ==0, metric == "var_u") |>
  plot.sim_results_old(stack = FALSE, CI = TRUE)

summaries_r2_25 |> dplyr::filter(ICC == 0.3 | ICC ==0, metric == "calib_slope") |>
  plot.sim_results_old(stack = FALSE, CI = TRUE)

summaries_r2_25 |> dplyr::filter(ICC == 0.3 | ICC ==0, metric == "r-squared") |>
  plot.sim_results_old(stack = FALSE, CI = TRUE)
