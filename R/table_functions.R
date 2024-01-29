quantile_df <- function(x, probs = c(0.25, 0.5, 0.75)) {
  tibble::tibble(
    val = quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}

quantiles_table_est <- function(data) {
  data |>
    summarise(
      p005 = stats::quantile(est, probs = 0.05) |> round(digits = 2),
      p25 = stats::quantile(est, probs = 0.25)|> round(digits = 2),
      p50 = stats::quantile(est, probs = 0.50)|> round(digits = 2),
      p75 = stats::quantile(est, probs = 0.75)|> round(digits = 2),
      p95 = stats::quantile(est, probs = 0.95)|> round(digits = 2)
  )
}


quantiles_table_tau <- function(data) {
  data |>
    mutate(tau = sqrt(tau2)) |>
    summarise(
      p005 = stats::quantile(tau, probs = 0.05) |> round(digits = 2),
      p25 = stats::quantile(tau, probs = 0.25)|> round(digits = 2),
      p50 = stats::quantile(tau, probs = 0.50)|> round(digits = 2),
      p75 = stats::quantile(tau, probs = 0.75)|> round(digits = 2),
      p95 = stats::quantile(tau, probs = 0.95)|> round(digits = 2)
    )
}
