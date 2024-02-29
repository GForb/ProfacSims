
create_stacked_results <- function(data) {
  data0 <- data |> filter(predict_method == "new0", intercept_est_sample_size == 10) |>
    mutate(intercept_est_sample_size = 0)
  data_stacked <- data |> filter(predict_method != "new0") |> bind_rows(data0)
  return(data_stacked)
}

correct_model_spelling <- function(data) {
  data <- data |>
    mutate(
      model = case_when(model == "Random intercetp - ML" ~ "Random intercept - ML",
                        model == "Random intercetp - REML" ~ "Random intercept - REML",
                        model == "Random intercetp - REML x1bar" ~ "Random intercept - REML x1bar"
                        TRUE ~ model))

}
