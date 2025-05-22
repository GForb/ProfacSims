library(tidyverse)
library(ggplot2)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Loading files:
sim_name <- "240124-pred-new-data"
results_folder <- here("Results", "Database-extracts", sim_name)

calib_itl <- readRDS(file = here(results_folder, "calib_itl.RDS"))
 model_data <- calib_itl |> mutate(
   model = factor(model),
   predict_method = factor(predict_method),
 )

 model <- lm("est ~ model", data = model_data)

 summary(model)

 model_t <- lm("tau2 ~ n_studies + ICC + R2 + study_sample_size_train + model + predict_method + intercept_est_sample_size", data = model_data)

 summary(model_t)

 wide_est <- calib_itl |>
   select(n_studies, ICC, R2, study_sample_size_train, est, batch_no, model, intercept_est_sample_size, predict_method) |>
   pivot_wider(names_from = "model", values_from = "est")


 colnames(wide_est)[8] <- "fixed"
 colnames(wide_est)[9] <- "no_adj"
 colnames(wide_est)[10] <- "rand_reml"
 colnames(wide_est)[11] <- "rand_ml"

 wide_est


 wide_est <- wide_est |> rowwise() |> mutate(diff = abs(min(fixed, no_adj, rand_reml, rand_ml) - max(fixed, no_adj, rand_reml, rand_ml)))

 wide_est_pred0 <- wide_est |> filter(predict_method == "new0")

 max(wide_est_pred0$diff)

 wide_est_no_adj <- wide_est |> select(-fixed, -rand_reml, -rand_ml, -diff) |> pivot_wider(names_from = predict_method, values_from = no_adj) |>
   mutate(diff = abs(new0 - new_dynamic))

 max(wide_est_no_adj$diff)

 wide_est_no_adj |> filter(diff > 0.2)



 wide_est_fixed <- wide_est |> filter(predict_method == "new_studies") |>
   mutate(diff_fixed = abs(no_adj - fixed))

 max(wide_est_fixed$diff_fixed)
