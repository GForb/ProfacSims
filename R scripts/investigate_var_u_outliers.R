library(tidyverse)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Investigating high error_var_u

results <- readRDS(file = "Results/ClusterSims/SimInv/data962.RDS")
colnmaes <- colnames(results)



scenario_all_methods <- results |> filter(error_var_u > 0.7) |>
  select(metric, est, betas, model, train_data, n_studies, sigma_u, ICC, R2, error_var_u)

scenario <- scenario_all_methods |> filter(predict_method = "new0")
train_data <- scenario$train_data[[1]]


model <- model_lmm_random_int_reml(train_data)
get_var_u(model) # confrimed :)
scenario$sigma_u^2


# How does dynamic compare? How come so much better?
results |>
  filter(ICC ==0.2, R2 == 0.4, n_studies ==4, model == "Random intercetp - REML", study_sample_size_train ==50, intercept_est_sample_size==50) |>
  select(metric, est, betas, model, train_data,  error_var_u, predict_method)

results |> filter(metric == 'var_u', ICC == 0.2, R2 == 0.4, model == "Random intercetp - REML", study_sample_size_train ==50, n_studies ==4) |>
  select(metric, est, betas, model, train_data,  error_var_u, predict_method, intercept_est_sample_size)
