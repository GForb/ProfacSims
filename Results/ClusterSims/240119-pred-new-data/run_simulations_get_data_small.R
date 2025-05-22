#!/usr/bin/env Rscript

# 240119-pred-new-data

batch_no <- 962


ProfacSims:::set_stream_seed(batch_no, seed =644646468)

print("Batch-no")
print(batch_no)
print("Random number for checking variation in seed:")
runif(1) # random number for chekcing that seed is correctly set.

sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_data),
  n_studies = c(4),
  model_function_list = list(list(ProfacSims:::model_lmm_random_int_reml)),
  study_sample_size_train = c(50),
  ICC = c(0.2),
  R2 = c(0.4),
  intercept_est_sample_size = list(c(10, 50, 200)),
  n_studies_test = 30,
  n_predictors = 12,
  study_sample_size_test = 5000,
  save_data = TRUE
)
tictoc::tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
tictoc::toc()

results |> dplyr::filter(metric == "var_u") |> dplyr::select(metric, est, predict_method, intercept_est_sample_size2, sigma_u, error_var_u)


train_data <- results$train_data[[1]]
test_data_l
