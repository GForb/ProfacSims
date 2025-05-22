#!/usr/bin/env Rscript

# 240131-cluster-x

args = commandArgs(trailingOnly=TRUE)

batch_no <- as.integer(args)



ProfacSims:::set_stream_seed(batch_no, seed =644646468)

print("Batch-no")
print(batch_no)
print("Random number for checking variation in seed:")
runif(1) # random number for chekcing that seed is correctly set.

sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(4,8,16,32,64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml,
                                  ProfacSims:::model_lmm_lm_hausman)),
  study_sample_size_train = c(50, 200, 1000), #
  ICC = c(0.05, 0.2),
  R2 = c(0.4),
  single_x = TRUE,
  b_w_ratio = c(1, 1.25, 2), #
  pred_icc = c(0, 0.05, 0.2, 0.5, 0.9),  #
  int_pred_corr = 0,
  intercept_est_sample_size = list(c(200)),
  n_studies_test = 30,
  predictor_intercepts = "random",
  n_predictors = 1,
  study_sample_size_test = 5000
)

#sigma_checks <- do.call(ProfacSims:::check_simulation_pipeline, sim_params)
#sigma_checks |> print(n = 1000)

tictoc::tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
tictoc::toc()

sim_no_sting <- formatC(batch_no, width = 4, format = "d", flag = "0")
results$batch_no <- batch_no
saveRDS(results, file = paste0("results", sim_no_sting, ".RDS"))

