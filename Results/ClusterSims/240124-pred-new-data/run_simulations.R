#!/usr/bin/env Rscript

# 240124-pred-new-data

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
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  ICC = c(0, 0.05, 0.2),
  R2 = c(0.4, 0.7),
  intercept_est_sample_size = list(c(10, 50, 200)),
  n_studies_test = 30,
  predictor_intercepts = "random",
  n_predictors = 12,
  study_sample_size_test = 5000
)
tictoc::tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
tictoc::toc()

sim_no_sting <- formatC(batch_no, width = 4, format = "d", flag = "0")
results$batch_no <- batch_no
saveRDS(results, file = paste0("results", sim_no_sting, ".RDS"))


