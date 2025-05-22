#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)


batch_no <- as.integer(args)
set.seed(seed = 1234, kind = "L'Ecuyer-CMRG")

seed <- ProfacSims:::get_stream_seed(batch_no)


.Random.Seed <- ProfacSims:::get_stream_seed(batch_no)


sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(4, 8, 16, 32, 64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  ICC = c(0,0.05, 0.3),
  R2 = c(0.4, 0.7),
  pred_icc = c(0, 0.05, 0.5, 0.9),
  int_pred_corr = 0,
  intercept_est_sample_size =100,
  n_studies_test = 30,
  predictor_intercepts = "random",
  n_predictors = 1,
  study_sample_size_test = 5000
)
tictoc::tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
tictoc::toc()

saveRDS(results, file = paste0("results", batch_no, ".RDS"))


