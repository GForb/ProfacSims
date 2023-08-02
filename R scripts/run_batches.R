install_github("GForb/ProfacSims", upgrade= "never")


library(ProfacSims)
library(furrr)
library(tictoc)

sim_params <- list(
  nreps = 20,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(4, 8, 16, 32, 64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  study_sample_size_test = 5000,
  ICC = c(0, 0.3),
  R2 = c(0.7),
  int_pred_corr = c(0, 0.5),
  intercept_est_sample_size = list(c(10,100, 1000)),
  n_studies_test = 20
)


sim_params <- list(
  nreps = 20,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(4),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50),
  study_sample_size_test = 5000,
  ICC = c(0, 0.3),
  R2 = c(0.7),
  int_pred_corr = c(0, 0.5),
  intercept_est_sample_size = list(c(10)),
  n_studies_test = 20
)

tic()
ProfacSims:::run_simualtions_in_batches(sim_params = sim_params, n_batches = 2,filepath = "Results/HolidayBatch")
toc()
