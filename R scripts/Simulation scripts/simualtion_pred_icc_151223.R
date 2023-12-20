# Simulation to test hypothesis that Random intercepts will have lower MSE for betas when there is large between-study variance in beta as they can make use of more information.
install_github("GForb/ProfacSims", upgrade= "never")
library(tictoc)
sim_params_test <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(4,8,16,32,64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50),
  ICC = c(0.05),
  R2 = c(0.4, 0.7),
  pred_icc = c(0, 0.05, 0.5, 0.9),
  int_pred_corr = 0,
  intercept_est_sample_size =100,
  n_studies_test = 30,
  study_sample_size_test = 5000,
  predictor_intercepts = "random",
  n_predictors = 1
)

set.seed(1234)

tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params_test)
toc()

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


set.seed(1234)

tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params_test)
toc()

save(results,file =  here::here("Results/Pred ICC 151223/pred_icc_151223.RData"))


tic()
ProfacSims:::run_simualtions_in_batches(sim_params = sim_params, n_batches = 20, save_place = "Results/Pred ICC 151223")
toc()



db <- DBI::dbConnect(RSQLite::SQLite(), "Results/Database/sim_results.db")
DBI::dbExecute(db, "DELETE FROM  sim_results_v3 WHERE sim_name = 'pred_icc_061223'")

save_batch_results_db(database_connection = db,
                      table = "sim_results_v3",
                      results_folder = here::here("Results/Pred ICC 111223"),
                      sim_name = "pred_icc_061223")

DBI::dbDisconnect(db)




