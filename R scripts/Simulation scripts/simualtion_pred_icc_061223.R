# Simulation to test hypothesis that Random intercepts will have lower MSE for betas when there is large between-study variance in beta as they can make use of more information.
install_github("GForb/ProfacSims", upgrade= "never")
library(tictoc)


sim_params <- list(
  nreps = 500,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_model_only),
  n_studies = c(4, 8, 16, 32, 64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  ICC = c(0,0.05, 0.3),
  R2 = c(0.4, 0.7),
  pred_icc = c(0, 0.5),
  predictor_intercepts = "random",
  n_predictors = 1
)


set.seed(1234)

tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
toc()

save(results,file =  here::here("Results/Pred ICC 111223/pred_icc_061223.RData"))



sim_params <- list(
  nreps = 500,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_model_only),
  n_studies = c(4, 8, 16, 32, 64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  ICC = c(0,0.05, 0.3),
  R2 = c(0.4, 0.7),
  pred_icc = c(0.05, 0.9, 0, 0.5),
  predictor_intercepts = "random",
  n_predictors = 1
)


set.seed(1234)

tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
toc()

save(results,file =  here::here("Results/Pred ICC 111223/pred_icc_0590_061223.RData"))

# Retrospectively adding pred_icc

sigma_params <- list(
  ICC = c(0,0.05, 0.3),
  R2 = c(0.4, 0.7),
  pred_icc = c(0, 0.05, 0.5, 0.9),
  n_predictors = 1
)

sigma_params <- sigma_params <- do.call(tidyr::expand_grid, sigma_params)

beta_int_mapping <- sigma_params |> dplyr::rowwise() |> mutate(beta_int = get_sigmas(ICC= ICC, R2 = R2, pred_icc = pred_icc, n_predictors = n_predictors)$beta_int) |> ungroup()

load(here::here("Results/Pred ICC 111223/pred_icc_0590_061223.RData"))
results <- results |> left_join(beta_int_mapping)
colnames(results)
save(results,file =  here::here("Results/Pred ICC 111223/pred_icc_0590_061223.RData"))

load(here::here("Results/Pred ICC 111223/pred_icc_061223.RData"))
results <- results |> left_join(beta_int_mapping)
colnames(results)

save(results,file =  here::here("Results/Pred ICC 111223/pred_icc_061223.RData"))



db <- DBI::dbConnect(RSQLite::SQLite(), "Results/Database/sim_results.db")
#dbExecute(db, "DELETE FROM  sim_results_v2 WHERE sim_name = 'pred_icc_061223'")

save_batch_results_db(database_connection = db,
                      table = "sim_results_v3",
                      results_folder = here::here("Results/Pred ICC 111223"),
                      sim_name = "pred_icc_061223")

DBI::dbDisconnect(db)




