devtools::install_github("GForb/ProfacSims", upgrade= "never")

sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(16),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,                            
                                  ProfacSims:::model_lmm_random_int_reml)),
  study_sample_size_train = c(50), #
  ICC = c(0.05),
  R2 = c(0.3),
  intercept_est_sample_size = list(c(200)),
  n_studies_test = 30,
  predictor_intercepts = "random",
  n_predictors = c(30),
  study_sample_size_test = 5000
)

# This code checks that specified sim params lead to a viable data generating mechanism
sigma_checks <- do.call(ProfacSims:::check_simulation_pipeline, sim_params)
sigma_checks |> print(n = 1000)

tictoc::tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
tictoc::toc



results |> print(n = Inf)