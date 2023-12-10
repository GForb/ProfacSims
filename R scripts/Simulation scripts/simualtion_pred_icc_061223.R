# Simulation to test hypothesis that Random intercepts will have lower MSE for betas when there is large between-study variance in beta as they can make use of more information.
install_github("GForb/ProfacSims", upgrade= "never")

sim_params <- list(
  nreps = 1,
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

do.call(ipdma_simulation, sim_params)
