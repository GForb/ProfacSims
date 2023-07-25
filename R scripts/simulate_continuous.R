install_github("GForb/ProfacSims", upgrade= "never")
library(tidyr)
library(dplyr)
library(furrr)
library(tictoc)
library(ProfacSims)
plan("multisession")

set.seed = 123123

sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous),
  n_studies = c(4, 8, 16, 32 ,64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  study_sample_size_test = 5000,
  sigmas = list(c(ICC = 0.3, R2 = 0.4), c(c(ICC = 0.05, R2 = 0.4)), c(c(ICC = 0, R2 = 0.4)))
)

sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(4, 8, 16, 32 ,64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  study_sample_size_test = 5000,
  sigmas = list(c(ICC = 0.3, R2 = 0.4), c(c(ICC = 0.05, R2 = 0.4)), c(c(ICC = 0, R2 = 0.4))),
  intercept_est_sample_size = 10,
  n_studies_test = 100
)


tic()
sim_results_new <- do.call(ProfacSims:::ipdma_simulation, sim_params)
toc()

file_name <- paste("sim_results", format(lubridate::now(), "%y-%m-%d_%H-%M"), sep = "_")
save(sim_results_new, file = here::here("Results/", file_name))
file_name <- paste("sim_results_r2-25_nrep100", format(lubridate::now(), "%y-%m-%d_%H-%M"), sep = "_")
save(sim_results_new, file = here::here("Results/", file_name))

ProfacSims:::plot.sim_results(sim_results_new)


summaries <- sim_results_new |>
  sim_results_lazy_stack() |>
  select(metric, what, value, ICC, R2, study_sample_size_train, n_studies, model) |>
  group_by(across(c(-value))) |>
  summarise(mean = mean(value), n = sum(!is.na(value)))

summaries |>
  rename(value = mean) |>
  ProfacSims:::plot.sim_results(stack = FALSE)

summaries |>
  filter(model != "lm") |>
  rename(value = mean) |>
  ProfacSims:::plot.sim_results(stack = FALSE)


# To do:
# Evaluate predictions in new data:
# - have sample size for estimating new study intercepts
# - Have separate data for estimating intercepts and predictions

# Add confidence intervals to plot
# Plot tau not tau-squared

# Explore surrogate modelling - check paper, what is aim,

sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(4, 8, 16, 32 ,64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  study_sample_size_test = 5000,
  sigmas = list(c(ICC = 0.3, R2 = 0.4), c(c(ICC = 0.05, R2 = 0.4)), c(c(ICC = 0, R2 = 0.4)),
  intercept_est_sample_size = 10,
  n_studies_test = 30)
)


tic()
sim_results_new <- do.call(ProfacSims:::ipdma_simulation, sim_params)
toc()



