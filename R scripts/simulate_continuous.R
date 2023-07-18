library(tidyr)
library(dplyr)
library(furrr)
library(tictoc)
library(ProfacSims)
plan("multisession")

set.seed = 123123

sim_params <- list(
  nreps = 100,
  sim_rep_fun = list(sim_rep_continuous),
  n_studies = c(4, 8, 16, 32 ,100),
  model_function_list = list(list(model_lm_fixed_int, model_lm, model_lmm_random_int_reml, model_lmm_random_int_ml)),
  study_sample_size_train = c(50, 200, 1000),
  study_sample_size_test = 5000,
  sigma = list(c(ICC = 0.3, R2 = 0.7), c(c(ICC = 0.05, R2 = 0.7)), c(c(ICC = 0, R2 = 0.7)))
)



tic()
sim_results_new <- do.call(ipdma_simulation, sim_params)
toc()

file_name <- paste("sim_results", format(lubridate::now(), "%y-%m-%d_%H-%M"), sep = "_")
save(sim_results_new, file = here::here("Results/", file_name))

plot.sim_results(sim_results_new)



