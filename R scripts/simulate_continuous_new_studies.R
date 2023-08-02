install_github("GForb/ProfacSims", upgrade= "never")

library(tictoc)

tic()
library(tidyr)
library(dplyr)
library(furrr)
library(ProfacSims)
plan("multisession")
toc()
set.seed(123123)

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


tic()
sim_results_new <- do.call(ProfacSims:::ipdma_simulation, sim_params)
toc()
file_name <- paste("sim_results_int_vary_test_est", format(lubridate::now(), "%y-%m-%d_%H-%M"), sep = "_")
save(sim_results_new, file = here::here("Results/", file_name))



# To-do - write new plotting functions
ProfacSims:::plot.sim_results(sim_results_new)

get_summaries(sim_results_new) |>
  dplyr::rename(value = mean) |>
  plot.sim_results(stack = FALSE, CI = TRUE)

get_summaries(sim_results_new) |>
  filter(int_pred_corr == 0, ICC ==0) |>
  dplyr::rename(value = mean) |>
  plot.sim_results(stack = FALSE, CI = TRUE)
file_name <- paste("sim_int_pred_corr_0_ICC_0.3.png", format(lubridate::now(), "%y-%m-%d_%H-%M"), sep = "_")

ggplot2::ggsave(filename = here::here("Results/", file_name), width = 18, height = 18, units = "cm", device = "png")

get_summaries(sim_results_new) |>
  filter(int_pred_corr == 0, ICC ==0.3) |>
  dplyr::rename(value = mean) |>
  plot.sim_results(stack = FALSE, CI = TRUE)
file_name <- paste("sim_int_pred_corr_0_ICC_0.3", format(lubridate::now(), "%y-%m-%d_%H-%M"), sep = "_")
ggplot2::ggsave(filename = here::here("Results/", file_name), width = 18, height = 18, units = "cm", device = "png")

get_summaries(sim_results_new) |>
  filter(int_pred_corr == 0.5, ICC ==0.3) |>
  dplyr::rename(value = mean) |>
  plot.sim_results(stack = FALSE, CI = TRUE)
file_name <- paste("sim_int_pred_corr_0.5_ICC_0", format(lubridate::now(), "%y-%m-%d_%H-%M"), ".png", sep = "_")

ggplot2::ggsave(filename = here::here("Results/", file_name), width = 18, height = 18, units = "cm", device = "png")

get_summaries(sim_results_new) |>
  filter(int_pred_corr == 0.5, ICC ==0.3) |>
  dplyr::rename(value = mean) |>
  plot.sim_results(stack = FALSE, CI = TRUE)
file_name <- paste("sim_int_pred_corr_0.5_ICC_0.3", format(lubridate::now(), "%y-%m-%d_%H-%M"), sep = "_")

ggplot2::ggsave(filename = here::here("Results/", file_name), width = 18, height = 18, units = "cm", device = "png")
