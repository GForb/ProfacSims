library(tidyverse)

sim_name <- "240211-many-pred"
i <- 1

betas <- data |> slice(5) |> select(betas)
betas2 <- data |> slice(10) |> select(betas)

results_folder <- here::here("Results/ClusterSims/", sim_name)
save_time = lubridate::now()


sim_no <- formatC(i, width = 4, format = "d", flag = "0")
file <- paste0("results", sim_no, ".RDS")

print(file)

data <- readRDS(here::here(results_folder, file)) |> tibble()
tibble(data)

colnames(data)
data$predict_method |> table()
data |> filter(predict_method == "new_dynamic") |> select(metric, est, betas, model, predict_method, coef, studyid, sigma_u, beta_x, beta_int, error_var_u)

model1 <- data |> filter(predict_method == "new_dynamic") |> select(metric, est, betas, model, predict_method, coef, studyid, sigma_u, beta_x, beta_int, error_var_u) |> slice(5)

data |> filter(!is.na(coef))

# try processing
processed_data <- data |>
  get_results_df(sim_name = sim_name, filename = file) |>
  dplyr:: mutate(save_time = save_time)

colnames(processed)
