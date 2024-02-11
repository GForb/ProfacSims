library(tidyverse)

sim_name <- "240211-many-pred"
i <- 1



results_folder <- here::here("Results/ClusterSims/", sim_name)
save_time = lubridate::now()


sim_no <- formatC(i, width = 4, format = "d", flag = "0")
file <- paste0("results", sim_no, ".RDS")

print(file)

data <- readRDS(here::here(results_folder, file)) |> tibble()
tibble(data)

betas <- data |> slice(5) |> select(betas)
betas2 <- data |> slice(10) |> select(betas)

colnames(data)
data$intercept_est_sample_size |> table()
