library(tidyverse)

sim_name <- "240131-cluster-x"
i <- 1


results_folder <- here::here("Results/ClusterSims/", sim_name)
save_time = lubridate::now()


sim_no <- formatC(i, width = 4, format = "d", flag = "0")
file <- paste0("results", sim_no, ".RDS")

print(file)

data <- readRDS(here::here(results_folder, file)) |> tibble()
tibble(data)

colnames(data)


data |> filter(!is.na(coef))

# try processing
processed_data <- data |>
  get_results_df(sim_name = sim_name, filename = file) |>
  dplyr:: mutate(save_time = save_time)

colnames(processed_data)
