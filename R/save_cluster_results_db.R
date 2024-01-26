
save_cluster_results_db <- function(table, sim_name, db, min_file_no = 1, max_file_no = 1000) {
  db <- dbConnect(RSQLite::SQLite(), "Results/Database/sim_results.db")

  results_folder <- here::here("Results/ClusterSims/", sim_name)
  save_time = lubridate::now()

  for(i in min_file_no:max_file_no){
    sim_no <- formatC(i, width = 4, format = "d", flag = "0")
    file <- paste0("results", sim_no, ".RDS")

      print(file)

      data <- readRDS(here::here(results_folder, file))

      print("processing data")
      processed_data <- data |>
        get_results_df(sim_name = sim_name, filename = file) |>
        dplyr:: mutate(save_time = save_time)

      print("Writing to database")
      write_file_to_db(
        data = processed_data,
        database_connection = db,
        table = table)

  }
}
