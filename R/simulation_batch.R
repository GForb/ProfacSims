
run_simualtions_in_batches <- function(sim_params, n_batches, filepath) {
  file_names = ""
  for(i in 1:n_batches){

    plan("multisession")


    sim_results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
    file_name <- paste("sim_results", i, sep = "")
    save(sim_results, file = here::here(filepath, file_name))


    print(paste("Batch", i, "complete"))
    rm(list = ls(all.names = TRUE))
    gc()
  }
  sim_results_combined <- load(file = here::here("Results/HolidayBatch", 'sim_results1'))

  for(i in 2:n_batches){
    sim_results <- load(file = here::here(filepath, paste("sim_results", i, sep = "")))
    sim_results <- sim_results |> mutate(batch_no = i)
    sim_results_combined <- dplyr::bind_rows(sim_results_combined, sim_results)
  }
  save(sim_results_combined, file = here::here(filepath, "combined_results"))
  return(sim_results_combined)
}

