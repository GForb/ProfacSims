run_simualtions_in_batches <- function(sim_params, n_batches) {
  file_names = ""
  for(i in 1:n_batches){

    plan("multisession")


    sim_results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
    file_name <- paste("sim_results", i, ".RDS", sep = "")
    saveRDS(sim_results, file = here::here("Results/HolidayBatch", file_name))


    print(paste("Batch", i, "complete"))
    rm(list = ls(all.names = TRUE))
    gc()
  }
}

