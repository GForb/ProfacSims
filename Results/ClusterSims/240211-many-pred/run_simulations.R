#!/usr/bin/env Rscript

# 240211-many-pred

args = commandArgs(trailingOnly=TRUE)



batch_no <- as.integer(args)
if(length(batch_no) ==0){
  batch_no <- 1
}
sim_name <- "many-pred"

ProfacSims:::set_stream_seed(batch_no, seed =644646468)

print("Batch-no")
print(batch_no)
print("Random number for checking variation in seed:")
runif(1) # random number for chekcing that seed is correctly set.

sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(16),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml)),
  study_sample_size_train = c(50), #
  ICC = c(0.05),
  R2 = c(0.3),
  intercept_est_sample_size = list(c(200)),
  n_studies_test = 30,
  predictor_intercepts = "random",
  n_predictors = c(30),
  study_sample_size_test = 5000
)

#sigma_checks <- do.call(ProfacSims:::check_simulation_pipeline, sim_params)
#sigma_checks |> print(n = 1000)

tictoc::tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
tictoc::toc()

sim_no_sting <- formatC(batch_no, width = 4, format = "d", flag = "0")
results$batch_no <- batch_no
saveRDS(results, file = paste0("results", sim_no_sting, ".RDS"))

# Write to db:

print("processing data")
processed_data <- results |>
  ProfacSims:::get_results_df(sim_name = sim_name, filename = paste0("results", sim_no_sting, ".RDS"))

print("connecting to database")
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  glue::glue("{sim_name}.db")
)

print("saving to database")

db_done = FALSE
attempt = 1
while(!db_done){
  print(glue::glue("Attempt {attempt} to write to db"))
  attempt = attempt + 1
  try({
    ProfacSims:::write_file_to_db(
      data = processed_data,
      database_connection = con,
      table = sim_name)
    db_done = TRUE
  })
  if(!db_done) Sys.sleep(rnorm(1, 30, 5))
}


print("closing connection")

DBI::dbDisconnect(con)
