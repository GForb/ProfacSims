#!/usr/bin/env Rscript

# 240206-lvl2-endo

args = commandArgs(trailingOnly=TRUE)

batch_no <- as.integer(args)
if(length(batch_no) ==0){
  batch_no <- 1
}
sim_name <- "lvl2_endo"

ProfacSims:::set_stream_seed(batch_no, seed =644646468)

print("Batch-no")
print(batch_no)
print("Random number for checking variation in seed:")
runif(1) # random number for chekcing that seed is correctly set.

sim_params <- list(
  nreps = 1,
  sim_rep_fun = list(ProfacSims:::sim_rep_continuous_new_test_studies),
  n_studies = c(4,8,16,32,64),
  model_function_list = list(list(ProfacSims:::model_lm_fixed_int,
                                  ProfacSims:::model_lm,
                                  ProfacSims:::model_lmm_random_int_reml,
                                  ProfacSims:::model_lmm_random_int_ml,
                                  ProfacSims:::model_lmm_lm_hausman,
                                  ProfacSims:::model_lmm_reml_x1bar,
                                  ProfacSims:::model_lmm_lm_hausman_xbar)),
  study_sample_size_train = c(50, 200, 1000), #
  ICC = c(0.05, 0.2),
  R2 = c(0.4),
  single_x = TRUE,
  b_w_ratio = c(1), #
  pred_icc = c(0.25, 0.5, 0.9),  #
  int_pred_corr = c(0, 0.2, 0.5),
  intercept_est_sample_size = list(c(200)),
  n_studies_test = 30,
  predictor_intercepts = "random",
  n_predictors = 1,
  study_sample_size_test = 5000
)



sigma_checks <- do.call(ProfacSims:::check_simulation_pipeline, sim_params)
sigma_checks |> print(n = 1000)

tictoc::tic()
results <- do.call(ProfacSims:::ipdma_simulation, sim_params)
tictoc::toc()

sim_no_sting <- formatC(batch_no, width = 4, format = "d", flag = "0")
results$batch_no <- batch_no
saveRDS(results, file = paste0("results", sim_no_sting, ".RDS"))

# Write to db:

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
      data = data,
      database_connection = con,
      table = sim_name)
    db_done = TRUE
  })
  if(!db_done) Sys.sleep(rnorm(1, 30, 5))
}


print("closing connection")

DBI::dbDisconnect(con)
