
save_batch_results_db <- function(database_connection, table, results_folder, sim_name) {

  filenames <- list.files(results_folder, full.names=FALSE)
  print(filenames)
  for (file in filenames){
    print(file)
    load(here::here(results_folder, file))
    results_df <- get_results_df(sim_results, sim_name = sim_name, filename = file)

    DBI::dbWriteTable(conn = database_connection,
                 name = table,
                 value = results_df,
                 append = TRUE)

  }
}

get_results_df <- function(sim_results, sim_name, filename) {
  sim_results |>
    dplyr::select(-model_function_list) |>
    dplyr::mutate(batch_file = filename, # to reverse toString use parse(text = a) |> eval()
          sim_name = sim_name,
          intercept_est_sample_size = test_ss/n_studies_test
          ) |>
    dplyr::rowwise() |>
    dplyr::mutate(rng_state = toString(rng_state)) |>
    dplyr::ungroup() |>
    process_betas() |>
    as.data.frame()
}

process_betas <- function(data) {
  data |>
  clean_betas() |>
  dplyr::rowwise() |>
  dplyr::mutate(
    beta_pred = list(unlist(betas)[grep("^[xX].*", names(unlist(betas)), value=FALSE)]),
    beta_mean_error = mean(unlist(beta_pred)-beta_x),
    beta_mse = mean((unlist(beta_pred)-beta_x)^2),
    beta_names = toString(names(betas)),
    betas = toString(betas),
    beta_pred = toString(beta_pred)
  ) |>
  dplyr::ungroup()
}

clean_betas <- function(data) {
  data$betas <- lapply(data$betas, clean_beta)
  return(data)
}

clean_beta <- function(beta) {
  beta <- beta
  try({
    beta <- as.numeric(beta$studyid[1,])
    names(beta) <- c("(Intercept)", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11",  "x12")
  }, silent = TRUE)
  return(beta)
}


