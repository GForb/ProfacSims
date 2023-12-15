
save_batch_results_db <- function(database_connection, table, results_folder, sim_name) {
  save_time = lubridate::now()
  filenames <- list.files(results_folder, full.names=FALSE)
  print(filenames)
  for (file in filenames){
    if(file != "readme.rtf"){
      print(file)

      data_name <- load(here::here(results_folder, file))
      data <- get(data_name)

      processed_data <- data |>
        get_results_df(sim_name = sim_name, filename = file) |>
        dplyr:: mutate(save_time = save_time)


      write_file_to_db(
        data = processed_data,
        database_connection = database_connection,
        table = table)
    }
  }
}


write_file_to_db <- function(data, database_connection, table) {
  DBI::dbWriteTable(conn = database_connection,
                    name = table,
                    value = data,
                    append = TRUE)
}

get_results_df <- function(sim_results, sim_name, filename) {
  try(sim_results <- sim_results |> mutate(intercept_est_sample_size = test_ss/n_studies_test))
  try(sim_name <- sim_results |>
        dplyr::rowwise() |>
        dplyr::mutate(rng_state = toString(rng_state)) |>
        dplyr::ungroup())

  sim_results |>
    dplyr::select(-model_function_list) |>
    dplyr::mutate(
      batch_file = filename, # to reverse toString use parse(text = a) |> eval()
      sim_name = sim_name,
    ) |>
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
    beta_rmse = sqrt(beta_mse),
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
  if(!is.null(attr(beta,"class"))){
    if(attr(beta,"class") ==  "coef.mer"){
      names <-  colnames(beta$studyid)
      beta <- as.numeric(beta$studyid[1,])
      names(beta) <- names
    }
  }
  return(beta)
}


