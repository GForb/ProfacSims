merge_batch_results <- function(folder) {
  # Create results database

  all_results <- tibble::tibble()
  filenames <- list.files(folder, full.names=TRUE)
  print(filenames)
  for (file in filenames){
    print(file)
    load(file)
    all_results <- dplyr::bind_rows(all_results, sim_results)
  }
  return(all_results)
}
