generate_mixed_times <- function(n, fixed_time, n_timepoints, start_time, end_time, gap, timepoint_sd)  {
  args <- as.list(environment())
  n_studies = length(n)
  for(arg in args){
    if(length(arg) != n_studies){
      stop("arguments must all be of the same length")
    }
  }

  meta_data <- data.frame(args)
  meta_data$studyid <- 1:n_studies
  data_list <- purrr::pmap(meta_data, generate_times)
  data <- do.call(rbind, data_list)
  return(data)
}

generate_times <- function(studyid, n, fixed_time, n_timepoints, start_time, end_time, gap,timepoint_sd) {
  if(fixed_time){
    generate_fixed_times(studyid, n, n_timepoints, start_time, end_time, gap, timepoint_sd)
  } else {
    generate_varying_times(studyid, n, n_timepoints, start_time, end_time, gap)
  }

}

generate_fixed_times <- function(studyid, n, n_timepoints, start_time, end_time, gap, timepoint_sd){
  gaps_from_start <- cumsum(rep(gap, (n_timepoints -1)))
  gaps_from_start <- c(0, gaps_from_start)
  timepoints = start_time + gaps_from_start
  timepoints[n_timepoints] <- end_time

  ID <- sort(rep(1:n, n_timepoints))
  time <- rep(timepoints, n) + rnorm(n*n_timepoints, 0, timepoint_sd)
  wave <- rep(1:n_timepoints, n)
  data.frame(studyid = studyid, ID, wave, time)

}

generate_varying_times <- function(studyid, n, n_timepoints, start_time, end_time, gap){
  gaps_from_start <- cumsum(rep(gap, (n_timepoints -1)))
  gaps_from_start <- c(0, gaps_from_start)

  start_times <- start_time + gaps_from_start
  end_times <- end_time + gaps_from_start
  data_list <- list()
  for(wave in 1:n_timepoints){
    data_list[[wave]] <- data.frame(
      studyid = studyid,
      ID = 1:n,
      wave = wave,
      time = runif(n = n, min =  start_times[wave], max = end_times[wave])
    )
  }
  do.call(rbind, data_list)
}



