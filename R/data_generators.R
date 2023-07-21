generate_cbcl <- function(n = c(100, 100, 100, 100, 100, 158, 197)) {
  cbcl_studies <- list(n = n,
                       fixed_time = c(1,1,1, 0, 0,0,0 ),
                       n_timepoints =c(6,4,4,3,2,3,2),
                       start_time = c(8, 10, 11,5,6,2,6),
                       end_time =c(18, 25, 23,16,16,16,18),
                       gap = c(2,4,2,5,3,3,6),
                       timepoint_sd = c(0.5,0.2,0,0,0,0,0 ))

  data <- do.call(generate_mixed_times, cbcl_studies)
  data <- data |> dplyr::mutate(
    ID = paste(studyid, ID, sep = "_")
  )

  # getting ids
  IDs <- unique(data$ID)

  sigma <- matrix(c(1, 0.5, 0.5, 1), nrow=2)

  # Generating random effects
  rand_intercept <- MASS::mvrnorm(n = length(IDs), mu = c(0, 0), Sigma = sigma)
  colnames(rand_intercept) <- c("ri1" ,"ri2")
  rand_slope <- MASS::mvrnorm(n = length(IDs), mu = c(0, 0), Sigma = sigma)
  colnames(rand_slope) <- c("rs1", "rs2")
  person_data <- data.frame(ID = IDs, rand_intercept, rand_slope)

  data <- dplyr::left_join(data, person_data, by = "ID")

  # Data generating model
  data <- data |> dplyr::mutate(
    time_centered = (time - mean(time))/sd(time),
    lp_cbcl1 = studyid + 2*time_centered,
    lp_cbcl2 = studyid + 2*time_centered,
    error_cbcl1 =rnorm(nrow(data), 0, 2),
    error_cbcl2 =rnorm(nrow(data), 0, 2),
    cbcl = error_cbcl1 + lp_cbcl1 + ri1 + time_centered*rs1,
    cbcl2 = error_cbcl2 + lp_cbcl2 + ri2 + time_centered*rs2
  )



  data <- data |>
    dplyr::mutate(
      dplyr::across(
         .cols = c(ri1,rs1, lp_cbcl1, error_cbcl1, cbcl),
         .fns = ~((.x-mean(data$cbcl))/sd(data$cbcl))
       ),
       dplyr::across(
         .cols = c(ri2,rs2, lp_cbcl2, error_cbcl2, cbcl2),
         .fns = ~((.x-mean(data$cbcl2))/sd(data$cbcl2))
       ),
      studyid_raw = studyid,
      studyid = as.factor(studyid)
   )

  cutoff <- stats::quantile(data$cbcl, probs = 0.25)
  data$cbcl_bin <- as.integer(data$cbcl > cutoff)

  return(data)
}

get_sigmas <- function(sigma2_x = 1, n_predictors, ICC, R2) {
  if(ICC >= R2) {
    stop("R2 must be higher than the ICC")
  }
  R2_ <- (1-R2)/R2
  pred_var <- sigma2_x*n_predictors # calculate with beta = 1
  if(ICC == 0){
    sigma2_u = 0
    sigma2_e = R2_*pred_var
  } else {
    ICC_ = (1-ICC)/ICC
    sigma2_u <- R2_*pred_var/(ICC_- R2_)
    sigma2_e <- ICC_*sigma2_u
  }
  total_var = sigma2_u + sigma2_e + pred_var
  scale = 1/(total_var)
  sigma2_u <- sigma2_u*scale
  sigma2_e <- sigma2_e*scale
  beta <- sqrt(scale)
  return(list(u = sqrt(sigma2_u), e = sqrt(sigma2_e), beta = beta))
}

generate_continuous <- function(n_studies, study_sample_size,  n_predictors = 12, sigmas, train_data = NULL) {
  #Needs args:  n_studies, sample_szies, icc, out_prop (binary)
  # Fixed args: R-squared, number of predictors
  sigma_e <- sigmas$e
  beta <- sigmas$beta
  sigma_u <- sigmas$u

  total_n <- n_studies*study_sample_size
  if(is.null(train_data)){
    study_intercepts <-  rnorm(n_studies, sd = sigma_u)
    intercepts <- data.frame(studyid = 1:n_studies, study_intercept = study_intercepts)
  } else {
    intercepts <- train_data[,c("studyid", "study_intercept")] |> unique()
  }

  data <-  intercepts[rep(seq_len(nrow(intercepts)), study_sample_size), ]

  predictors <- generate_predictors(total_n, n_predictors)
  data <- cbind(data, predictors)
  data$lp <- generate_linear_predictor(predictors, beta)
  data$error = rnorm(total_n, 0, sigma_e)
  data$y <- data$lp+data$study_intercept+data$error
  data$studyid <- factor(data$studyid)
  return(data)
}
attr(generate_continuous, "n_predictors") = 12

generate_binary <- function(n) {
  n_predictors <- 12
  tau <- 1
  error_sd <- 1
  beta <- 1
  n_studies <- length(n)
  total_n <- sum(n)

  study_intercepts <-  rnorm(n_studies, sd = tau)
  intercepts <- data.frame(studyid = 1:n_studies, study_intercepts)
  studyid <- 1:n_studies
  data <- data.frame(studyid = rep(studyid, n))
  data <- dplyr::left_join(data, intercepts, by = "studyid")
  total_n <- sum(n)

  predictors <- generate_predictors(total_n, n_predictors)
  data <- cbind(data, predictors)
  data$lp <- generate_linear_predictor(predictors, beta)
  data$error = rnorm(total_n, 0, error_sd)
  data$y <- data$lp+data$study_intercept+data$error
  data$studyid <- factor(data$studyid )
  return(data)
}


generate_predictors <- function(n, parameters) {
  X <- rnorm(n * parameters)
  X <- matrix(X, nrow = n, ncol = parameters)
  colnames(X) <- paste0("x", 1:parameters)
  return(X)
}

generate_linear_predictor <- function( X, beta, intercept =0) {
  W_ <- rep(beta, ncol(X))
  lp <- X %*% W_ + intercept
  return(lp)
}

