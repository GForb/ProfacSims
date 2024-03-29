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



generate_continuous_new_studies <- function(n_studies, study_sample_size,  n_predictors = 12, sigmas, intercept_est_sample_size, intercepts_data=NULL, min_study_id = 1, predictor_intercepts = "study") {

  int <- NULL

    if(intercept_est_sample_size > 0) {
      int <- generate_continuous(
        n_studies = n_studies,
        study_sample_size = intercept_est_sample_size,
        n_predictors = n_predictors,
        sigmas = sigmas,
        intercepts_data = intercepts_data,
        min_study_id = min_study_id,
        predictor_intercepts = predictor_intercepts
      )

      int$int_est = TRUE
    }


  data <- generate_continuous(
    n_studies = n_studies,
    study_sample_size= study_sample_size,
    n_predictors=n_predictors,
    sigmas=sigmas,
    intercepts_data = int,
    min_study_id = min_study_id,
    predictor_intercepts = predictor_intercepts)

  data$int_est = FALSE
  if(intercept_est_sample_size > 0) {
    data <- dplyr::bind_rows(int, data)
  }
    return(data)
}



#' Title
#'
#' @param n_studies number of studies
#' @param study_sample_size sample size for all studies
#' @param n_predictors number of predictors
#' @param sigmas output from get_sigmas with error variances and other model terms
#' @param intercepts_data a vector contianing study intercepts. This can be used to generate data from existing studies
#' @param min_study_id the minimum study ID to use. When generating data for new studies this will need to exceed the ID used in train data.
#' @param predictor_intercepts Either 'study' or 'random'. If study, the intercepts passed to the function that generates the predictor data are study intercepts. If random the interceps passed are randomly generated (or existing preductor intercepts if intercept data is provided)
#' Use study when data is generated with a correlation betweens study interceps and predictos. Use random if data is generated with predictors correlated within study. If intercepts are iid accross studies this option will make no difference.
#'
#' @return returns a dataframe of simualated data with outcome y and predictors x1 ... x{n_predictors}
#' @export
#'
#' @examples
#'
#' I should refactor so n_predictos, and predictor interceps are part of sigmas. That way all arguments that are unchanged between reps, and between train and test data are contained in sigmas.
#'
generate_continuous <- function(n_studies, study_sample_size,  n_predictors = 12, sigmas, intercepts_data = NULL, min_study_id = 1, predictor_intercepts = "study") {
  #Needs args:  n_studies, sample_szies, icc, out_prop (binary)
  # Fixed args: R-squared, number of predictors
  sigma_e <- sigmas$e
  beta <- sigmas$beta_x
  sigma_u <- sigmas$u
  beta_int = sigmas$beta_int

  if(!is.null(sigmas$single_x)){
    predictor_intercepts <- "random"
  }

  total_n <- n_studies*study_sample_size
  if(is.null(intercepts_data)){
    study_intercepts <-  rnorm(n_studies, sd = sigma_u)
    intercepts <- data.frame(studyid = min_study_id:(n_studies+min_study_id-1), study_intercept = study_intercepts)
  } else {
    intercepts <- intercepts_data[,c("studyid", "study_intercept")] |> unique()
  }

  if(predictor_intercepts == "random") {
    if(is.null(intercepts_data$predictor_intercept)){
      if(is.null(sigmas$x_b)){
        intercepts$predictor_intercept <- rnorm(n_studies, sd = 1)
      } else {
        intercepts$predictor_intercept <- rnorm(n_studies, sd = sigmas$x_b)
      }
    } else {
      intercepts <- intercepts_data[,c("studyid", "study_intercept", "predictor_intercept")] |> unique()
    }
  }


  data <-  intercepts[rep(seq_len(nrow(intercepts)), study_sample_size), ]
  if(predictor_intercepts == "study") {
    pred_intercepts <- data$study_intercept
  } else if(predictor_intercepts == "random"){
    pred_intercepts <- data$predictor_intercept
  }
  if(is.null(sigmas$single_x)){
    predictors <- generate_predictors(
      n = total_n,
      n_predictors = n_predictors,
      intercepts = pred_intercepts,
      beta_int = beta_int)

    row.names(data) <- NULL


    data <- cbind(data, predictors)
    data$lp <- generate_linear_predictor(predictors, beta)
    data$error = rnorm(total_n, 0, sigma_e)
    data$y <- data$lp+data$study_intercept+data$error
  } else {
    predictors <- generate_single_predictor(
      n = total_n,
      outcome_intercepts = data$study_intercept,
      predictor_intercepts = data$predictor_intercept,
      beta_int = sigmas$beta_int,
      sigma_Xw = sigmas$x_w
    )
    data  <- cbind(data, predictors)
    data$y <- sigmas$beta_w*data$X_w + sigmas$beta_b*data$X_b + data$study_intercept + rnorm(total_n, sd = sigmas$e)
  }

  data$studyid <- factor(data$studyid)
  return(data)
}
attr(generate_continuous, "n_predictors") = 12

count_predictors <- function(data) {
  data |> dplyr::select(dplyr::starts_with("x", ignore.case = FALSE)) |> ncol()
}

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

# Total variance of a single predictor is 1/n_predictors
# This gives the total variance of predictors to be 1
# Formulas for beta_int and sigma2_err_x are derived form fromulas for r-squared and total variance
# For any predictor total var = 1/n_predictors. Therefore error var = 1/n_predictors=beta_int^
generate_predictors <- function(n, n_predictors, intercepts,  beta_int) {
  sigma2_err_x = 1
  X_error <- rnorm(n * n_predictors, sd = sqrt(sigma2_err_x))
  X <- matrix(X_error, nrow = n, ncol = n_predictors)
  X <- X + beta_int*intercepts
  colnames(X) <- paste0("x", 1:n_predictors)
  return(X)
}

generate_linear_predictor <- function( X, beta, intercept =0) {

  W_ <- rep(beta, ncol(X))
  lp <- X %*% W_ + intercept
  return(lp)
}



# For any predictor total var = 1/n_predictors. Therefore error var = 1/n_predictors=beta_int^
generate_single_predictor <- function(n, outcome_intercepts, predictor_intercepts,  beta_int, sigma_Xw) {
  n_studies = length(outcome_intercepts)
  study_sample_size = n/n_studies
  X_b <- beta_int*outcome_intercepts + predictor_intercepts

  between_data <- data.frame(
    X_b = X_b
  )

  X_w <- rnorm(n, sd = sigma_Xw)

  X <- data.frame(x1 = X_w + X_b,  X_w = X_w)
  dplyr::bind_cols(X, between_data)
}
