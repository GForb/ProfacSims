get_sigmas <- function(sigma2_x = 1, n_predictors, ICC, R2, int_pred_corr=0, pred_icc = 0) {
  if(ICC >= R2) {
    stop("R2 must be higher than the ICC")
  }

  error_vars <- get_error_vars(R2, ICC, int_pred_corr)
  sigma2_u = error_vars$sigma2_u
  sigma2_e = error_vars$sigma2_e
  pred_var = error_vars$pred_var

  int_R2 = int_pred_corr^2
  # variance of an individual x is used to give beta_int
  if(ICC==0){
    beta_int = 0
  } else{
    beta_int = sqrt(int_R2*sigma2_x/(sigma2_u*(1+int_R2)))
  }

  # the total variance of the sum of predictors is sum_var_x
  sum_var_x <- n_predictors*sigma2_x + n_predictors^2*beta_int^2*sigma2_u

  if(pred_icc !=0) {
    if(int_pred_corr !=0) stop("Either int_pred_corr is nonzero or pred_icc is nonzero")
    # Predictors are generated with individual variance sigma2_x, and study variance of beta_int^2.
    # Pred ICC is therefore beta_int^2/(sigma2_x+ beta_int^2). Rearranging gives the following
    # pred_icc cannot be used in conjuction wiht
    beta_int = sqrt(sigma2_x*pred_icc/(1-pred_icc))
    # the variance of random predictor intercepts is 1
    sum_var_x = n_predictors*sigma2_x + n_predictors^2*beta_int^2
  }

  # set beta_x to give the needed total predictor variance
  beta_x =  sqrt(pred_var/sum_var_x)


  return(list(u = sqrt(sigma2_u), e = sqrt(sigma2_e), beta_x = beta_x, beta_int = beta_int))
}

get_error_vars <- function(R2, ICC, int_pred_corr) {
  if(ICC >= R2) {
    stop("R2 must be higher than the ICC")
  }

  if(ICC ==0){
    sigmas <- get_error_vars_ICC0(R2)

  } else if(int_pred_corr ==0){
    sigmas <- get_error_vars_ICC(R2, ICC)
  } else {
    sigmas <- get_error_vars_int_pred_corr(R2, ICC, int_pred_corr)
  }


  return(sigmas)
}

get_error_vars_ICC <- function(R2, ICC) {
  R2_ <- R2/(1-R2)
  gamma <- (1-ICC)/ICC
  sigma2_u <- (1-R2)/gamma
  sigma2_e <- gamma*sigma2_u
  pred_var <- 1-(1+gamma)*sigma2_u

  return(list(sigma2_u=sigma2_u, sigma2_e=sigma2_e, pred_var=pred_var))
}

get_error_vars_ICC0 <- function(R2) {
  sigma2_u <- 0
  sigma2_e <- 1 - R2
  pred_var <- R2

  return(list(sigma2_u=sigma2_u, sigma2_e=sigma2_e, pred_var=pred_var))
}


get_error_vars_int_pred_corr <- function(R2, ICC, int_pred_corr) {

  # use legacy if it ever worked: sigmau + sigmae = 1, go forth from there. Or fix sigma beta x to be 1... Find better constraint.
  sigma2_u <- 1


  gamma <- (1-ICC)/ICC
  sigma2_e <- gamma*sigma2_u

  a <- 1-R2
  b <- 2*int_pred_corr*a
  c <- 1-R2-R2*gamma

  quad_root <- (-b + sqrt(b^2-4*a*c))/(2*a)
  pred_var <- quad_root^2
  total_var <- sigma2_e + sigma2_u + pred_var + 2*int_pred_corr*(sqrt(sigma2_u*pred_var))
  # Scale variance components by total variance - does this preserve things
  sigma2_e <- sigma2_e/total_var
  sigma2_u <- sigma2_u/total_var
  pred_var <- pred_var/total_var

  return(list(sigma2_u=sigma2_u, sigma2_e=sigma2_e, pred_var=pred_var))
}
