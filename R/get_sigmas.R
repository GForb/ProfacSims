get_sigmas<- function(sigma2_x = 1, n_predictors, ICC, R2, int_pred_corr=0, pred_icc = 0, single_x = FALSE, b_w_ratio = NULL){
  if(!single_x){
    return(get_sigmas_default(sigma2_x, n_predictors, ICC, R2, int_pred_corr, pred_icc))
  } else {
    return(get_sigmas_single_x(int_pred_corr = int_pred_corr, icc_x = pred_icc, R2 = R2, ICC = ICC, b_w_ratio = b_w_ratio ))
  }
}


get_sigmas_default <- function(sigma2_x = 1, n_predictors, ICC, R2, int_pred_corr=0, pred_icc = 0) {
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

get_pred_between_var <- function(int_pred_corr, icc_x) {
  if(icc_x <int_pred_corr^2){stop("Predictor ICC must be greater than int_pred_corr^2")}
  sigma2_Xb <- (icc_x-int_pred_corr^2)/(1-icc_x)
  return(sigma2_Xb)
}

get_beta_int <- function(int_pred_corr, sigma2_u, sigma2_Xb){
  # This function works assuming the within-study variation in predictor is equal to 1
  beta_int2 <- int_pred_corr^2*(1+ sigma2_Xb)/(sigma2_u*(1-int_pred_corr^2))
  return(sqrt(beta_int2))
}

check_icc_x <- function(beta_int, sigma2_Xb, icc_x) {
  icc_x - (sigma2_Xb+beta_int^2)/(1+sigma2_Xb+beta_int^2)
}

check_int_pred_corr  <- function(int_pred_corr, beta_int, sigma2_Xb) {
  int_pred_corr-sqrt(beta_int^2/(beta_int^2+sigma2_Xb+1))
}


get_sigmas_single_x <- function(int_pred_corr, icc_x, R2, ICC, b_w_ratio){
  sigma2_Xw <- 1
  sigma2_u <- 1
  sigma2_Xb <- get_pred_between_var(int_pred_corr = int_pred_corr, icc_x = icc_x)
  beta_int <- get_beta_int(int_pred_corr = int_pred_corr, sigma2_u = sigma2_u, sigma2_Xb = sigma2_Xb)
  sigma2_e <- get_error_vars_single_x(ICC)

  sigma2_u <- 1

  beta_b = get_beta_b(b_w_ratio =b_w_ratio,  R2 = R2, sigma2_e =sigma2_e, sigma2_Xb = sigma2_Xb, beta_int = beta_int )
  beta_w = beta_b*b_w_ratio
  return(list(u = sqrt(sigma2_u),
              e = sqrt(sigma2_e),
              x_w = sqrt(sigma2_Xw),
              x_b=sqrt(sigma2_Xb),
              beta_int = beta_int,
              beta_b = beta_b,
              beta_w = beta_w,
              single_x = TRUE))
}

get_error_vars_single_x <- function(ICC) {
  #var_b = beta_int^2 + sigma2_Xb
  #sigma2_e = (1 + b_w_ratio*sigma2_Xb + sigma2_Xb*var_b)*(1-R2)/R2
  sigma2_e <- (1-ICC)/ICC

  return(sigma2_e= sigma2_e)
}

get_beta_b <- function(b_w_ratio, R2, sigma2_e, sigma2_Xb, beta_int){
  var_b = beta_int^2 + sigma2_Xb
  beta_b <- sqrt((R2 + R2*sigma2_e - 1)/((1-R2)*(b_w_ratio^2+var_b)))
}


