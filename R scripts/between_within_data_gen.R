set.seed(1234)
sigmas <- get_sigmas(n_predictors = 1, ICC = 0.5, R2 = 0.7, int_pred_corr = 0.5)
data <- generate_continuous(n_studies = 100, study_sample_size = 50, n_predictors =1, sigmas = sigmas)

  haven::write_dta(data, "Results/int_pred_corr.dta")

  sigmas
