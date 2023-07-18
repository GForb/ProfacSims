sigmas <- get_sigma2s(sigma2_x = 1,
                      beta = 1/sqrt(12),
                      n_predictors = 12,
                      ICC = 0.3,
                      R2 = 0.7)

sim_results <- sim_rep_continuous(
  list(model_lm_fixed_int, model_lm),
  n_studies = 10,
  study_sample_size_train = 50,
  study_sample_size_test = 5000,
  sigma_e = sqrt(sigmas["sigma2_e"]),
  sigma_u = sqrt(sigmas["sigma2_u"])
)


inv <- sim_results <- sim_rep_continuous_inv(
  list(model_lm_fixed_int, model_lm),
  n_studies = 10,
  study_sample_size_train = 5000,
  study_sample_size_test = 5000,
  sigma_e = sqrt(sigmas["sigma2_e"]),
  sigma_u = sqrt(sigmas["sigma2_u"]),
  sim_rep = sim_results
)

model1 <- inv$models[[1]]
summary(model1)


test_study1 <- inv$test_data |> dplyr::filter(studyid ==1)
pred <- predict(model1, newdata = test_study1)

data <- data.frame(predicted_lp = pred, observed_outcome = test_study1$y)
rsq_oosse(data)
rsq(data)

model2 <- inv$models[[2]]
summary(model2)


inv$results[[1]]$by_study |> dplyr::filter(metric == "r-squared")
1/(1+sigmas["sigma2_e"])


get_var <- function(x) {
  data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigma_e = sqrt(sigmas["sigma2_e"]),
    sigma_u = sqrt(sigmas["sigma2_u"])
  )
  var(data$y)


}

sapply(1:100, get_var) |> mean()
1 + sigmas["sigma2_e"]


# Maximum possible performance is less than actual performance.

# Simualtion pipeline:
# repeat sims function
# take as args n_reps, + simulation paramaters
# then expand_grid + pmap.

