library(tictoc)

sigmas <-  get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

tic()
sim_results <- sim_rep_continuous(
  list(model_lmm_random_int_ml, model_lm),
  n_studies = 10,
  study_sample_size_train = 500,
  study_sample_size_test = 5000,
  sigmas = sigmas
)
toc()

tic()
  sim_results <- sim_rep_continuous_new_test_studies(
    list(model_lmm_random_int_ml, model_lm, model_lm_fixed_int),
    n_studies = 10,
    study_sample_size_train = 500,
    study_sample_size_test = 5000,
    sigmas = sigmas,
    intercept_est_sample_size = 10,
    n_studies_test = 64
  )
toc()

tic()
sim_results <- sim_rep_continuous_new_test_studies(
  list(model_lmm_random_int_ml, model_lm, model_lm_fixed_int),
  n_studies = 10,
  study_sample_size_train = 500,
  study_sample_size_test = 5000,
  sigmas = sigmas,
  intercept_est_sample_size = 100,
  n_studies_test = 100
)
toc()


tic()
sim_results <- sim_rep_continuous(
  list(model_lmm_random_int_ml, model_lm),
  n_studies = 100,
  study_sample_size_train = 500,
  study_sample_size_test = 5000,
  sigmas = sigmas
)
toc()


tic()
sim_results <- sim_rep_continuous_new_test_studies(
  list(model_lmm_random_int_ml),
  n_studies = 10,
  study_sample_size_train = 500,
  study_sample_size_test = 5000,
  sigmas = sigmas,
  intercept_est_sample_size = 100,
  n_studies_test = 100
)
toc()

tic()
sim_results <- sim_rep_continuous_new_test_studies(
  list(model_lm),
  n_studies = 10,
  study_sample_size_train = 500,
  study_sample_size_test = 5000,
  sigmas = sigmas,
  intercept_est_sample_size = 100,
  n_studies_test = 100
)
toc()

