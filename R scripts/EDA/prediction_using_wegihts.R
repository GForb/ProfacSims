# lme with small weights

model_lmm_random_int_reml

sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
train_data <- generate_continuous(
  n_studies = 10,
  study_sample_size = 50,
  sigmas = sigmas
) |> mutate(weight = 1)



test_data <- generate_continuous(
  n_studies = 10,
  study_sample_size = 50,
  sigmas = sigmas
) |> mutate(weight = 0.00000001)

all_data <- bind_rows(train_data, test_data)

model_1 <- model_lmm_random_int_reml(train_data)

model_2 <- model_lmm_random_int_reml_weight(all_data)



model_lmm_random_int_reml(all_data)
