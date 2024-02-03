test_that("predict_random_int_blup", {
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                      data = data)

  pred_re <- predict(model, random.only = TRUE)
  my_re <- predict_random_int_blup(model, newdata = data)
  diff <- pred_re - my_re
  expect_equal(length(my_re), length(pred_re))

  expect_equal(sum(abs(diff)), 0, tolerance = 0.01)

})

test_that("predict_random_int_blup", {
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                      data = data)

  pred_re <- predict(model, random.only = TRUE)
  my_re <- predict_random_int_blup(model, newdata = data)
  diff <- pred_re - my_re
  expect_equal(length(my_re), length(pred_re))

  expect_equal(sum(abs(diff)), 0, tolerance = 0.01)

})

test_that("get_x_formula_text",{
  sigmas <- get_sigmas(n_predictors = 2, ICC = 0, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas, n_predictors = 2)

  x_text <- get_x_formula_text(data)
  expect_equal(x_text, "x1 + x2")
})


test_that("get_betas",{
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- model_lm_fixed_int(data)
  betas <- get_betas(model)
  expect_vector(betas)
})

test_that("phtest_glmer",{
  # Test is run against Stata results

  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 1, ICC = 0.05, R2 = 0.7, int_pred_corr = 0.2)

  data <- generate_continuous(n_studies = 100, study_sample_size = 100, n_predictor = 1, sigmas = sigmas)

  model <- lme4::lmer("x1~1 + (1|studyid)", data = data)
  performance::icc(model)

  rand_model <- model_lmm_random_int_reml(data)
  fixed_model <- model_lm_fixed_int(data)

  htest <- phtest_glmer(glmerMod = rand_model, glmMod = fixed_model)
  expect_true(round(htest$statistic[1,1], digits = 0) %in% c(64, 71))
  # there is a strnge effect whereby this sometimes gives different answers despite seed being set. Seems to be a datagen issue as can replicate output in stata.
  expect_equal(htest$parameter[1], c(df = 1))
})

test_that("model_hausman", {
  sigmas <- get_sigmas(n_predictors = 1, ICC = 0.05, R2 = 0.4, int_pred_corr = 0, b_w_ratio = 2, pred_icc = 0.5, single_x = TRUE)
  data <- generate_continuous(n_studies = 64, study_sample_size = 50, n_predictor = 1, sigmas = sigmas)

  rand_model <- model_lmm_random_int_reml(data)
  fixed_model <- model_lm_fixed_int(data)
  htest <- phtest_glmer(glmerMod = rand_model, glmMod = fixed_model)
  htest$p.value < 0.05
  model <- model_lmm_lm_hausman(data)
  model

})


test_that("model_lmm_reml_x1bar", {
  sigmas <- get_sigmas(n_predictors = 1, ICC = 0.05, R2 = 0.4, int_pred_corr = 0, b_w_ratio = 2, pred_icc = 0.5, single_x = TRUE)
  data <- generate_continuous(n_studies = 64, study_sample_size = 50, n_predictor = 1, sigmas = sigmas)


  model <- model_lmm_reml_x1bar(data)
  expect_true(attr(model, "x1_centered"))



})
