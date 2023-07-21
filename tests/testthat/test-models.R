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

