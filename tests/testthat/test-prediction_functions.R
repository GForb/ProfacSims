
test_that("predict_rand_int", {
  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                      data = data)

  pred_re <- lme4::ranef(model)
  my_re <- predict_rand_int(model, newdata = data)
  diff <- pred_re$studyid - my_re$pred_intercept
  expect_equal(length(pred_re$studyid [[1]]), length(my_re$pred_intercept))

  expect_equal(sum(abs(diff)), 0, tolerance = 0.0001)

  my_re <- predict_rand_int(model, newdata = data)
  diff <- pred_re$studyid - my_re$pred_intercept
  expect_equal(sum(abs(diff)), 0, tolerance = 0.0001)


})




test_that("get_x_prediction", {
  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- lm("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12",
              data = data)

 pred <- predict(model)
 fixed_pred <- pred - model$coef["(Intercept)"]

  pred_x <- get_x_prediction(model, newdata = data)
  diff <- sum(abs(pred_x - fixed_pred))
  expect_equal(diff, 0, tol = 0.0001)


})

test_that("get_fixed_int_offset", {
  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- lm("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+studyid",
                      data = data)

  my_re <- get_fixed_int_offset(model, newdata = data, cluster_var = "studyid")
  expect_equal(nrow(my_re), 10)
  expect_equal(ncol(my_re), 2)

  my_re <- predict_intercepts(model, newdata = data, cluster_var = "studyid")
  expect_equal(nrow(my_re), 10)
  expect_equal(ncol(my_re), 2)


})

test_that("predict_fixed", {
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  train_data <-  generate_continuous(n_studies = 10, study_sample_size =100, n_predictors = 12, sigmas = sigmas)

  rand_model <- model_lmm_random_int_ml(train_data)
  fixed_model <- model_lm_fixed_int(train_data)
  lm_model <- model_lm(train_data)

  test_data_existing <- generate_continuous(n_studies = 10, study_sample_size =  100, n_predictors = 12, sigmas = sigmas, intercepts_data = train_data)
  test_data_new <- test_data_new <- generate_continuous_new_studies(
    sigmas = sigmas,
    n_studies = 10,
    study_sample_size = 100,
    intercepts_data = train_data,
    min_study_id = 11,
    n_predictors = 12,
    intercept_est_sample_size = 50)

  test_data_new_test_only <- test_data_new |> dplyr::filter(int_est == FALSE)

  # Creating data with different studies but the same xs
  new_test_data <- dplyr::bind_cols(
    test_data_existing[grep("x\\d+", names(test_data_existing))],
    test_data_new_test_only[c("y", "studyid")])

  # Check number of rows
  expect_equal(predict_fixed(rand_model, test_data_existing) |> length(), nrow(test_data_existing))
  expect_equal(predict_fixed(fixed_model, test_data_existing) |> length(), nrow(test_data_existing))
  expect_equal(predict_fixed(lm_model, test_data_existing) |> length(), nrow(test_data_existing))

  # Tests that changing the outcome and study ids doen't change preidctions - as predictions should be dependent on fixed only.
  expect_equal(predict_fixed(rand_model, test_data_existing), predict_fixed(rand_model, new_test_data))
  expect_equal(predict_fixed(fixed_model, test_data_existing), predict_fixed(fixed_model, new_test_data))
  expect_equal(predict_fixed(lm_model, test_data_existing), predict_fixed(lm_model, new_test_data))

  expect_false(isTRUE(all.equal(predict_fixed(rand_model, test_data_existing), predict_fixed(rand_model, test_data_new_test_only))))
  expect_false(isTRUE(all.equal(predict_fixed(fixed_model, test_data_existing), predict_fixed(fixed_model, test_data_new_test_only))))
  expect_false(isTRUE(all.equal(predict_fixed(lm_model, test_data_existing), predict_fixed(lm_model, test_data_new_test_only))))

})

test_that("predict_ml", {
  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  train_data <-  generate_continuous(n_studies = 10, study_sample_size =100, n_predictors = 12, sigmas = sigmas)
  model <- model_lm_fixed_int(data = train_data)
  model_intercepts <- c(0,model$coefficients[2:10])
  pred_intercepts <- predict_intercept_ml(model, train_data, cluster_var = "studyid")
  diff <- sum(abs(pred_intercepts$pred_intercept - model_intercepts))
  expect_equal(diff, 0, tolerance = 0.0001)


})


test_that("predict_default", {
  # I want this to return a prediction with intercepts estimated from training data
  # To test this I need to make a prediction, subtract the fixed part of the prediction and intercetp, this will give the study specific intercept
  # Check that the results are the same for training data as they are for

  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  train_data <-  generate_continuous(n_studies = 10, study_sample_size =100, n_predictors = 12, sigmas = sigmas)
  test_data_existing <- generate_continuous(n_studies = 10, study_sample_size =  100, n_predictors = 12, sigmas = sigmas, intercepts_data = train_data)

  rand_model <- model_lmm_random_int_ml(train_data)
  fixed_model <- model_lm_fixed_int(train_data)
  lm_model <- model_lm(train_data)

  expect_equal(predict_default(rand_model, train_data) - predict_fixed(rand_model, train_data), predict_default(rand_model, test_data_existing) - predict_fixed(rand_model, test_data_existing))
  expect_equal(predict_default(fixed_model, train_data) - predict_fixed(fixed_model, train_data), predict_default(fixed_model, test_data_existing) - predict_fixed(fixed_model, test_data_existing))
  expect_equal(predict_default(lm_model, train_data) - predict_fixed(lm_model, train_data), predict_default(lm_model, test_data_existing) - predict_fixed(lm_model, test_data_existing))

})

test_that("predict_average_intercept", {
  # I want this to return a prediction with intercepts estimated from training data
  # To test this I need to make a prediction, subtract the fixed part of the prediction and intercetp, this will give the study specific intercept
  # Check that the results are the same for training data as they are for
  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  train_data <-  generate_continuous(n_studies = 10, study_sample_size =100, n_predictors = 12, sigmas = sigmas)
  test_data_existing <- generate_continuous(n_studies = 10, study_sample_size =  100, n_predictors = 12, sigmas = sigmas, intercepts_data = train_data)

  rand_model <- model_lmm_random_int_ml(train_data)
  fixed_model <- model_lm_fixed_int(train_data)
  lm_model <- model_lm(train_data)

  # test I get the right number of rows
  expect_equal(predict_average_intercept(rand_model, test_data_existing) |> length(), nrow(test_data_existing))
  expect_equal(predict_average_intercept(fixed_model, test_data_existing) |> length(), nrow(test_data_existing))
  expect_equal(predict_average_intercept(lm_model, test_data_existing) |> length(), nrow(test_data_existing))

  expect_equal(unname(predict_average_intercept(rand_model, train_data) - predict_fixed(rand_model, train_data)), rep(0, 1000))  # predict fixed include intercept for random models but not for not random models
  expect_equal(unname(predict_average_intercept(fixed_model, train_data) - predict_fixed(fixed_model, train_data)),c(0,fixed_model$coefficients[grep("studyid\\d+", names(fixed_model$coefficients))]) |>  mean() |> rep(1000))
  expect_equal(unname(predict_average_intercept(lm_model, train_data) - predict_fixed(lm_model, train_data)), rep(0, 1000))

  expect_equal(unname(predict_average_intercept(rand_model, test_data_existing) - predict_fixed(rand_model, test_data_existing)), rep(0, 1000))  # predict fixed include intercept for random models but not for not random models
  expect_equal(unname(predict_average_intercept(fixed_model, test_data_existing) - predict_fixed(fixed_model, test_data_existing)),c(0,fixed_model$coefficients[grep("studyid\\d+", names(fixed_model$coefficients))]) |>  mean() |> rep(1000))
  expect_equal(unname(predict_average_intercept(lm_model, test_data_existing) - predict_fixed(lm_model, test_data_existing)), rep(0, 1000))

  expect_equal(unname(predict_average_intercept(rand_model, test_data_existing) - predict_fixed(rand_model, test_data_existing)), rep(0, 1000))  # predict fixed include intercept for random models but not for not random models
  expect_equal(unname(predict_average_intercept(fixed_model, test_data_existing) - predict_fixed(fixed_model, test_data_existing)),c(0,fixed_model$coefficients[grep("studyid\\d+", names(fixed_model$coefficients))]) |>  mean() |> rep(1000))
  expect_equal(unname(predict_average_intercept(lm_model, test_data_existing) - predict_fixed(lm_model, test_data_existing)), rep(0, 1000))

  test_data_new <- test_data_new <- generate_continuous_new_studies(
    sigmas = sigmas,
    n_studies = 10,
    study_sample_size = 100,
    intercepts_data = train_data,
    min_study_id = 11,
    n_predictors = 12,
    intercept_est_sample_size = 50)

  test_data_new_test_only <- test_data_new |> dplyr::filter(int_est == FALSE)

  expect_equal(unname(predict_average_intercept(rand_model, test_data_new_test_only) - predict_fixed(rand_model, test_data_new_test_only)), rep(0, 1000))  # predict fixed include intercept for random models but not for not random models
  expect_equal(unname(predict_average_intercept(fixed_model, test_data_new_test_only) - predict_fixed(fixed_model, test_data_new_test_only)),c(0,fixed_model$coefficients[grep("studyid\\d+", names(fixed_model$coefficients))]) |>  mean() |> rep(1000))
  expect_equal(unname(predict_average_intercept(lm_model, test_data_new_test_only) - predict_fixed(lm_model, test_data_new_test_only)), rep(0, 1000))

})



test_that("predict_new_intercept", {

  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  train_data <-  generate_continuous(n_studies = 10, study_sample_size =100, n_predictors = 12, sigmas = sigmas)

  rand_model <- model_lmm_random_int_ml(train_data)
  fixed_model <- model_lm_fixed_int(train_data)
  lm_model <- model_lm(train_data)

  test_data_new  <- generate_continuous_new_studies(
    sigmas = sigmas,
    n_studies = 10,
    study_sample_size = 100,
    intercepts_data = train_data,
    min_study_id = 11,
    n_predictors = 12,
    intercept_est_sample_size = 50)

  test_data_new_test_only <- test_data_new |> dplyr::filter(!int_est)

  test_data_new2 <-  generate_continuous_new_studies(
    sigmas = sigmas,
    n_studies = 10,
    study_sample_size = 100,
    intercepts_data = train_data,
    min_study_id = 11,
    n_predictors = 12,
    intercept_est_sample_size = 50)

  test_data_new_2_test_only <- test_data_new2 |>
    dplyr::filter(!int_est)
  test_data_new2 <- test_data_new_2_test_only |>
    dplyr::bind_rows(test_data_new |> dplyr::filter(int_est))

  expect_equal(predict_with_new_intercept_data(rand_model, test_data_new) |> length(), 1000)
  expect_equal(predict_with_new_intercept_data(fixed_model, test_data_new) |> length(), 1000)
  expect_equal(predict_with_new_intercept_data(lm_model, test_data_new) |> length(), 1000)

  expect_equal(test_data_new2 |> dplyr::filter(int_est), test_data_new |> dplyr::filter(int_est))
  expect_equal(predict_with_new_intercept_data(rand_model, test_data_new) - predict_fixed(rand_model, test_data_new_test_only), predict_with_new_intercept_data(rand_model, test_data_new2) - predict_fixed(rand_model, test_data_new_2_test_only))
  expect_equal(predict_with_new_intercept_data(fixed_model, test_data_new) - predict_fixed(fixed_model, test_data_new_test_only), predict_with_new_intercept_data(fixed_model, test_data_new2) - predict_fixed(fixed_model, test_data_new_2_test_only))
  expect_equal(predict_with_new_intercept_data(lm_model, test_data_new) - predict_fixed(lm_model, test_data_new_test_only), predict_with_new_intercept_data(lm_model, test_data_new2) - predict_fixed(lm_model, test_data_new_2_test_only))

})

test_that("predict_with_new_intercept_data", {

  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

  train_data <-  generate_continuous(n_studies = 10, study_sample_size =100, n_predictors = 12, sigmas = sigmas)
  test_data <- train_data
  test_data$int_est <-  FALSE
  intercept_data <- train_data
  intercept_data$int_est <- TRUE
  test_data <- dplyr::bind_rows(test_data, intercept_data)
  model_fixed <- model_lm_fixed_int(data = train_data)
  pred1 <- predict(model_fixed)
  pred2 <- predict_with_new_intercept_data(model_fixed, test_data = test_data)

  diff_fixed <- abs(pred1 -pred2)
  expect_equal(sum(diff_fixed), 0, tolerance = 0.001)


  model_random <- model_lmm_random_int_reml(data = train_data)
  pred1 <- predict(model_random)
  pred2 <- predict_with_new_intercept_data(model_random, test_data = test_data)

  diff_random<- abs(pred1 -pred2)
  expect_equal(sum(diff_random), 0, tolerance = 0.001)


})

test_that("predict_with_new_intercept_data x1 centred", {

  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 1, ICC = 0.05, R2 = 0.4, single_x = TRUE, int_pred_corr = 0, pred_icc = 0.9, b_w_ratio = 2)

  train_data <-  generate_continuous(n_studies = 64, study_sample_size =100, n_predictors = 1, sigmas = sigmas)
  test_data <- train_data
  test_data$int_est <-  FALSE
  intercept_data <- train_data
  intercept_data$int_est <- TRUE
  test_data <- dplyr::bind_rows(test_data, intercept_data)

  model_random <- model_lmm_reml_x1bar(data = train_data)
  pred1 <- predict_with_new_intercept_data(model_random, test_data = test_data)
  expect_equal(length(pred1), 6400)

  model_h <- model_lmm_lm_hausman_xbar(test_data)
  pred2 <- predict_with_new_intercept_data(model_random, test_data = test_data)
  expect_equal(length(pred2), 6400)

})

