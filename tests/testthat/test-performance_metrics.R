test_that("evaluate_performance_continuous_generic", {
  perf = evaluate_performance_continuous_generic(
    test_data = generate_cbcl(),
    model = model_lm_cbcl_test( data = generate_cbcl()),
    predict_function = predict_default)

  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 4)

})

test_that("evaluate_performance_continuous", {
  perf = evaluate_performance_continuous(test_data = generate_cbcl(),
                                         model = model_lm_cbcl_test( data = generate_cbcl()))
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 4)

  int_data = generate_cbcl()
  int_data$int_est = TRUE
  test_data = generate_cbcl()
  test_data$int_est = FALSE
  test_data <- rbind(int_data, test_data)

  perf = evaluate_performance_continuous_new_studies(test_data = test_data,
                                         model = model_lm_cbcl_test( data = generate_cbcl()))
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 4)


})


test_that("evaluate_performance_binary", {
  perf = evaluate_performance_binary(test_data = generate_cbcl(),
                                         model = model_logistic_cbcl_test( data = generate_cbcl()))
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 3)
})



