test_that("evaluate_performance_continuous", {
  perf = evaluate_performance_continuous(test_data = generate_cbcl(),
                                         model = model_lm_cbcl_test( data = generate_cbcl()))
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 3)
})


test_that("evaluate_performance_binary", {
  perf = evaluate_performance_binary(test_data = generate_cbcl(),
                                         model = model_logistic_cbcl_test( data = generate_cbcl()))
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 3)
})

