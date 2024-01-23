test_that("set_stream_seed", {
  set_stream_seed(batch = 5, seed = 1234)
  a = runif(1)

  set_stream_seed(batch = 5, seed = 1234)
  b = runif(1)


  set_stream_seed(batch = 6, seed = 1234)
  c = runif(1)

  expect_equal(a, b)
  expect_false(b ==c)

})

