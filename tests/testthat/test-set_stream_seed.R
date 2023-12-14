test_that("get_stream_seed", {
  # I have used tempfiles here as when I did not the test failed as the seed didn't change. It appears that when .Random.seed is assigned inside a function something odd happens.
  set.seed(seed = 1234, kind = "L'Ecuyer-CMRG")
  tmp <- tempfile()
  seed <- get_stream_seed(1, seed = 1234)
  save(seed, file = tmp)

  tmp2 <- tempfile()
  seed2 <- get_stream_seed(200, seed = 1234)
  save(seed2, file = tmp2)

  .Random.seed <- load(tmp)
  n1 <- rnorm(1)

  .Random.seed <- load(tmp2)
  n2<- rnorm(1)

  set.seed(seed = 1234, kind = "L'Ecuyer-CMRG")
  .Random.seed <- get_stream_seed(1, seed = 1234)
  n3 <- rnorm(1)

  expect_equal(n1, n3)
  expect_false(n1 == n2)

})

