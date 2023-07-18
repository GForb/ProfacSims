test_that("generate_cbcl", {
  data <- generate_cbcl()
  expect_equal(nrow(data), 2768)
  expect_equal(ncol(data), 17)
})


test_that("generate_continuous", {
  sigmas <- get_sigmas(1, 1/12, 12, 0.3, 0.7)
  train_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigma_e = sigmas$e,
    sigma_u = sigmas$u
  )

  expect_equal(nrow(train_data), 500)

  sigmas <- get_sigmas(1, 1/12, 12, 0.3, 0.7)
  test_data <- generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigma_e = sigmas$e,
    sigma_u = sigmas$u,
    train_data = train_data
  )

  expect_equal(nrow(test_data), 500)
})
