

test_that("generate_fixed_times", {
  expect_equal(
    generate_fixed_times(1,2, 3, 0, 5, 2, 0) , data.frame(studyid = 1,
                                                          ID = c(rep(1, 3), rep(2, 3)),
                                                          wave = rep(1:3, 2),
                                                          time = rep(c(0, 2, 5), 2))
  )
  expect_equal(
    generate_fixed_times(1,100, 3, 0, 5, 2, 1) |> ncol() , 4
  )
  expect_equal(
    generate_fixed_times(1, 100, 3, 0, 5, 2, 1) |> nrow() , 300
  )
})

test_that("generate_varying_times", {
  expect_equal(
    generate_varying_times(1, 100, 3, 0, 5, 2) |> nrow(), 300
  )
  expect_equal(
    generate_varying_times(1, 100, 3, 0, 5, 2) |> ncol(), 4
  )
})

test_that("generate_times", {
  expect_equal(
    generate_times(1, 100, 1, 3, 0, 5, 2, 0) |> nrow(), 300
  )
  expect_equal(
    generate_times(1, 100, 0, 3, 0, 5, 2, 0) |> ncol(), 4
  )
})

test_that("generate_mixed_times", {



  cbcl_studies <- list(n = c(100, 200, 300),
                       fixed_time = c(1,0,0),
                       n_timepoints =c(2,3,2),
                       start_time = c(10, 5, 6),
                       end_time =c(18, 15, 12),
                       gap = c(2,3,4),
                       timepoint_sd = c(0,0,0))

  data <- do.call(generate_mixed_times, cbcl_studies)
  expect_equal(ncol(data), 4)
  expect_equal(nrow(data), 1400)

})
