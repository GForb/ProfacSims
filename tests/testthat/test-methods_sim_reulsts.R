test_that("prep_data_for_plot", {

  # Sim results from db
  db <- DBI::dbConnect(RSQLite::SQLite(), here::here("Results/Database/sim_results.db"))
  data <- DBI::dbGetQuery(db,
                     "SELECT n_studies, ICC, R2, study_sample_size_train ,test_ss, int_pred_corr, model, est, tau2, sigma_e, sigma_u, intercept_est_sample_size, metric, n_studies_test, rep_number
            FROM sim_results_v1
            WHERE sim_name = 'BatchAug2023' AND metric = 'calib_itl' AND model = 'Fixed intercept' AND ICC = 0 AND R2 = 0.4 AND test_ss = 300 AND int_pred_corr = 0.0 AND n_studies = 4 AND study_sample_size_train = 50"
  )

  prepped_data <- data |> prep_data_for_plot()
  expect_equal(prepped_data$n, rep(200, nrow(prepped_data)))
})




test_that("sim_results_lazy_stack", {

  # Sim results from db
  db <- DBI::dbConnect(RSQLite::SQLite(), here::here("Results/Database/sim_results.db"))
  data <- DBI::dbGetQuery(db,
                     "SELECT n_studies, ICC, R2, study_sample_size_train ,test_ss, int_pred_corr, model, est, tau2, sigma_e, sigma_u, intercept_est_sample_size, metric, n_studies_test, rep_number, error_var_u
            FROM sim_results_v1
            WHERE sim_name = 'BatchAug2023' AND model = 'Fixed intercept' AND ICC = 0 AND R2 = 0.4 AND test_ss = 300 AND int_pred_corr = 0.0 AND n_studies = 4 AND study_sample_size_train = 50"
  )

  stacked <- data |> sim_results_lazy_stack()
  expect_equal(nrow(stacked), 1200)
})
