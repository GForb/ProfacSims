# Create files for qmd

library(tidyverse)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Connecting to database
table = "sim_results_v5"
sim_name = "240124-pred-new-data"
where = glue("sim_name = '{sim_name}'")

save_folder <- here::here(glue("Results/Database-extracts/{sim_name}"))






db <- dbConnect(RSQLite::SQLite(), here("Results/Database/sim_results.db"))

# Var U
var_u <-  DBI::dbGetQuery(db,
    glue(
      "SELECT n_studies, ICC, R2, study_sample_size_train, model, predict_method, intercept_est_sample_size, metric, est, error_var_u, sigma_u, batch_no
      FROM {table}
      WHERE {where}  AND metric = 'var_u' AND (model ='Random intercetp - ML' OR model = 'Random intercetp - REML') AND predict_method = 'new0' AND intercept_est_sample_size = 10
  ")
) |> correct_model_spelling()

saveRDS(var_u, file = here(save_folder, "var_u.RDS"))

# random_models_dynamic <-  DBI::dbGetQuery(db,
#                                           glue(
#                                             "SELECT n_studies, ICC, R2, study_sample_size_train, model, predict_method, intercept_est_sample_size, metric, est, error_var_u, sigma_u, batch_no
#     FROM {table}
#     WHERE {where}  AND metric = 'var_u' AND (model ='Random intercetp - ML' OR model = 'Random intercetp - REML') AND predict_method = 'new_dynamic' AND intercept_est_sample_size = 10
#   ")
# )


# Calib-ITl
calib_itl <-  DBI::dbGetQuery(db,
                              glue(
                                "SELECT n_studies, ICC, R2, study_sample_size_train, model, predict_method, intercept_est_sample_size, metric, est, tau2, error_var_u, sigma_u, batch_no
    FROM {table}
    WHERE {where}  AND metric = 'calib_itl'
  ")
) |>
  correct_model_spelling() |>
  mutate(tau = sqrt(tau2))

saveRDS(calib_itl, file = here(save_folder, "calib_itl.RDS"))

calib_itl_stacked <- create_stacked_results(calib_itl)
saveRDS(calib_itl_stacked, file = here(save_folder, "calib_itl_stacked.RDS"))

# Calib-Slope
calib_slope <-  DBI::dbGetQuery(db,
                                glue(
                                  "SELECT n_studies, ICC, R2, study_sample_size_train, model, predict_method, intercept_est_sample_size, metric, est, tau2, error_var_u, sigma_u, batch_no
    FROM {table}
    WHERE {where}  AND metric = 'calib_slope'
  ")
) |>
  correct_model_spelling() |>
  mutate(tau = sqrt(tau2))

saveRDS(calib_slope, file = here(save_folder, "calib_slope.RDS"))


calib_slope_stacked <- create_stacked_results(calib_slope)
saveRDS(calib_slope_stacked, file = here(save_folder, "calib_slope_stacked.RDS"))

# R-squared

r_squared <- DBI::dbGetQuery(db,
  glue(
    "SELECT n_studies, ICC, R2, study_sample_size_train, model, predict_method, intercept_est_sample_size, metric, est, tau2, error_var_u, sigma_u, batch_no
    FROM {table}
    WHERE {where}  AND metric = 'r-squared'
  ")
) |>
  correct_model_spelling() |>
  mutate(tau = sqrt(tau2))

saveRDS(r_squared, file = here(save_folder, "r_squared.RDS"))


r_squared_stacked <- create_stacked_results(r_squared)
saveRDS(r_squared_stacked, file = here(save_folder, "r_squared_stacked.RDS"))


DBI::dbDisconnect(db)

