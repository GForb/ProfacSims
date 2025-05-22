# Create files for qmd

library(tidyverse)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Connecting to database

db_table <- "sim_results_many_pred_v2"
sim_name <- "240211-many-pred"
where = glue("sim_name = '{sim_name}'")

always_include <- "n_studies, ICC, R2, study_sample_size_train, model, metric, est, tau2, batch_no, n_predictors, beta_names, intercept_est_sample_size"

save_folder <- here::here(glue("Results/Database-extracts/{sim_name}"))

if (!dir.exists(save_folder)) {
  dir.create(save_folder, recursive = TRUE)
  message("Directory created at: ", save_folder)
}

db <- dbConnect(RSQLite::SQLite(), here("Results/Database/sim_results.db"))

# Count rows in table
dbGetQuery(db,
           glue("SELECT sim_name, COUNT(*) FROM {db_table} GROUP BY sim_name"))

dbGetQuery(db,
           glue("SELECT * FROM {db_table} WHERE 1 = 0"))


# Calib-Slope
calib_slope <-  DBI::dbGetQuery(db,
                  glue(
                    "SELECT {always_include}
                      FROM {db_table}
                      WHERE {where}  AND metric = 'calib_slope'
  ")
) |> correct_model_spelling()

saveRDS(calib_slope, file = here(save_folder, "calib_slope.RDS"))



DBI::dbDisconnect(db)

