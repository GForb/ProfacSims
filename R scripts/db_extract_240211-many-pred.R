# Create files for qmd

library(tidyverse)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Connecting to database

db_table <- "sim_results_many_pred"
sim_name <- "240211-many-pred"
where = glue("sim_name = '{sim_name}'")

always_include <- "n_studies, ICC, R2, study_sample_size_train, model, metric, est, tau2, batch_no, n_predictors, beta_names"

save_folder <- here::here(glue("Results/Database-extracts/{sim_name}"))

if (!dir.exists(save_folder)) {
  dir.create(save_folder, recursive = TRUE)
  message("Directory created at: ", save_folder)
}


correct_model_spelling <- function(data) {
  data <- data |>
    mutate(
      model = case_when(model == "Random intercetp - ML" ~ "Random intercept - ML",
                        model == "Random intercetp - REML" ~ "Random intercept - REML",
                        TRUE ~ model))

}


db <- dbConnect(RSQLite::SQLite(), here("Results/Database/sim_results.db"))

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

