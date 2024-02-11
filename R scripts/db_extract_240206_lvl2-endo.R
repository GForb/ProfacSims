# Create files for qmd

library(tidyverse)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Connecting to database
db_table <- "sim_results_lvl2_endo"
sim_name <- "240206-lvl2-endo"
where = glue("sim_name = '{sim_name}'")

always_include <- "n_studies, ICC, R2, study_sample_size_train, model, metric, est, tau2, b_w_ratio, pred_icc, batch_no "

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

# Var U
var_u <-  DBI::dbGetQuery(db,
    glue(
      "SELECT {always_include}, error_var_u, sigma_u
      FROM {db_table}
      WHERE {where}  AND metric = 'var_u' AND (model ='Random intercetp - ML' OR model = 'Random intercetp - REML')
  ")
) |> correct_model_spelling()

saveRDS(var_u, file = here(save_folder, "var_u.RDS"))

# beta
beta <-  DBI::dbGetQuery(db,
            glue(
              "SELECT {always_include},
                beta_b, beta_w,  beta_pred, beta_mean_error, beta_mse, beta_rmse, beta_x, beta_names
              FROM {db_table}
              WHERE {where}  AND metric = 'var_u'
              ")
            ) |> correct_model_spelling()

saveRDS(beta, file = here(save_folder, "betas.RDS"))

# Calib-ITl
calib_itl <-  DBI::dbGetQuery(db,
      glue(
        "SELECT {always_include}
          FROM {db_table}
          WHERE {where}  AND metric = 'calib_itl'
  ")
) |> correct_model_spelling()

saveRDS(calib_itl, file = here(save_folder, "calib_itl.RDS"))

# Calib-Slope
calib_slope <-  DBI::dbGetQuery(db,
                  glue(
                    "SELECT {always_include}
                      FROM {db_table}
                      WHERE {where}  AND metric = 'calib_slope'
  ")
) |> correct_model_spelling()

saveRDS(calib_slope, file = here(save_folder, "calib_slope.RDS"))


# R-squared

r_squared <- DBI::dbGetQuery(db,
  glue(
    "SELECT {always_include}
    FROM {db_table}
    WHERE {where}  AND metric = 'r-squared'
  ")
) |> correct_model_spelling()
saveRDS(r_squared, file = here(save_folder, "r_squared.RDS"))


mse <- DBI::dbGetQuery(db,
                             glue(
                               "SELECT {always_include}
    FROM {db_table}
    WHERE {where}  AND metric = 'mse'
  ")
) |> correct_model_spelling()


saveRDS(mse, file = here(save_folder, "mse"))

DBI::dbDisconnect(db)

