library(tidyverse)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Investigating high error_var_u

# Connecting to database
table = "sim_results_v4"
sim_name = "240119-pred-new-data"
where = glue("sim_name = '{sim_name}'")

db <- dbConnect(RSQLite::SQLite(), here("Results/Database/sim_results.db"))


colnames <- dbGetQuery(db,
                       glue("SELECT * FROM {table} WHERE 1 = 0"))

random_models <-  DBI::dbGetQuery(db,
                                  glue(
                                    "SELECT n_studies, ICC, R2, study_sample_size_train, model, predict_method, intercept_est_sample_size, metric, est, error_var_u, sigma_u, batch_no, rng_state
    FROM {table}
    WHERE {where} AND intercept_est_sample_size = 10 AND (predict_method = 'new0' OR
    predict_method = 'new_dynamic') AND metric = 'var_u'
    AND (model ='Random intercetp - ML' OR model = 'Random intercetp - REML')
    AND error_var_u > 0.5
  ")
)

previous_rep <- DBI::dbGetQuery(db,
                                      glue(
                                        "SELECT *
    FROM {table}
    WHERE {where} AND intercept_est_sample_size = 10
      AND R2 = 0.4
      AND ICC = 0.2
      AND study_sample_size_train = 50
      AND n_studies = 4
      AND batch_no = 961
  ")
)


batch_to_investigate <- DBI::dbGetQuery(db,
                                        glue(
                                          "SELECT *
    FROM {table}
    WHERE {where}
      AND batch_no = 962
  ")
) |> tibble()

batch_to_investigate |> filter(rng_state != "NA") |>
  select(n_studies, R2, ICC, study_sample_size_train, rng_state, intercept_est_sample_size) |>
  print(n = 50)



model_params <- rep_to_investigate |> filter(model == "Random intercetp - REML", ICC == 0.2, R2 == 0.4, predict_method == 'new0', metric == 'var_u') |>
  select(betas, est, error_var_u) |>
  tibble()

# Gettnig rng_state from previous rep
rng_state <- batch_to_investigate |> filter(rng_state != "NA") |>
  select(rng_state) |>
  slice(13)

rng_state <- rng_state[[1]] |> strsplit(split = ", ") |> unlist() |> as.numeric()


rng_state
set_stream_seed(batch = 962, seed = rng_state)

runif(1)


sigmas <- get_sigmas(n_predictors = 12, ICC = 0.2, R2 = 0.4)
train_data <- generate_continuous(
  n_studies = 4,
  study_sample_size = 50,
  sigmas = sigmas,
  n_predictors = 12)

model <- model_lmm_random_int_reml(train_data)
get_var_u(model)
