library(DBI)
library(RSQLite)

db <- dbConnect(RSQLite::SQLite(), "Results/Database/sim_results.db")


# Running some queeries
dbListTables(db)

# Counting rows
dbGetQuery(db,
           "SELECT COUNT(*) FROM sim_results_v1")

#Counting rows from current batch
dbGetQuery(db,
           "SELECT COUNT(*)
           FROM (
              SELECT *
              FROM sim_results_v1
              WHERE sim_name = 'BatchAug2023'
            )
            AS subquery;"
)

#Counting rows from one file
dbGetQuery(db,
           "SELECT COUNT(*)
           FROM (
              SELECT *
              FROM sim_results_v1
              WHERE sim_name = 'BatchAug2023' AND batch_file = 'sim_results1'
            )
            AS subquery;"
)

# Getting column names
dbGetQuery(db,
           "SELECT * FROM sim_results_v1 WHERE 1 = 0")

#Getting distinct rows for different data generating scenarios columns
dbGetQuery(db,
            "SELECT  DISTINCT n_studies, ICC, R2, study_sample_size_train , int_pred_corr
            FROM sim_results_v1
            WHERE sim_name = 'BatchAug2023'  AND metric = 'var_u'"
)

#checking distict values of beta_mean_error - should correspond to models run
dbGetQuery(db,
           "SELECT DISTINCT beta_mean_error
            FROM sim_results_v1
            WHERE sim_name = 'BatchAug2023' AND metric = 'var_u'"
)

#Getting mean beta_...
dbGetQuery(db,
           "SELECT n_studies, ICC, R2, study_sample_size_train , int_pred_corr, model, AVG(beta_mean_error) as beta_mean_error, SQRT(AVG(beta_mse)) as beta_RMSE, beta_x, sigma_e, sigma_u
            FROM sim_results_v1
            WHERE sim_name = 'BatchAug2023' AND metric = 'var_u'
            GROUP BY n_studies, ICC, R2, study_sample_size_train , int_pred_corr, model"
)


dbDisconnect(db)
