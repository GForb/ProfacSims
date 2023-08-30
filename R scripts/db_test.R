library(RSQLite)
library(DBI)

rtest_db <- dbConnect(RSQLite::SQLite(), "Results/Database/test.db")

dbListTables(rtest_db)



df <- sim_results_new |> select(-model_function_list, -rng_state) |> data.frame()

dbWriteTable(conn = rtest_db,
             name = "test_results",
             value = df)


calib_slopes <- dbGetQuery(rtest_db,
           "SELECT metric, est  FROM test_results WHERE metric LIKE 'calib_slope'") |> tibble()
