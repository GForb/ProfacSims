library(DBI)
library(RSQLite)

db <- dbConnect(RSQLite::SQLite(), "Results/Database/sim_results.db")

# drop previously saved results
#dbExecute(db, "DELETE FROM  sim_results_v4 WHERE sim_name = '160122-pred-new-data'")



save_cluster_results_db(db = db,
                      table = "sim_results_v4",
                      sim_name = "160122-pred-new-data")

# Count rows in table
dbGetQuery(db,
           "SELECT sim_name, COUNT(*)
            FROM sim_results_v4
            GROUP BY sim_name"
)

dbDisconnect(db)
