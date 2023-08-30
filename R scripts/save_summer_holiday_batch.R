library(DBI)
library(RSQLite)

db <- dbConnect(RSQLite::SQLite(), "Results/Database/sim_results.db")

# drop previously saved results
# try(dbExecute(conn = db, "DELETE FROM sim_results_v1 WHERE sim_name = 'BatchAug2023'"))

save_batch_results_db(database_connection = db,
                      table = "sim_results_v1",
                      results_folder = here::here("Results/HolidayBatch"),
                      sim_name = "BatchAug2023")
dbDisconnect(db)
