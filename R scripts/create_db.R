library(RSQLite)
library(DBI)

db <- dbConnect(RSQLite::SQLite(), "Results/Database/sim_results.db")
dbDisconnect(db)

