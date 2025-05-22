library(DBI)
library(RSQLite)
library(glue)

db_table <- "sim_results_cluster_x_v2"
sim_name <- "240203-cluster-x"
db <- dbConnect(RSQLite::SQLite(), "Results/Database/sim_results.db")

dbListTables(db)
#dbGetQuery(db,
#           glue("SELECT sim_name, COUNT(*) FROM {db_table} GROUP BY sim_name"))

#dbGetQuery(db,
#           glue("SELECT * FROM {db_table} WHERE 1 = 0"))

#Drop table if required
#dbExecute(db,glue("DROP TABLE {db_table}"))
# drop previously saved results
#dbExecute(db, glue("DELETE FROM  {db_table} WHERE sim_name = {sim_name}"))



save_cluster_results_db(
  db = db,
  table = db_table,
  sim_name = sim_name
)

# Count rows in table
dbGetQuery(db,
           glue("SELECT sim_name, COUNT(*) FROM {db_table} GROUP BY sim_name"))

dbDisconnect(db)
