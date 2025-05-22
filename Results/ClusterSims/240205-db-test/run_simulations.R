#!/usr/bin/env Rscript

# 240205-db-test

args = commandArgs(trailingOnly=TRUE)
batch_no <- as.integer(args)
if(length(batch_no) ==0){
  batch_no = 1
}
sim_name <- "db_test"

ProfacSims:::set_stream_seed(batch_no, seed =644646468)

print("Batch-no")
print(batch_no)
print("Random number for checking variation in seed:")
runif(1) # random number for chekcing that seed is correctly set.

n <- 100000
n_col <- 10
data <- data.frame(x0 = rnorm(n))
for(i in 1:n_col){
  data[paste0("x", i)] <- rnorm(n)
}

#1. Check if database exists - if not create it
#2. Connect to database
print("connecting to database")
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  glue::glue("{sim_name}.db")
  )

print("saving to database")

db_done = FALSE
attempt = 1
while(!db_done){
  print(glue::glue("Attempt {attempt} to write to db"))
  attempt = attempt + 1
  try({
    ProfacSims:::write_file_to_db(
      data = data,
      database_connection = con,
      table = sim_name)
    db_done = TRUE
  })
  if(!db_done) Sys.sleep(rnorm(1, 30, 5))
}


print("closing connection")

DBI::dbDisconnect(con)
