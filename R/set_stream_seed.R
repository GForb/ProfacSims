set_stream_seed <- function(batch, seed) {
  library(parallel)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed) #set seed to something
  s <- .Random.seed
  for (i in 1:batch) {
    s <- parallel::nextRNGStream(s)
  }
  .GlobalEnv$.Random.seed <- s

}
