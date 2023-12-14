get_stream_seed <- function(stream_no, seed=NULL) {
  if(!is.null(seed)){
    set.seed(seed = seed, kind = "L'Ecuyer-CMRG")

  }
  for(i in 1:stream_no) {
    .Random.seed <- parallel::nextRNGStream(.Random.seed)
  }
  return(.Random.seed)

}
