library(rstream)

# see chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://journal.r-project.org/articles/RN-2005-016/RN-2005-016.pdf

s <- new("rstream.mrg32k3a",
         seed=rep(12345,6), force.seed = TRUE)
print(s)
rnorm(1)
print(s)
rstream.RNG(s)
rnorm(1)
print(s)
print(.Random.seed)
rstream.nextsubstream(s)

print(s)
rnorm(1)
rstream.packed(s) <- TRUE
print(s)

rstream.packed(s) <- FALSE
rstream.RNG(s)
rnorm(1)
.Random.seed

rstream.reset(s)



first100 <- rnorm(100)

first100

set.seed(1)

set_rng_stream <- function(n, first100) {
  s <- new("rstream.mrg32k3a",
           seed=rep(65426, 6), force.seed = TRUE)
  for(i in 1:n){
    rstream.nextsubstream(s)
  }
  rstream.RNG(s)
  print(.Random.seed)
  print(rnorm(1))
  print(.Random.seed)
  rand <- rnorm(1)

  rand
  print(rand %in% first100)
  return(rand)
}

set_rng_stream(5, first100)

# how to save and restore rng state?

RNGkind("L'Ecuyer-CMRG")
set.seed(123)
s <- .Random.seed
rnorm(10)
.Random.seed
rnorm(10)
s1 <- .Random.seed
rnorm(1)

parallel::nextRNGStream(s)
rnorm(10)
.Random.seed
s2 = .Random.seed
rnorm(1)

RNGkind("L'Ecuyer-CMRG")
set.seed(s2)
rnorm(1)

RNGkind("L'Ecuyer-CMRG")
set.seed(s1)
rnorm(1)


