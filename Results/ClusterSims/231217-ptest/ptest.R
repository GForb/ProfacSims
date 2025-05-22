#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
batch_no <- as.integer(args)

set.seed(seed = 1234, kind = "L'Ecuyer-CMRG")

seed <- ProfacSims:::get_stream_seed(batch_no)


.Random.Seed <- ProfacSims:::get_stream_seed(batch_no)

results <- tibble::tibble(
  batch_no = batch_no,
  result = rnorm(1),
  rng = list(seed)
)

saveRDS(results, file = paste0("results", batch_no, ".RDS"))
