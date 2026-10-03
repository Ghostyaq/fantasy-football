rm(list = ls())

library(nflreadr)
library(tidyverse)
library(parallel)
library(pbapply)
library(purrr)

source("function-calculations.R")

cl <- makeCluster(detectCores() - 1)

clusterEvalQ(cl, {
    library(nflreadr)
    library(tidyverse)
})
clusterExport(cl, ls())
    
pboptions(type = "txt")
results <- lapply(2016:2026, mh)