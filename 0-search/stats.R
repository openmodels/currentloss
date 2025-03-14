setwd("~/research/currentloss/search2")

library(dplyr)

for (source in c('scopus', 'websci')) {
    source("standardize.R")
