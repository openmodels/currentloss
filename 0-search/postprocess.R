## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)

source <- "websci"

source("src/0-search/standardize.R")

df3 <- subset(df2, included | AI.include)
write.csv(df3, paste0("further-consideration-", source, ".csv"), row.names=F)



