## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

source("src/lib/loadmetadata.R")

library(dplyr)

allres <- data.frame()
for (mc in 1:30) {
    filepath <- paste0("data/metaanal/mcrfres-0.36-", mc, "-obs.RData")
    if (!file.exists(filepath)) {
        print(paste("Missing after", mc))
        break
    }
    load(filepath)
    allres <- rbind(allres, results)
}

allres2 <- allres %>% group_by(paper, name) %>% summarize(usage=mean(usage)) %>% group_by(paper) %>% summarize(usage=sum(usage))

allres2[order(allres2$usage, decreasing=T),]
