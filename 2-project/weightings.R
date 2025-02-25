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

allres2 <- allres %>% group_by(index) %>% summarize(usage=mean(usage))

load("data/mcres.RData")

allstat <- mcres %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(dimpact)), NA, max(Year[is.na(dimpact) & Year < 2000]))) %>%
        group_by(paper, name) %>% summarize(status=max(status, na.rm=T))
allstat$index <- 1:nrow(allstat)

allres3 <- allres2 %>% left_join(allstat, by='index')

allres3[order(allres3$usage, decreasing=T),]
