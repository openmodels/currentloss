## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)

source("~/projects/research-common/R/myPBSmapping.R")
source("src/lib/loadmetadata.R")

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

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

allres2 <- allres %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name) %>%
    summarize(usage=sum(usage * POP_EST) / sum(POP_EST))

allres3 <- group_by(paper) %>% summarize(usage=sum(usage))

allres3[order(allres3$usage, decreasing=T),]
