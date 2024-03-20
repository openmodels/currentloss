## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(PBSmapping)

persist <- 0.08
source("src/lib/utils2.R")
load.solowdata()

allyr <- results %>%
    left_join(tradeloss, by=c('mc', 'Year'='year', 'ISO')) %>%
    left_join(tradeloss.global, by=c('Year'='year'), suffix=c('', '.global'))
allyr$fracloss[is.na(allyr$fracloss)] <- allyr$fracloss.global[is.na(allyr$fracloss)]

solowdone <- read.csv("data/solow-v4.csv")

solownew <- data.frame()

for (mcii in 1:30) {
    load.solowdata.mc(mcii)

    for (iso in unique(allyr$ISO)) {
        print(c(mcii, iso))

        scaling <- 1
        if (any(solowdone$ISO == iso & solowdone$mc == mcii)) {
            row <- solowdone[which(solowdone$ISO == iso & solowdone$mc == mcii),]
            load(paste0("data/solow/v4-", iso, "-", mcii, ".RData"))
        } else {
            ## Not projected by capital model
            next
        }

        stan.data <- make.stan.data(iso)
        solowout.prod <- model.solow(la, stan.data, "prodonly")

        product.end.ccpc <- mean(solowout.prod$product[, 62])
        procap.end.ccpc <- mean(solowout.prod$procap_model[, 63])
        rencap.end.ccpc <- mean(solowout.prod$rencap_model[, 63])
        rencap.chg.ccpc <- 1 - rencap.end.ccpc / row$rencap.end.true

        row$rencap.chg.ccpc <- rencap.chg.ccpc * scaling

        solownew <- rbind(solownew, row)
    }
}

censorbounds <- quantile(solownew$rencap.chg.ccpc, c(.025, .975), na.rm=T)
solownew$rencap.chg.ccpc[solownew$rencap.chg.ccpc < censorbounds[1]] <- NA
solownew$rencap.chg.ccpc[solownew$rencap.chg.ccpc > censorbounds[2]] <- NA

write.csv(solownew, "data/solow-v4-ccpc.csv", row.names=F)
