setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

library(Hmisc)
library(PBSmapping)
source("src/lib/utils2.R")
load.solowdata()

allyr <- results %>%
    left_join(tradeloss, by=c('mc', 'Year'='year', 'ISO')) %>%
    left_join(tradeloss.global, by=c('Year'='year'), suffix=c('', '.global'))
allyr$fracloss[is.na(allyr$fracloss)] <- allyr$fracloss.global[is.na(allyr$fracloss)]

solowold <- rbind(read.csv("results-old/solow-v2.csv"), read.csv("results-old/solow-v2-11.csv"), read.csv("results-old/solow-v2-21.csv"), read.csv("results-old/solow-v2-26.csv"), read.csv("results-old/solow-v2-x.csv"))
solowdone <- rbind(read.csv("solow-prefill/solow-v2.csv"), read.csv("solow-prefill/solow-v2-11.csv"), read.csv("solow-prefill/solow-v2-21.csv"))

solownew <- data.frame()

for (mcii in 1:30) {
    load.solowdata.mc(mcii)

    for (iso in unique(allyr$ISO)) {
        print(c(mcii, iso))

        scaling <- 1
        if (any(solowdone$ISO == iso & solowdone$mc == mcii)) {
            row <- solowdone[which(solowdone$ISO == iso & solowdone$mc == mcii),]
            load(paste0("solow-prefill/v2-", iso, "-", mcii, ".RData"))
        } else if (any(solowold$ISO == iso & solowold$mc == mcii)) {
            row <- solowold[which(solowold$ISO == iso & solowold$mc == mcii),][1, ]

            ## Construct fractional correction
            allyrii <- allyr[allyr$ISO == iso & allyr$mc == mcii & allyr$Year == 2022,]
            if (is.na(row$itlimpact.end)) {
                presolow.old <- row$totimpact.end
                presolow.new <- allyrii$totimpact
            } else {
                presolow.old <- row$totimpact.end + row$itlimpact.end
                presolow.new <- allyrii$totimpact - allyrii$fracloss
            }
            if (sign(presolow.new) != sign(presolow.old)) {
                next # Don't use this
            } else if (abs(presolow.new) < abs(presolow.old)) {
                scaling <- abs(presolow.new) / abs(presolow.old)
                print(paste0("Fill in with scaled old at ", scaling, "."))
            } else
                print("Fill in with old.")

            row$totimpact.end <- allyrii$totimpact
            row$itlimpact.end <- -allyrii$fracloss

            load(paste0("results-old/v2-", iso, "-", mcii, ".RData"))
        } else {
            ## Not projected by capital model
            next
        }

        stan.data <- make.stan.data(iso)
        solowout <- model.solow(la, stan.data, F, rencaptrue=la$rencap_model)

        product.end.true <- mean(la$product[, 62])
        product.end.nocc <- mean(solowout$product[, 62])

        procap.end.true <- mean(la$procap_model[, 63])
        procap.end.nocc <- mean(solowout$procap_model[, 63])

        rencap.end.true <- mean(la$rencap_model[, 63])
        rencap.end.nocc <- mean(solowout$rencap_model[, 63])

        product.chg <- 1 - product.end.nocc / product.end.true
        rencap.chg <- 1 - rencap.end.nocc / rencap.end.true
        procap.chg <- 1 - procap.end.nocc / procap.end.true

        ## print(data.frame(mcii, iso, product.chg.old=row$product.chg, product.chg.new=product.chg,
        ##                  rencap.chg.old=row$rencap.chg, rencap.chg.new=rencap.chg,
        ##                  procap.chg.old=row$procap.chg, procap.chg.new=procap.chg))

        row$product.end.nocc <- product.end.nocc * scaling
        row$rencap.end.nocc <- rencap.end.nocc * scaling
        row$procap.end.nocc <- procap.end.nocc * scaling
        row$product.chg <- product.chg * scaling
        row$rencap.chg <- rencap.chg * scaling
        row$procap.chg <- procap.chg * scaling
        ## Rewrite these in case using scaling
        row$product.end.true <- product.end.true * scaling
        row$procap.end.true <- procap.end.true * scaling
        row$rencap.end.true <- rencap.end.true * scaling

        solowout.prod <- model.solow(la, stan.data, "prodonly")

        product.end.ccpc <- mean(solowout.prod$product[, 62])
        procap.end.ccpc <- mean(solowout.prod$procap_model[, 63])
        rencap.end.ccpc <- mean(solowout.prod$rencap_model[, 63])
        rencap.chg.ccpc <- 1 - rencap.end.ccpc / rencap.end.true

        row$rencap.chg.ccpc <- rencap.chg.ccpc * scaling

        solownew <- rbind(solownew, row)
        save(la, file=paste0("solow/v3-", iso, "-", mcii, ".RData"))
    }
}

censorbounds <- quantile(solownew$rencap.chg.ccpc, c(.025, .975), na.rm=T)
solownew$rencap.chg.ccpc[solownew$rencap.chg.ccpc < censorbounds[1]] <- NA
solownew$rencap.chg.ccpc[solownew$rencap.chg.ccpc > censorbounds[2]] <- NA

write.csv(solownew, "solow-v3.csv", row.names=F)
