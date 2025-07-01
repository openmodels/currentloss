## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

do.skip.existing <- F
do.obsimport <- F

library(ranger)
library(readxl)
library(dplyr)
source("~/projects/research-common/R/myPBSmapping.R")

rf.approaches <- c("all", "controls", "nonlinear", "dataset")

load("data/mcres.RData")

rfstats <- data.frame()

for (persist in c("0", "0.21", "0.36", "0.47")) {
for (rf.approach in rf.approaches) {
    if (do.obsimport && (persist != "0.36" || rf.approach != "all"))
      next

    if (rf.approach == 'all') {
        savepath <- function(mcii) paste0("data/metaanal/mcrfres-", persist, "-", mcii, ifelse(do.obsimport, "-obs", ""), ".RData")
    } else {
        savepath <- function(mcii) paste0("data/metaanal/mcrfres-", persist, "-", rf.approach, "-", mcii, ifelse(do.obsimport, "-obs", ""), ".RData")
    }

    if (do.skip.existing) {
        foundall <- T
        for (mcii in 1:max(mcres$mc))
            if (!file.exists(savepath(mcii))) {
	        foundall <- F
	        break
	    }

        if (foundall)
            next
    }

    load("data/mcres-decumul.RData")
    kotzreplace <- decumul.bypersist[[persist]]
    rm('decumul.bypersist')
    allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), kotzreplace)

    ## Find rows for valid models that are NA (before some point in that model)
    allstat <- allres %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(dimpact)), NA, max(Year[is.na(dimpact) & Year < 2000]))) %>%
        group_by(paper, name) %>% summarize(status=max(status, na.rm=T))
    allres2 <- allres %>% group_by(ISO, paper, name) %>% filter(!all(is.na(dimpact))) %>%
        mutate(dimpact=ifelse(is.na(dimpact), 0, dimpact))

    allstat2 <- allres2 %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(dimpact)), NA, max(Year[is.na(dimpact) & Year < 2000]))) %>%
      group_by(paper, name) %>% summarize(status=max(status, na.rm=T))

source("src/lib/loadmetadata.R")

MCNUM <- max(allres$mc)
isos <- unique(allres$ISO)
years <- unique(allres$Year)

for (mcii in 1:MCNUM) {
    if (do.skip.existing && file.exists(savepath(mcii)))
        next
    print(savepath(mcii))

    allres3 <- subset(allres2, mc == mcii)
    results <- data.frame()

    for (iso in isos) {
        for (year in years) {
            print(c(rf.approach, persist, mcii, iso, year))

            allres4 <- subset(allres3, !is.na(dimpact) & Year == year & ISO == iso) %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))

            if (rf.approach == 'all') {
                values <- allres4[, c('dimpact', 'Q.Weather', 'Q.Poverty', 'Q.Temp', 'Q.Prec', 'Q.YearFE', 'Q.Trends',
                                      'Q.OtherFE', 'Q.Control', 'Q.GLags', 'Q.YearLate', 'Q.YearSpan')]

                preddf <- data.frame(Q.Weather=1, Q.Poverty=1, Q.Temp=1, Q.Prec=1, Q.YearFE=1, Q.Trends=1,
                                     Q.OtherFE=1, Q.Control=1, Q.GLags=1, Q.YearLate=1, Q.YearSpan=1)
            } else if (rf.approach == 'controls') {
                values <- allres4[, c('dimpact', 'Q.YearFE', 'Q.Trends',
                                      'Q.OtherFE', 'Q.Control', 'Q.GLags')]
                preddf <- data.frame(Q.YearFE=1, Q.Trends=1, Q.OtherFE=1, Q.Control=1, Q.GLags=1)
            } else if (rf.approach == 'nonlinear') {
                values <- allres4[, c('dimpact', 'Q.Poverty', 'Q.Temp', 'Q.Prec')]
                preddf <- data.frame(Q.Poverty=1, Q.Temp=1, Q.Prec=1)
            } else if (rf.approach == 'dataset') {
                values <- allres4[, c('dimpact', 'Q.Weather', 'Q.YearLate', 'Q.YearSpan')]
                preddf <- data.frame(Q.Weather=1, Q.YearLate=1, Q.YearSpan=1)
            }

            rfmod <- ranger(dimpact ~ ., data=values, num.trees=500, max.depth=12, verbose=TRUE)
            if (!do.obsimport) {
                predictions <- predict(rfmod, preddf)
                results <- rbind(results, data.frame(mc=mcii, Year=year, ISO=iso, dimpact=mean(predictions$predictions)))
            } else {
                terminals <- predict(rfmod, values, type="terminalNodes")$predictions
                chosen <- predict(rfmod, preddf, type="terminalNodes")$predictions
                values$usage <- sapply(1:nrow(values), function(ii) mean(terminals[ii, ] == chosen))
                results <- rbind(results, data.frame(mc=mcii, Year=year, ISO=iso, paper=allres4$paper, name=allres4$name, usage=values$usage))
            }

            rfstats <- rbind(rfstats, data.frame(persist, rf.approach, mc=mcii, Year=year, ISO=iso, mse=rfmod$prediction.error, r2=rfmod$r.squared))
        }
    }

    if (rf.approach == 'all') {
        save(results, file=savepath(mcii))
    } else {
        save(results, file=savepath(mcii))
    }
}

}
}

rfstats2 <- rfstats %>% left_join(allres3 %>% group_by(ISO, Year, mc) %>% summarize(vary=var(dimpact)))
hist(rfstats2$mse[!is.na(rfstats$r2)] / rfstats2$vary[!is.na(rfstats$r2)])

mean(rfstats2$mse[!is.na(rfstats$r2)])
quantile(rfstats2$mse[!is.na(rfstats$r2)])

var(allres3$dimpact)
mean(allres3$dimpact[allres3$dimpact != 0])

## R2 = 1 - (y - yhat)^2 / (y - ybar)^2
