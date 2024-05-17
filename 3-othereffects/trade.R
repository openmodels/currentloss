## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
source("src/lib/loadutils.R")
source("src/3-othereffects/trade-io.R")

method <- 'dd'
method.function <- NULL #calc.final.demand.method

dir.create(paste0("data/tradeloss-", method))

comtrade <- rbind(read.csv("data/trade/uncomtrade-1992.csv"), read.csv("data/trade/uncomtrade-2002.csv"),
                  read.csv("data/trade/uncomtrade-2012.csv"), read.csv("data/trade/uncomtrade-2022.csv"))

## Get all GDPs (for SLR fraction calc)
df.gdp3 <- load.gdp3()
slr2 <- load.slr2(df.gdp3)

for (persist in c("0.08", "0.21")) {
    load(paste0("data/mcrfres-", persist, ".RData"))

    results2 <- results %>% group_by(ISO, mc) %>%
        mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30]) %>%
        left_join(slr2, by=c('ISO', 'Year'='year', 'mc'))
    results2$slrloss[is.na(results2$slrloss)] <- 0

    if (method != 'dd') {
        for (year in unique(results2$Year)) {
            tradeloss <- data.frame()
            for (mcii in unique(results2$mc)) {
                print(c(persist, year, mcii))

                results2.year <- subset(results2, Year == year & mc == mcii)
                losses <- method.function(year, results2.year$ISO, results2.year$totimpact - results2.year$slrloss)

                tradeloss <- rbind(tradeloss, data.frame(ISO=results2.year$ISO, mc=mcii, year, tradeloss=losses))
            }
            save(tradeloss, file=paste0("data/tradeloss-", method, "/tradeloss-", year, "-", persist, ".RData"))
        }
    } else {
        for (mcii in unique(results2$mc)) {
            allthisyear <- list()
            allglobals <- data.frame()
            for (year in unique(results2$Year)) {
                print(c(persist, year, mcii))

                results2.year <- subset(results2, Year == year & mc == mcii)
                output <- calc.domar.distribute.method1(year, results2.year$ISO, results2.year$totimpact - results2.year$slrloss)
                allglobals <- rbind(allglobals, output$global)
                allthisyear[[year - min(results2$Year) + 1]] <- output$thisyear2
            }

            allglobals$yy <- allglobals$domar.change * allglobals$global.gdp
            scalebys <- log(allglobals$yy[allglobals$yy > 0]) - log(allglobals$global.fracloss[allglobals$yy > 0])
            ## years <- unique(results2$Year)[allglobals$yy > 0]
            ## ploy(years, scalebys)
            ## mod <- lm(scalebys ~ years)
            ## smoothscalebys <- exp(predict(mod, data.frame(years=unique(results2$Year)))) * exp(var(mod$resid) / 2)
            ## smoothscalebys[smoothscalebys > 1] <- 1

            scalebys <- scalebys[scalebys < 0]
            mod <- lm(scalebys ~ 1)
            smoothscalebys <- exp(predict(mod, data.frame(years=unique(results2$Year)))) * exp(var(mod$resid) / 2)

            tradeloss <- data.frame()
            for (ii in 1:length(allthisyear)) {
                losses <- calc.domar.distribute.method2(smoothscalebys[ii], results2.year$ISO, allthisyear[[ii]])
                tradeloss <- rbind(tradeloss, data.frame(ISO=results2.year$ISO, mc=mcii, year=min(results2$Year) + ii - 1, tradeloss=losses))
            }
            save(tradeloss, file=paste0("data/tradeloss-", method, "/tradeloss-", mcii, "-", persist, ".RData"))
        }
    }
}

