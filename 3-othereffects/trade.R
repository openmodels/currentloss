## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
source("src/lib/loadutils.R")
source("src/3-othereffects/trade-io.R")

## method <- 'dd'
## method.function <- calc.final.demand.method
do.keep.incgrp <- NA
do.outdir.suffix <- "" #"-mcpaperall" # "-mcr2all" # ""

method.function.map <- list('dd'=NULL, 'fd'=calc.final.demand.method, 'li'=calc.leontief.method)

for (method in c('dd', 'fd', 'li')) {
    method.function <- method.function.map[[method]]

for (do.keep.incgrp in c(NA, '1-2', '3-5')) {

if (!is.na(do.keep.incgrp)) {
    source("~/projects/research-common/R/myPBSmapping.R")

    polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')
    if (do.keep.incgrp == '3-5')
        dropiso <- polydata$ADM0_A3[polydata$INCOME_GRP %in% c("2. High income: nonOECD", "1. High income: OECD")]
    else
        dropiso <- polydata$ADM0_A3[polydata$INCOME_GRP %in% c("5. Low income", "4. Lower middle income", "3. Upper middle income")]
    suffix <- paste0('-', do.keep.incgrp)
} else {
    dropiso <- c()
    suffix <- ''
}

dir.create(paste0("data/tradeloss-", method, do.outdir.suffix))

comtrade <- rbind(read.csv("data/trade/uncomtrade-1992.csv"), read.csv("data/trade/uncomtrade-2002.csv"),
                  read.csv("data/trade/uncomtrade-2012.csv"), read.csv("data/trade/uncomtrade-2022.csv"))

## Get all GDPs (for SLR fraction calc)
df.gdp3 <- load.gdp3()
slr2 <- load.slr2(df.gdp3)

for (persist in c('0.36', '0', '0.21', '0.47')) {
    results <- read.metaanal.trade(do.outdir.suffix, persist)

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
                dimpact <- results2.year$totimpact - results2.year$slrloss
                dimpact[results2.year$ISO %in% dropiso] <- 0

                losses <- method.function(year, results2.year$ISO, dimpact)

                tradeloss <- rbind(tradeloss, data.frame(ISO=results2.year$ISO, mc=mcii, year, tradeloss=losses))
            }
            save(tradeloss, file=paste0("data/tradeloss-", method, do.outdir.suffix, "/tradeloss-", year, "-", persist, suffix, ".RData"))
        }
    } else {
        for (mcii in unique(results2$mc)) {
            allthisyear <- list()
            allglobals <- data.frame()
            for (year in unique(results2$Year)) {
                print(c(persist, year, mcii))

                results2.year <- subset(results2, Year == year & mc == mcii)
                dimpact <- results2.year$totimpact - results2.year$slrloss
                dimpact[results2.year$ISO %in% dropiso] <- 0

                output <- calc.domar.distribute.method1(year, results2.year$ISO, dimpact)
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
            if (length(scalebys[!is.na(scalebys)]) == 0) {
                tradeloss <- data.frame()
                for (ii in 1:length(allthisyear)) {
                    tradeloss <- rbind(tradeloss, data.frame(ISO=results2.year$ISO, mc=mcii, year=min(results2$Year) + ii - 1, tradeloss=NA))
                }
                save(tradeloss, file=paste0("data/tradeloss-", method, do.outdir.suffix, "/tradeloss-", mcii, "-", persist, suffix, ".RData"))
                next
            }

            mod <- lm(scalebys ~ 1)
            smoothscalebys <- exp(predict(mod, data.frame(years=unique(results2$Year)))) * exp(var(mod$resid) / 2)

            tradeloss <- data.frame()
            for (ii in 1:length(allthisyear)) {
                losses <- calc.domar.distribute.method2(smoothscalebys[ii], results2.year$ISO, allthisyear[[ii]])
                tradeloss <- rbind(tradeloss, data.frame(ISO=results2.year$ISO, mc=mcii, year=min(results2$Year) + ii - 1, tradeloss=losses))
            }
            save(tradeloss, file=paste0("data/tradeloss-", method, do.outdir.suffix, "/tradeloss-", mcii, "-", persist, suffix, ".RData"))
        }
    }
}

}
}
