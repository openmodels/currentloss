## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
source("src/lib/loadutils.R")
source("src/3-othereffects/trade-io.R")

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

    for (year in unique(results2$Year)) {
        tradeloss <- data.frame()
        for (mcii in unique(results2$mc)) {
            print(c(persist, year, mcii))
            thisyear <- data.frame()
            for (iso in unique(results2$ISO)) {

                comtrade.iso <- subset(comtrade, ReporterISO == iso & PartnerISO != 'W00')
                if (nrow(comtrade.iso) == 0)
                    next
                results2.iso <- subset(results2, ISO == iso)

                maxgrow <- max(0, results2.iso$totimpact[results2.iso$ISO == iso & results2.iso$Year == year & results2.iso$mc == mcii])

                if (year <= min(comtrade.iso$Period)) {
                    calcdf <- subset(comtrade.iso, Period == min(comtrade.iso$Period)) %>% left_join(subset(results2, Year == year & mc == mcii), by=c('PartnerISO'='ISO'))
                } else if (year >= max(comtrade.iso$Period)) {
                    calcdf <- subset(comtrade.iso, Period == max(comtrade.iso$Period)) %>% left_join(subset(results2, Year == year & mc == mcii), by=c('PartnerISO'='ISO'))
                } else if (year %in% comtrade.iso$Period) {
                    calcdf <- subset(comtrade.iso, Period == year) %>% left_join(subset(results2, Year == year & mc == mcii), by=c('PartnerISO'='ISO'))
                } else {
                    yearbefore <- max(comtrade.iso$Period[comtrade.iso$Period < year])
                    yearafter <- min(comtrade.iso$Period[comtrade.iso$Period > year])
                    portionafter <- (year - yearbefore) / (yearafter - yearbefore)
                    comtrade.mix <- subset(comtrade.iso, Period == yearbefore) %>% full_join(subset(comtrade.iso, Period == yearafter), by=c('PartnerISO', 'FlowDesc'), suffix=c('.bef', '.aft'))
                    comtrade.mix$Cifvalue <- ifelse(is.na(comtrade.mix$Cifvalue.bef), comtrade.mix$Cifvalue.aft,
                                             ifelse(is.na(comtrade.mix$Cifvalue.aft), comtrade.mix$Cifvalue.bef,
                                                    portionafter * comtrade.mix$Cifvalue.aft + (1 - portionafter) * comtrade.mix$Cifvalue.bef))
                    comtrade.mix$Fobvalue <- ifelse(is.na(comtrade.mix$Fobvalue.bef), comtrade.mix$Fobvalue.aft,
                                             ifelse(is.na(comtrade.mix$Fobvalue.aft), comtrade.mix$Fobvalue.bef,
                                                    portionafter * comtrade.mix$Fobvalue.aft + (1 - portionafter) * comtrade.mix$Fobvalue.bef))
                    calcdf <- comtrade.mix %>% left_join(subset(results2, Year == year & mc == mcii), by=c('PartnerISO'='ISO'))
                }

                ## Limit any growth to growth of country
                calcdf$cif.lost <- calcdf$Cifvalue * pmax(-calcdf$totimpact + calcdf$slrloss, -maxgrow)
                calcdf$fob.lost <- calcdf$Fobvalue * pmax(-calcdf$totimpact + calcdf$slrloss, -maxgrow)

                ## Fill in NAs, with preference based on direction
                calcdf$fob.lost[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export'] <- calcdf$cif.lost[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export']
                calcdf$Fobvalue[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export'] <- calcdf$Cifvalue[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export']
                calcdf$cif.lost[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import'] <- calcdf$fob.lost[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import']
                calcdf$Cifvalue[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import'] <- calcdf$Fobvalue[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import']

                fracloss.import <- sum(calcdf$cif.lost[calcdf$FlowDesc == 'Import'], na.rm=T) / sum(calcdf$Cifvalue[calcdf$FlowDesc == 'Import'], na.rm=T)
                fracloss.export <- sum(calcdf$fob.lost[calcdf$FlowDesc == 'Export'], na.rm=T) / sum(calcdf$Fobvalue[calcdf$FlowDesc == 'Export'], na.rm=T)

                thisyear <- rbind(thisyear, data.frame(ISO=iso, mc=mcii, year, fracloss.import, fracloss.export))
            }

            ## Scale to the Domar loss
            results2.year <- subset(results2, Year == year & mc == mcii)
            domar.loss <- calc.domar.loss(year, results2.year$ISO, results2.year$totimpact - results2.year$slrloss)

            if (year <= 2022) {
                thisyear2 <- thisyear %>% left_join(df.gdp3, by=c('ISO'='Country Code', 'year'='Year'))
            } else {
                thisyear2 <- thisyear %>% left_join(subset(df.gdp3, Year == 2022), by=c('ISO'='Country Code'))
            }
            ## domar.loss[1] * sum(thisyear2$GDP.2019.est) = A * sum(thisyear2$fracloss.export * thisyear2$GDP.2019.est)
            scaleby <- domar.loss[1] * sum(thisyear2$GDP.2019.est, na.rm=T) / sum(ifelse(is.na(thisyear2$fracloss.export), 0, thisyear2$fracloss.export) * thisyear2$GDP.2019.est, na.rm=T)
            thisyear2$tradeloss <- thisyear2$fracloss.export * scaleby

            tradeloss <- rbind(tradeloss, thisyear2[, c('ISO', 'mc', 'year', 'tradeloss')])
        }
        save(tradeloss, file=paste0("data/tradeloss/tradeloss-", year, "-", persist, ".RData"))
    }
}

