## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)

comtrade <- rbind(read.csv("data/trade/uncomtrade-1992.csv"), read.csv("data/trade/uncomtrade-2002.csv"),
                  read.csv("data/trade/uncomtrade-2012.csv"), read.csv("data/trade/uncomtrade-2022.csv"))

slrimpact <- read.csv("data/slrbyadm0-final.csv")

## Get all GDPs (for SLR fraction calc)
df.gdp2 <- read.wb("data/capital/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_5871893.xls", 'GDP.2015')
df.gdp3 <- subset(df.gdp2, `Country Code` %in% unique(df.gdp2$`Country Code`[!is.na(df.gdp2$GDP.2015)]) & !(`Country Code` %in% c("LIE", 'NCL'))) %>% group_by(`Country Code`) %>%
    reframe(Year=Year, GDP.2015.est=approx(Year, GDP.2015, Year, rule=2)$y)
df.gdp3$GDP.2019.est <- df.gdp3$GDP.2015.est * 106.87654 / 100

slr <- read.csv("data/slrbyadm0-final.csv")
slr2 <- slr %>% left_join(df.gdp3, by=c('ISO'='Country Code', 'year'='Year')) %>%
    group_by(ISO, year) %>% reframe(mc=1:30, slrloss=rnorm(30, mu / GDP.2019.est, ((q95 - q05) / diff(qnorm(c(.05, .95)))) / GDP.2019.est) / 10) # XXX: 1/10


for (persist in c("0.08", "0.21")) {
    load(paste0("data/mcrfres-", persist, ".RData"))

    results2 <- results %>% group_by(ISO, mc) %>%
        mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30])

    tradeloss <- data.frame()

    for (year in unique(results2$Year)) {
        for (mcii in unique(results2$mc)) {
            for (iso in unique(results2$ISO)) {
                print(c(year, iso, mcii))

                comtrade.iso <- subset(comtrade, ReporterISO == iso & PartnerISO != 'W00')
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
                calcdf$cif.lost <- calcdf$Cifvalue * pmax(-calcdf$totimpact, -maxgrow)
                calcdf$fob.lost <- calcdf$Fobvalue * pmax(-calcdf$totimpact, -maxgrow)

                fracloss.import <- sum(calcdf$cif.lost[calcdf$FlowDesc == 'Import'], na.rm=T) / sum(calcdf$Cifvalue[calcdf$FlowDesc == 'Import'], na.rm=T)
                fracloss.export <- sum(calcdf$fob.lost[calcdf$FlowDesc == 'Export'], na.rm=T) / sum(calcdf$Fobvalue[calcdf$FlowDesc == 'Export'], na.rm=T)






                ## Convert to %, just in case future model cares
            dgdp <- predict(mod, data.frame(dimport=-100 * fracloss.import, dexport=-100 * fracloss.export))
            tradeloss <- rbind(tradeloss, data.frame(ISO=iso, mc=mcii, year, fracloss=-dgdp / 100))
        }
    }

    if (redo.results) {
        save(tradeloss, file=paste0("data/tradeloss-", persist, ".RData"))
	if (nrow(tradeloss) > nrow(results2)/2)
	    break
    } else {
        save(tradeloss, file=paste0("data/tradeloss2-", persist, ".RData"))
    }
}

}
}
