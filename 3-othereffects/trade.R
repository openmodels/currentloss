setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

library(dplyr)

redo.results <- T

## https://pdfs.semanticscholar.org/542c/47f8fe03a36eb9871115e8b226288c519032.pdf?_ga=2.131851215.1529047367.1645760744-972231136.1645760744
## https://www.piie.com/system/files/documents/wp19-5.pdf

df <- data.frame(source=rep(c('Latorre et al. 2019', 'Dhingra et al. 2017'), each=2), brexit=rep(c('Soft Brexit', 'Hard Brexit'), 2), scenario=c('Soft (Norway case)', 'Hard (WTO case)', 'Soft Brexit scenario', 'Hard Brexit scenario'), case=c('Latorre et al. 2019 Soft (Norway case)', 'Latorre et al. 2019 Hard (WTO case)', 'Dhingra et al. 2017 Soft Brexit scenario', 'Dhingra et al. 2017 Hard Brexit scenario'), dgdp=c(-1.23, -2.53, -1.34, -2.66), dexport=c(-7.54, -16.94, -9, -16), dimport=c(-6.44, -14.42, -14, -16), method=rep(c('GAMS', 'Eaton–Kortum'), each=2))

mod <- lm(dgdp ~ 0 + dexport + dimport, data=df)

comtrade <- rbind(read.csv("data/trade/uncomtrade-1992.csv"), read.csv("data/trade/uncomtrade-2002.csv"),
                  read.csv("data/trade/uncomtrade-2012.csv"), read.csv("data/trade/uncomtrade-2022.csv"))

load("mcrfres.RData")
if (redo.results) {
    tradeloss.orig <- data.frame(ISO=c())
} else {
    load("tradeloss.RData")
    tradeloss.orig <- tradeloss
}

tradeloss <- data.frame()
for (iso in unique(results$ISO)) {
    if (iso %in% tradeloss.orig$ISO)
        next
    comtrade.iso <- subset(comtrade, ReporterISO == iso & PartnerISO != 'W00')
    results.iso <- subset(results, ISO == iso)

    for (mcii in unique(results.iso$mc)) {
        print(c(iso, mcii))
        for (year in unique(results.iso$Year)) {
            maxgrow <- max(0, results.iso$totimpact[results.iso$ISO == iso & results.iso$Year == year & results.iso$mc == mcii])

            if (year <= min(comtrade.iso$Period))
                calcdf <- subset(comtrade.iso, Period == min(comtrade.iso$Period)) %>% left_join(subset(results, Year == year & mc == mcii), by=c('PartnerISO'='ISO'))
            else if (year >= max(comtrade.iso$Period))
                calcdf <- subset(comtrade.iso, Period == max(comtrade.iso$Period)) %>% left_join(subset(results, Year == year & mc == mcii), by=c('PartnerISO'='ISO'))
            else if (year %in% comtrade.iso$Period)
                calcdf <- subset(comtrade.iso, Period == year) %>% left_join(subset(results, Year == year & mc == mcii), by=c('PartnerISO'='ISO'))
            else {
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
                calcdf <- comtrade.mix %>% left_join(subset(results, Year == year & mc == mcii), by=c('PartnerISO'='ISO'))
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
        save(tradeloss, file="tradeloss.RData")
    } else {
        save(tradeloss, file="tradeloss2.RData")
    }
}

