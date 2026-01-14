## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(dplyr)
library(mice)

load("data/mcres.RData")
load("data/mcres-decumul.RData")

source("src/lib/loadutils.R")
source("src/lib/loadmetadata.R")

metadata$papername <- paste(metadata$Paper, metadata$Name)

df.gdp3 <- load.gdp3()
df.pop2 <- read.wb("data/capital/API_SP.POP.TOTL_DS2_en_excel_v2_5871620.xls", 'Population')
df.baseline <- df.gdp3 %>% left_join(df.pop2, by=c('Country Code', 'Year')) %>% group_by(`Country Code`) %>%
    mutate(loggdppc=log(GDP.2019.est / Population), growth=c(NA, diff(loggdppc)))

## Cross-validate each model on the years it was not fit to.
results <- data.frame()
for (persist in c("0.36", "0", "0.21", "0.47")) {
    allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[persist]])

    ## Find rows for valid models that are NA (before some point in that model)
    allresfix <- allres %>% group_by(ISO, paper, name) %>% filter(!all(is.na(dimpact))) %>%
        mutate(dimpact=ifelse(is.na(dimpact), 0, dimpact))
    allres2 <- allres %>% left_join(allresfix, by=c('ISO', 'Year', 'paper', 'name', 'mc'), suffix=c('.ori', '.fix'))
    rm(allres, allresfix)

    allres2$dimpact <- ifelse(is.na(allres2$dimpact.ori), allres2$dimpact.fix, allres2$dimpact.ori)
    allres2$papername <- paste(allres2$paper, allres2$name)
    allres3 <- allres2 %>% group_by(ISO, Year, papername) %>% summarize(dimpact=mean(dimpact))

    persist <- as.numeric(persist)
    ## allres3 <- allres2 %>% left_join(df.baseline, by=c('ISO'='Country Code', 'Year')) %>%
    ##     group_by(papername, ISO, mc) %>%
    ##     mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - persist)^(0:30), sides=1)[-1:-30],
    ##            growth.nocc=growth - totimpact,
    ##            growth.nocc.hat=predict(lm(growth.nocc ~ lag(growth.nocc) + poly(year, 3)), data.frame(growth.nocc, year)))

    for (papernameii in unique(allres3$papername)) {
        for (isoii in unique(allres3$ISO[allres3$papername == papernameii])) {
            print(c(papernameii, isoii))
            subres3 <- allres3 %>% filter(papername == papernameii & ISO == isoii) %>%
                left_join(df.baseline, by=c('ISO'='Country Code', 'Year')) %>%
                group_by(papername, ISO) %>%
                mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - persist)^(0:30), sides=1)[-1:-30],
                       growth.nocc=growth - totimpact)
            if (sum(!is.na(subres3$growth.nocc)) < 5)
                next
            mod <- lm(growth.nocc ~ lag(growth.nocc) + poly(Year, 3), data=subres3)
            subres3$growth.nocc.hat <- predict(mod, subres3)
            subres3$growth.hat <- subres3$growth.nocc.hat + subres3$totimpact
            last.year <- metadata$last.year[metadata$papername == papernameii]
            crossval <- (subres3$growth - subres3$growth.hat)[subres3$Year > last.year]
            rmse <- sqrt(mean(crossval^2, na.rm=T))
            results <- rbind(results, data.frame(papername=papernameii, ISO=isoii, persist,
                                                 rmse, last.year, valid=length(crossval),
                                                 pop=mean(subres3$Population, na.rm=T),
                                                 gdp=mean(subres3$GDP.2019.est, na.rm=T)))
        }
    }
}

save(results, file="data/cross-validation.RData")

## load("data/cross-validation.RData")
results2 <- results %>% group_by(persist, papername) %>% summarize(rmse.pop=sum(rmse * pop) / sum(pop),
                                                                   rmse.gdp=sum(rmse * gdp) / sum(gdp))
