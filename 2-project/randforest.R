## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(ranger)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(PBSmapping)

rf.approaches <- c("all", "controls", "nonlinear", "dataset")

load("data/mcres.RData")
load("data/mcres-decumul.RData")

for (persist in c("0.21", "0.08")) {
for (rf.approach in rf.approaches) {
    allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[persist]])

    ## Find rows for valid models that are NA (before some point in that model)
    allstat <- allres %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(dimpact)), NA, max(Year[is.na(dimpact) & Year < 2000]))) %>%
      group_by(paper, name) %>% summarize(status=max(status, na.rm=T))
    allresfix <- allres %>% group_by(ISO, paper, name) %>% filter(!all(is.na(dimpact))) %>%
      mutate(dimpact=ifelse(is.na(dimpact), 0, dimpact))
    allres2 <- allres %>% left_join(allresfix, by=c('ISO', 'Year', 'paper', 'name', 'mc'), suffix=c('.ori', '.fix'))
    allres2$dimpact <- ifelse(is.na(allres2$dimpact.ori), allres2$dimpact.fix, allres2$dimpact.ori)
    allstat2 <- allres2 %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(dimpact)), NA, max(Year[is.na(dimpact) & Year < 2000]))) %>%
      group_by(paper, name) %>% summarize(status=max(status, na.rm=T))

source("src/loadmetadata.R")

metadata$Q.Weather <- 1 * (metadata$`Weather weight` == "Pop. weight")
metadata$Q.Poverty <- ifelse(metadata$`Rich/Poor` == "Interact", 0.5,
                      ifelse(metadata$`Rich/Poor` == "Subsetted", 1.0, 0.))

metadata$Q.Temp <- ifelse(metadata$Temp == "VarT, DT, LDT, DT:T, LDT:LT", 1 - ((1 - .5)*(1 - .25)*(1 - .5)*(1 - .25)),
                   ifelse(metadata$Temp %in% c("DT, LDT, DT:T, LDT:T, T", "DT, LDT, DT:T, LDT:T, LT"), 1 - ((1 - .25)*(1 - .5)*(1 - .25)*(1 - .5)),
                   ifelse(metadata$Temp %in% c("DT, LDT, DT:T, LDT:T, T, T2", "DT, LDT, DT:T, LDT:T, LT, LT2"), 1 - ((1 - .25)*(1 - .5)*(1 - .25)*(1 - .5)*(1 - .5)),
                   ifelse(metadata$Temp == "DT, LDT", 1 - (1 - .25),
                   ifelse(metadata$Temp == "1 Lag", 1 - (1 - 0.25),
                   ifelse(metadata$Temp == "5 Lags", 1 - (1 - 0.25)^4,
                   ifelse(metadata$Temp %in% c("Quad", "Interacted with average", "Linear Spline", "LT, DT"), .5,
                   ifelse(metadata$Temp == "10 Lags", 1 - (1 - 0.25)^9,
                   ifelse(metadata$Temp %in% c("Linear", "Z-score", "FD", "Average 1986-2000 -  Average 1970-1985", "Symmetric Spline"), 0., NA)))))))))
metadata$Q.Prec <- ifelse(metadata$Prec. %in% c("DP, LDP, DP:P, LDP:P, P", "DP, LDP, DP:P, LDP:P, LP"), 1 - ((1 - .25)*(1 - .5)*(1 - .25)*(1 - .5)),
                   ifelse(metadata$Prec. %in% c("DP, LDP, DP:P, LDP:P, P, P2", "DP, LDP, DP:P, LDP:P, LP, LP2"), 1 - ((1 - .25)*(1 - .5)*(1 - .25)*(1 - .5)*(1 - .5)),
                   ifelse(metadata$Prec. == "Indicatators", 1 - (1 - 0.5)*(1 - 0.5)^2*(1 - 0.5)^2*(1 - 0.5)^2,
                   ifelse(metadata$Prec. == "5 Lags", 1 - (1 - 0.25)^4,
                   ifelse(metadata$Prec. %in% c("Quad", "Interacted with average", "Linear Spline", "LP, DP"), .5,
                   ifelse(metadata$Prec. %in% c("1 Lag", "DT, LDT"), 1 - (1 - 0.25),
                   ifelse(metadata$Prec. == "10 Lags", 1 - (1 - 0.25)^9,
                   ifelse(metadata$Prec. %in% c("Linear", "Z-score", "FD", "Average 1986-2000 -  Average 1970-1985", "Symmetric Spline"), 0,
                   ifelse(metadata$Prec. == "NA", -1, NA)))))))))
metadata$Q.YearFE <- ifelse(metadata$`Year FE` == "By Region", 1,
                     ifelse(metadata$`Year FE` == "By Continent", 0.75,
                     ifelse(metadata$`Year FE` == "Yes", 0.5, 0)))
metadata$Q.Trends <- ifelse(metadata$`Trends` == "Quad, by Unit", 1,
                     ifelse(metadata$`Trends` == "Linear, by Unit", 0.5,
                     ifelse(metadata$`Trends` == "Global", 0.25, 0)))
metadata$Q.OtherFE <- ifelse(metadata$`Other FE` == "IIS", 1,
                      ifelse(metadata$`Other FE` == "Poor x Year", .75,
                      ifelse(metadata$`Other FE` == "HPJ-FE", 0.5,
                      ifelse(metadata$`Other FE` == "Poor", 0.25, 0))))
metadata$Q.Control <- ifelse(metadata$`Other Controls` == "Lag GDP, Lag capital, Pesaran controls", 1,
                      ifelse(metadata$`Other Controls` == "Lag Weather, Disaster", 0.75,
                      ifelse(metadata$`Other Controls` == "Lag Weather", 0.5, 0)))
metadata$Q.GLags <- as.numeric(metadata$`Growth Lags`) / 4
metadata$Q.YearLate <- 5 / (2015 - metadata$last.year + 5)
metadata$Q.YearSpan <- (metadata$last.year - metadata$first.year) / 65

MCNUM <- max(allres$mc)
isos <- unique(allres$ISO)
years <- unique(allres$Year)

for (mcii in 1:MCNUM) {
    allres3 <- subset(allres2, mc == mcii)
    results <- data.frame()

    for (iso in isos) {
        for (year in years) {
            print(c(rf.approach, persist, mcii, iso, year))

            allres4 <- subset(allres3, !is.na(dimpact) & Year == year & ISO == iso) %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))

            if (rf.approach == 'all') {
                values <- allres4[, c('dimpact', 'Q.Weather', 'Q.Poverty', 'Q.Temp', 'Q.Prec', 'Q.YearFE', 'Q.Trends',
                                      'Q.OtherFE', 'Q.Control', 'Q.GLags', 'Q.YearLate', 'Q.YearSpan')]

                ## mod <- rpart(dimpact ~ `Dependent` + `Weather weight` + `Rich/Poor` + `Temp` + `Prec.` +
                ##  `Year FE` + `Trends` + `Other FE` + `Growth Lags` + `Dataset` + `Year Coverage`, data=values)
                ## rpart.plot(mod)

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
            predictions <- predict(rfmod, preddf)

            results <- rbind(results, data.frame(mc=mcii, Year=year, ISO=iso, dimpact=mean(predictions$predictions)))
        }
    }

    if (rf.approach == 'all') {
        save(results, file=paste0("data/metaanal/mcrfres-", persist, "-", mcii, ".RData"))
    } else {
        save(results, file=paste0("data/metaanal/mcrfres-", persist, "-", rf.approach, "-", mcii, ".RData"))
    }
}

}
}
