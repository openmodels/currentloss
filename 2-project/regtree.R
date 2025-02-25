## NOT UPDATED FOR v2

## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(rpart)
library(rpart.plot)

load("data/mcres.RData")
load("data/mcres-decumul.RData")

persist <- '0.21'

allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[persist]])

### COPIED FROM randforest.R

## Find rows for valid models that are NA (before some point in that model)
allstat <- allres %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(dimpact)), NA, max(Year[is.na(dimpact) & Year < 2000]))) %>%
    group_by(paper, name) %>% summarize(status=max(status, na.rm=T))
allresfix <- allres %>% group_by(ISO, paper, name) %>% filter(!all(is.na(dimpact))) %>%
    mutate(dimpact=ifelse(is.na(dimpact), 0, dimpact))
allres2 <- allres %>% left_join(allresfix, by=c('ISO', 'Year', 'paper', 'name', 'mc'), suffix=c('.ori', '.fix'))
allres2$dimpact <- ifelse(is.na(allres2$dimpact.ori), allres2$dimpact.fix, allres2$dimpact.ori)
allstat2 <- allres2 %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(dimpact)), NA, max(Year[is.na(dimpact) & Year < 2000]))) %>%
    group_by(paper, name) %>% summarize(status=max(status, na.rm=T))

source("src/lib/loadmetadata.R")

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

### END COPY

allres3 <- allres2 %>% group_by(ISO, Year, name, paper) %>% summarize(dimpact=mean(dimpact)) %>%
    filter(Year > 2013) %>% group_by(ISO, name, paper) %>% summarize(dimpact=mean(dimpact, na.rm=T)) %>%
    group_by(name, paper) %>% summarize(dimpact=mean(dimpact, na.rm=T))

allres4 <- allres3 %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))

values <- allres4[, c('dimpact', 'Dependent', 'Weather weight', 'Rich/Poor', 'Temp', 'Prec.',
                 'Year FE', 'Trends', 'Other FE', 'Other Controls', 'Growth Lags', 'Dataset',
                 'Year Coverage', 'last.year', 'first.year', 'Q.Weather', 'Q.Poverty', 'Q.Temp',
                 'Q.Prec', 'Q.YearFE', 'Q.Trends',
                 'Q.OtherFE', 'Q.Control', 'Q.GLags', 'Q.YearLate', 'Q.YearSpan')]
values$year.span <- values$last.year - values$first.year + 1

mod <- rpart(dimpact ~ `Weather weight` + `Rich/Poor` + Q.Temp + Q.Prec + Q.YearFE + Q.Trends + Q.OtherFE + Q.Control + Q.GLags + first.year + last.year + year.span, data=values)
rpart.plot(mod)
