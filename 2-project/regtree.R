## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(rpart)
library(rpart.plot)
source("src/lib/loadutils.R")

persist <- '0.6'
allres <- load.allres(persist)

### COPIED FROM randforest.R

## Find rows for valid models that are NA (before some point in that model)
allstat <- allres %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(dimpact)), NA, max(Year[is.na(dimpact) & Year < 2000]))) %>%
    group_by(paper, name) %>% summarize(status=max(status, na.rm=T))
allres2 <- allres %>% group_by(ISO, paper, name) %>% filter(!all(is.na(dimpact))) %>%
    mutate(dimpact=ifelse(is.na(dimpact), 0, dimpact))

source("src/lib/loadmetadata.R")

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
