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

for (persist in c("0.08", "0.21")) {
for (rf.approach in rf.approaches[4]) { # XXX: Currently do not do all, since already have that
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

metadata <- read_xlsx("data/Current Losses Estimate Metadata.xlsx")
metadata <- subset(metadata, !is.na(Paper) & Paper != "Rising & Tahmid")

metadata$Name[is.na(metadata$Name)] <- "NA"
metadata$Dependent[is.na(metadata$Dependent)] <- "NA"
metadata$`Weather weight`[is.na(metadata$`Weather weight`)] <- "NA"
metadata$`Weather weight`[grep("Pop.", metadata$`Weather weight`)] <- "Pop. weight"
metadata$`Rich/Poor`[is.na(metadata$`Rich/Poor`)] <- "NA"
metadata$`Rich/Poor`[metadata$`Rich/Poor` == "Project poor only"] <- "Subsetted"
metadata$Temp[is.na(metadata$Temp)] <- "NA"
metadata$Prec....13[is.na(metadata$Prec....13)] <- "NA"
metadata$`Year FE`[is.na(metadata$`Year FE`)] <- "NA"
metadata$`Trends`[is.na(metadata$`Trends`)] <- "NA"
metadata$`Trends`[metadata$`Trends` %in% c("Implicit linear by region", "Linear by Unit", "By Country", "Linear, By Country")] <- "Linear, by Unit"
metadata$`Trends`[metadata$`Trends` %in% c("Quad, By Country", "Quad by Unit")] <- "Quad, by Unit"
metadata$`Trends`[metadata$`Trends` == "Implicit linear by region"] <- "Linear, by Unit"
metadata$`Other FE`[is.na(metadata$`Other FE`)] <- "NA"
metadata$`Other Controls`[is.na(metadata$`Other Controls`)] <- "NA"
metadata$`Growth Lags`[is.na(metadata$`Growth Lags`)] <- "NA"
metadata$`Dataset`[is.na(metadata$`Dataset`)] <- "NA"
metadata$`Year Coverage`[is.na(metadata$`Year Coverage`)] <- "NA"
metadata$last.year <- sapply(metadata$`Year Coverage`, function(yys) as.numeric(strsplit(yys, " - ")[[1]][2]))
metadata$first.year <- sapply(metadata$`Year Coverage`, function(yys) as.numeric(strsplit(yys, " - ")[[1]][1]))
metadata$first.year[is.na(metadata$first.year)] <- 1950 # Varying 1901
metadata$`Climate`[is.na(metadata$`Climate`)] <- "NA"

metadata$Q.Weather <- 1 * (metadata$`Weather weight` == "Pop. weight")
metadata$Q.Poverty <- 1 * (metadata$`Rich/Poor` == "Interact")
metadata$Q.Temp <- ifelse(metadata$Temp %in% c("Quad", "Interacted with average", "Linear Spline"), 1,
                   ifelse(metadata$Temp %in% c("1 Lag", "10 Lags", "5 Lags", "Quad of Historical Differences", "VarT, DT, LDT, DT:T, LDT:LT"), 0.75,
                   ifelse(metadata$Temp != "NA", 0.5, 0)))
metadata$Q.Prec <- ifelse(metadata$Prec....13 %in% c("Quad", "Interacted with average", "Segmented", "Linear Spline", "Indicatators"), 1,
                   ifelse(metadata$Prec....13 %in% c("10 Lags", "1 Lag", "5 Lags"), 0.75,
                   ifelse(metadata$Prec....13 != "NA", 0.5, 0)))
metadata$Q.YearFE <- ifelse(metadata$`Year FE` == "By Region", 1,
                     ifelse(metadata$`Year FE` == "By Continent", 0.75,
                     ifelse(metadata$`Year FE` == "Yes", 0.5, 0)))
metadata$Q.Trends <- ifelse(metadata$`Trends` == "Quad, by Unit", 1,
                     ifelse(metadata$`Trends` == "Linear, by Unit", 0.5, 0))
metadata$Q.OtherFE <- ifelse(metadata$`Other FE` %in% c("Poor x Year", "HPJ-FE"), 1,
                      ifelse(metadata$`Other FE` %in% c("Poor", "IIS"), 0.5, 0))
metadata$Q.Control <- ifelse(metadata$`Other Controls` == "Lag Weather, Disaster", 1,
                      ifelse(metadata$`Other Controls` %in% c("Lag Weather", "7.0"), 0.5, 0))
metadata$Q.GLags <- as.numeric(metadata$`Growth Lags`) / 4
metadata$Q.YearLate <- 5 / (2015 - metadata$last.year + 5)
metadata$Q.YearSpan <- (metadata$last.year - metadata$first.year) / 65

MCNUM <- max(allres$mc)
isos <- unique(allres$ISO)
years <- unique(allres$Year)

results <- data.frame()
for (mcii in 1:MCNUM) {
    allres3 <- subset(allres2, mc == mcii)

    for (iso in isos) {
        for (year in years) {
            print(c(rf.approach, persist, mcii, iso, year))

            allres4 <- subset(allres3, !is.na(dimpact) & Year == year & ISO == iso) %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))

            if (rf.approach == 'all') {
                values <- allres4[, c('dimpact', 'Q.Weather', 'Q.Poverty', 'Q.Temp', 'Q.Prec', 'Q.YearFE', 'Q.Trends',
                                      'Q.OtherFE', 'Q.Control', 'Q.GLags', 'Q.YearLate', 'Q.YearSpan')]

                ## mod <- rpart(dimpact ~ `Dependent` + `Weather weight` + `Rich/Poor` + `Temp` + `Prec....13` +
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
}

    if (rf.approach == 'all') {
        save(results, file=paste0("data/mcrfres-", persist, ".RData"))
    } else {
        save(results, file=paste0("data/mcrfres-", persist, "-", rf.approach, ".RData"))
    }
}
}
