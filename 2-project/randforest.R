setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

library(ranger)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(PBSmapping)

## allres <- read.csv("allres.csv")
load("mcres.Rdata")
allres <- df.imp2
allres$name[is.na(allres$name)] <- "NA"

## Find rows for valid models that are NA (before some point in that model)
allstat <- allres %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(totimpact)), NA, max(Year[is.na(totimpact) & Year < 2000]))) %>% group_by(paper, name) %>% summarize(status=max(status, na.rm=T))
allresfix <- allres %>% group_by(ISO, paper, name) %>% mutate(totimpact=ifelse(all(is.na(totimpact)), NA, ifelse(all(!is.na(totimpact[Year > 1970 & Year < 2000])), totimpact, c(rep(0, 1970 - 1940 + 1), totimpact[Year > 1970]))))
allstat2 <- allresfix %>% group_by(ISO, paper, name) %>% summarize(status=ifelse(all(is.na(totimpact)), NA, max(Year[is.na(totimpact) & Year < 2000]))) %>% group_by(paper, name) %>% summarize(status=max(status, na.rm=T))

metadata <- read_xlsx("data/Current Losses Estimate Metadata.xlsx")
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
    allres2 <- subset(allresfix, mc == mcii)

    for (iso in isos) {
        for (year in years) {
            print(c(mcii, iso, year))

            allres3 <- subset(allres2, !is.na(totimpact) & Year == year & ISO == iso) %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))
            values <- allres3[, c('totimpact', 'Q.Weather', 'Q.Poverty', 'Q.Temp', 'Q.Prec', 'Q.YearFE', 'Q.Trends', 'Q.OtherFE', 'Q.Control', 'Q.GLags', 'Q.YearLate', 'Q.YearSpan')]

            ## mod <- rpart(dimpact ~ `Dependent` + `Weather weight` + `Rich/Poor` + `Temp` + `Prec....13` + `Year FE` + `Trends` + `Other FE` + `Growth Lags` + `Dataset` + `Year Coverage`, data=values)
            ## rpart.plot(mod)

            rfmod <- ranger(totimpact ~ ., data=values, num.trees=500, max.depth=12, verbose=TRUE)
            predictions <- predict(rfmod, data.frame(Q.Weather=1, Q.Poverty=1, Q.Temp=1, Q.Prec=1, Q.YearFE=1, Q.Trends=1, Q.OtherFE=1, Q.Control=1, Q.GLags=1, Q.YearLate=1, Q.YearSpan=1))

            results <- rbind(results, data.frame(mc=mcii, Year=year, ISO=iso, totimpact=mean(predictions$predictions)))
        }
    }
}

save(results, file="mcrfres.RData")
## load("mcrfres.RData")

polydata <- attr(importShapefile("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses/data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

results2 <- results %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(Year, mc) %>% summarize(gloimpact=sum(totimpact * POP_EST) / sum(POP_EST)) %>%
    group_by(Year) %>% summarize(mu=mean(gloimpact),
                                 ci25=quantile(gloimpact, .25),
                                 ci75=quantile(gloimpact, .75))
##results2.loess <- rbind(data.frame(Year=1850:1939, mu=0, ci25=0, ci75=0), results2)
## results2$muloess = tail(predict(loess(mu ~ 0 + Year, results2.loess, span=.25)), nrow(results2))
## results2$ci25loess = tail(predict(loess(ci25 ~ Year, results2.loess, span=.25)), nrow(results2))
## results2$ci75loess = tail(predict(loess(ci75 ~ Year, results2.loess, span=.25)), nrow(results2))

results2$muloess = predict(loess(mu ~ 0 + Year, results2, span=.24))
results2$ci25loess = predict(loess(ci25 ~ Year, results2, span=.24))
results2$ci75loess = predict(loess(ci75 ~ Year, results2, span=.24))

library(ggplot2)
ggplot(results2, aes(Year, muloess)) +
    geom_line() + geom_ribbon(aes(ymin=ci25loess, ymax=ci75loess), alpha=.5) +
    theme_bw() + xlab(NULL) + scale_y_continuous("Global population-weighted GDP loss", labels=scales::percent)

ggplot(results2, aes(Year, mu, linetype=Year <= 1970)) +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + scale_y_continuous("Global population-weighted GDP loss", labels=scales::percent) +
    scale_x_continuous(NULL, limits=c(1950, 2022), expand=c(0, 0)) +
    guides(linetype=F) + geom_text(x=1960, y=-.002, label="(Insignificant change)")
ggsave("figures/randforest.pdf", width=6.5, height=5)
