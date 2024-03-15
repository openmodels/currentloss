## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(ranger)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(PBSmapping)

load("data/mcres.RData")
load("data/mcres-decumul.RData")

for (persist in c("0.08", "0.21")) {
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
metadata$Prec.[is.na(metadata$Prec.)] <- "NA"
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
metadata$Q.Prec <- ifelse(metadata$Prec. %in% c("Quad", "Interacted with average", "Segmented", "Linear Spline", "Indicatators"), 1,
                   ifelse(metadata$Prec. %in% c("10 Lags", "1 Lag", "5 Lags"), 0.75,
                   ifelse(metadata$Prec. != "NA", 0.5, 0)))
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
            print(c(mcii, iso, year))

            allres4 <- subset(allres3, !is.na(dimpact) & Year == year & ISO == iso) %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))
            values <- allres4[, c('dimpact', 'Q.Weather', 'Q.Poverty', 'Q.Temp', 'Q.Prec', 'Q.YearFE', 'Q.Trends',
	    	      		  'Q.OtherFE', 'Q.Control', 'Q.GLags', 'Q.YearLate', 'Q.YearSpan')]

            ## mod <- rpart(dimpact ~ `Dependent` + `Weather weight` + `Rich/Poor` + `Temp` + `Prec.` +
	    ##  `Year FE` + `Trends` + `Other FE` + `Growth Lags` + `Dataset` + `Year Coverage`, data=values)
            ## rpart.plot(mod)

            rfmod <- ranger(dimpact ~ ., data=values, num.trees=500, max.depth=12, verbose=TRUE)
            predictions <- predict(rfmod, data.frame(Q.Weather=1, Q.Poverty=1, Q.Temp=1, Q.Prec=1, Q.YearFE=1, Q.Trends=1,
	      Q.OtherFE=1, Q.Control=1, Q.GLags=1, Q.YearLate=1, Q.YearSpan=1))

            results <- rbind(results, data.frame(mc=mcii, Year=year, ISO=iso, dimpact=mean(predictions$predictions)))
        }
    }
}

save(results, file=paste0("data/mcrfres-", persist, ".RData"))
}

load("data/mcrfres-0.08.RData")

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

results2 <- results %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(Year, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST) / sum(POP_EST)) %>%
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
    guides(linetype=F)
ggsave("figures/randforest.pdf", width=6.5, height=5)

## Combined figure
allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[["0.08"]])

allres2 <- allres %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST) / sum(POP_EST)) %>%
    group_by(paper, name, Year) %>% summarize(mu=mean(gloimpact))

allres3 <- allres2 %>% group_by(Year) %>% summarize(mu=median(mu, na.rm=T))

labels <- data.frame(Year=c(2018, 2018), xend=c(1997, 1997),
                     y=c(results2$mu[results2$Year == 2018], allres3$mu[allres3$Year == 2018]),
                     yend=c(-.035, .015), label=c("Random Forest", "Median Model"))

ggplot(allres2, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.04, .02)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    geom_line(data=allres3, size=2, colour='black', alpha=.75) +
    geom_line(data=results2, size=2, colour='#b15928', alpha=.75) +
    geom_segment(data=labels, aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels, aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1940, 2022)) +
    scale_colour_discrete("Reference:")
ggsave("figures/allimpacts-withrf.pdf", width=8, height=4)
