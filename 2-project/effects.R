## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(dplyr)
library(ggplot2)
source("src/lib/myPBSmapping.R")
source("src/lib/loadutils.R")

persist <- 0.6
allres <- load.allres(persist)

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

allres2 <- allres %>% filter(!is.na(dimpact) & Year > 2013) %>%
    group_by(paper, name, ISO, mc) %>% summarize(dimpact=mean(dimpact)) %>%
    left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST) / sum(POP_EST))

source("src/lib/loadmetadata.R")
metadata$`Dependent` <- ifelse(metadata$`Dependent` %in% c("PPP GDPpc growth", "GCPpc growth", "GRPpc growth"), metadata$`Dependent`,
                        ifelse(metadata$`Dependent` == "PPP GDPpc growth (chained)", "PPP GDPpc growth", "Other"))
metadata$`Weather weight`[metadata$`Weather weight` == "Centroid"] <- "Area weight"
metadata$`Year FE`[metadata$`Year FE` == "NA"] <- "No"
metadata$`Trends`[metadata$`Trends` == "NA"] <- "No"
metadata$`Other Controls`[metadata$`Other Controls` == "NA"] <- "None"
metadata$`Other Controls` <- ifelse(metadata$`Other Controls` %in% c("No"), metadata$`Other Controls`,
                             ifelse(metadata$`Other Controls` %in% c("Lag Weather", "Pop Growth", "Mean Growth"), "1 Control",
                             ifelse(metadata$`Other Controls` %in% c("ENSO x 2", "Lag Weather, Disaster", "WASP x 2"), "2 Controls",
                             ifelse(metadata$`Other Controls` %in% c("Lag GDP, Lag capital, Pesaran controls", "Mean Growth, Temp, Precip.", "Runoff, WASR x 2"), "3 Controls",
                             ifelse(metadata$`Other Controls` %in% c("FDI, Gov, CR, TROP, HC", "Runoff, WASP x 2, WASR x 2", "LGDP, GOV, HC, TRADE, FDI, POP"), "≥4 Controls", metadata$`Other Controls`)))))
metadata$year.length <- metadata$last.year - metadata$first.year + 1
metadata$f.first.year <- factor(metadata$first.year)
metadata$f.last.year <- factor(metadata$last.year)
metadata$f.year.length <- factor(metadata$year.length)

allres3 <- allres2 %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))

summary(lm(gloimpact ~ 0 + Dependent + `Weather weight` + `Rich/Poor` + Temp + Prec. + `Year FE` + Trends + `Other FE` + `Other Controls` + `Growth Lags` + f.first.year + f.year.length, data=allres3)) # Don't believe it: mcs

get.coeffs <- function(pred, predname, renames=c(), levels=NULL) {
    formpred <- ifelse(grepl(' |/', pred), paste0('`', pred, '`'), pred)
    coeffsmc <- data.frame()
    allres4 <- allres3
    allres4[, pred] <- as.character(allres4[, pred, drop=T])
    if (!is.character(allres4[, pred, drop=T]))
        allres4[, pred] <- as.character(allres4[, pred, drop=T])
    allres4[is.na(allres4[, pred]), pred] <- 'NA'
    for (old in names(renames))
        allres4[as.character(allres4[, pred, drop=T]) == old, pred] <- renames[old]
    for (mcii in 1:30) {
        mod <- lm(as.formula(paste("gloimpact ~ 0 +", formpred)), data=subset(allres4, mc == mcii))
        coeffs <- coef(mod)
        coeffsmc <- rbind(coeffsmc, data.frame(pred=predname, option=substring(names(coeffs), nchar(formpred) + 1), coef=as.numeric(coeffs)))
    }
    results <- coeffsmc %>% group_by(pred, option) %>% summarize(mu=mean(coef), ci25=quantile(coef, .25), ci75=quantile(coef, .75)) %>%
        left_join(allres4 %>% group_by(across(all_of(pred))) %>% summarize(papers=length(unique(paper)),
                                                                           models=length(unique(paste(paper, name)))), by=c('option'=pred))
    if (!is.null(levels)) {
        results$option <- factor(results$option, levels=c(levels, unique(results$option)[!(unique(results$option) %in% levels)]))
        results <- results %>% arrange(option)
        results$option <- as.character(results$option)
    } else {
        results <- results %>% arrange(models)
    }

    results
}

pdf <- rbind(get.coeffs('Weather weight', "Predictor weighting"),
             get.coeffs("Rich/Poor", "Rich/Poor Distinction", c('NA'='Pooled')),
             get.coeffs("Temp", "Temperature", c("10 Lags"="Lags", "5 Lags"="Lags", "1 Lag"="Lags",
                                                 "DT, LDT"="First Difference", "FD"="First Difference", "LT, DT"="First Difference",
                                                 "DT, LDT, DT:T, LDT:T, LT"="Interacted FDs",
                                                 "DT, LDT, DT:T, LDT:T, LT, LT2"="Interacted FDs",
                                                 "DT, LDT, DT:T, LDT:T, T"="Interacted FDs",
                                                 "DT, LDT, DT:T, LDT:T, T, T2"="Interacted FDs",
                                                 "VarT, DT, LDT, DT:T, LDT:LT"="Interacted FDs",
                                                 "Linear Spline"="Other", "Symmetric Spline"="Other", "Cubic"="NA",
                                                 "T1, LT1, T2, LT2"="Lags",
                                                 'Quad'='Quadratic', 'Linear by country, Lag by country'="Other",
                                                 'Deviations'="Other")),
             get.coeffs("Prec.", "Precipitation", c("10 Lags"="Lags", "5 Lags"="Lags", "1 Lag"="Lags",
                                                    "DP, LDP"="First Difference", "FD"="First Difference", "LP, DP"="First Difference",
                                                    'DT, LDT'="First Difference",
                                                    "DP, LDP, DP:P, LDP:P, LP"="Interacted FDs",
                                                    "DP, LDP, DP:P, LDP:P, LP, LP2"="Interacted FDs",
                                                    "DP, LDP, DP:P, LDP:P, P"="Interacted FDs",
                                                    "DP, LDP, DP:P, LDP:P, P, P2"="Interacted FDs",
                                                    'Quad'='Quadratic', 'No'='None', 'NA'='None',
                                                    'Linear by country, Lag by country'="Other",
                                                    "Linear Spline"="Other", "Symmetric Spline"="Other", "Indicatators"="Other")),
             get.coeffs("Year FE", "Year FE", c('By Continent'='By Region', 'Yes'='Common', 'No'='None', 'NA'='None')),
             get.coeffs("Trends", "Trends", c('No'='None', 'NA'='None')), get.coeffs("Other FE", "Other FE", c('NA'='None')),
             get.coeffs("Other Controls", "Other Controls", c('NA'='None'),
                        c("None", "1 Control", "2 Controls", "3 Controls", "≥4 Controls")),
             get.coeffs("Growth Lags", "Growth Lags", levels=c("0", "1", "2", "4")),
             get.coeffs("f.first.year", "First Data Year", c('1955'='1950', '1961'='1960', '1962'='1960', '1991'='1990', '1851'='≤ 1900',
                                                             '1900'='≤ 1900'),
                        c('≤ 1900', '1950', '1960', '1980', '1990')),
             get.coeffs("f.last.year", "Last Data Year", c('2000'='≤ 2005', '2003'='≤ 2005', '2005'='≤ 2005', '2004'='≤ 2005',
                                                           '2010'='2010-2012', '2011'='2010-2012', '2012'='2010-2012',
                                                           '2015'='2015-2018', '2017'='2015-2018', '2018'='2015-2018',
                                                           '2019'='2019-2021', '2021'='2019-2021'),
                        c('≤ 2005', '2010-2012', '2014', '2015-2018', '2019-2021')),
             get.coeffs("f.year.length", "Data Year Length", c('16'='< 30', '22'='< 30', '25'='< 30', '26'='< 30', '28'='< 30',
                                                               '30'='30-49', '31'='30-49', '35'='30-49', '40'='30-49', '42'='30-49', '43'='30-49', '49'='30-49',
                                                               '50'='50-59', '51'='50-59', '53'='50-59', '55'='50-59', '57'='50-59', '59'='50-59',
                                                               '60'='60-69', '62'='60-69', '65'='60-69', '66'='60-69',
                                                               '115'='≥ 70', '168'='≥ 70'),
                        c('< 30', '30-49', '50-59', '60-69', '≥ 70')))

pdf$pred <- factor(pdf$pred, levels=c('Predictor weighting', 'Rich/Poor Distinction', 'Temperature', 'Precipitation', 'Year FE', 'Trends', 'Other FE', 'Other Controls', 'Growth Lags', 'First Data Year', 'Last Data Year', 'Data Year Length'))
pdf$option <- factor(pdf$option, levels=pdf$option[!duplicated(pdf$option)])

pdf2 <- pdf %>% left_join(pdf %>% filter(models > 1 & !is.na(option) & option != 'NA') %>% group_by(pred) %>% summarize(lhs=min(ci25), rhs=max(ci75)))

gp <- ggplot(subset(pdf2, models > 1 & !is.na(option) & option != 'NA'), aes(option, mu)) +
    facet_wrap(~ pred, scales='free', ncol=2) + coord_flip() +
    geom_hline(yintercept=0, alpha=.5) +
    geom_errorbar(aes(ymin=ci25, ymax=ci75)) + geom_point() +
    geom_text(aes(y=rhs + (rhs - lhs) * .1, label=paste0(models, '/', papers)), size=2) +
    theme_bw() + xlab(NULL) + ylab("Global population-weighted GDP loss") +
    theme(plot.margin = margin(.1, .5, .1, .1, "cm")) +
    scale_y_continuous(expand=expansion(mult=c(0.05, .1)))
ggsave("figures/bypredictor.pdf", width=6.5, height=8)
