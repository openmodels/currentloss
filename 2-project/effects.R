## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(dplyr)
library(ggplot2)
source("~/projects/research-common/R/myPBSmapping.R")

persist <- 0.46
load("data/mcres.RData")
load("data/mcres-decumul.RData")

allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[as.character(persist)]])

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

allres2 <- allres %>% filter(!is.na(dimpact) & Year > 2013) %>%
    group_by(paper, name, ISO, mc) %>% summarize(dimpact=mean(dimpact)) %>%
    left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST) / sum(POP_EST))

source("src/lib/loadmetadata.R")
metadata$`Year FE`[metadata$`Year FE` == "NA"] <- "No"
metadata$`Trends`[metadata$`Trends` == "NA"] <- "No"
metadata$`Other Controls`[!(metadata$`Other Controls` %in% c("NA", "Lag Weather"))] <- "Other"
metadata$year.length <- metadata$last.year - metadata$first.year + 1
metadata$f.first.year <- factor(metadata$first.year)
metadata$f.last.year <- factor(metadata$last.year)
metadata$f.year.length <- factor(metadata$year.length)

allres3 <- allres2 %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))

summary(lm(gloimpact ~ 0 + Dependent + `Weather weight` + `Rich/Poor` + Temp + Prec. + `Year FE` + Trends + `Other FE` + `Other Controls` + `Growth Lags` + f.first.year + f.year.length, data=allres3)) # Don't believe it: mcs

get.coeffs <- function(pred, predname, renames=c()) {
    formpred <- ifelse(grepl(' |/', pred), paste0('`', pred, '`'), pred)
    coeffsmc <- data.frame()
    allres4 <- allres3
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
    if (is.factor(allres3[, pred, drop=T])) {
        results$option <- as.numeric(as.character(results$option))
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
                                                 "Symmetric Spline"="Linear Spline", "Cubic"="NA",
                                                 "T1, LT1, T2, LT2"="Lags",
                                                 'Quad'='Quadratic')),
             get.coeffs("Prec.", "Precipitation", c("10 Lags"="Lags", "5 Lags"="Lags", "1 Lag"="Lags",
                                                    "DP, LDP"="First Difference", "FD"="First Difference", "LP, DP"="First Difference",
                                                    'DT, LDT'="First Difference",
                                                    "DP, LDP, DP:P, LDP:P, LP"="Interacted FDs",
                                                    "DP, LDP, DP:P, LDP:P, LP, LP2"="Interacted FDs",
                                                    "DP, LDP, DP:P, LDP:P, P"="Interacted FDs",
                                                    "DP, LDP, DP:P, LDP:P, P, P2"="Interacted FDs",
                                                    "Symmetric Spline"="Linear Spline",
                                                    'Quad'='Quadratic', 'No'='None', 'NA'='None')),
             get.coeffs("Year FE", "Year FE", c('By Continent'='By Region', 'Yes'='Common', 'No'='None', 'NA'='None')),
             get.coeffs("Trends", "Trends", c('No'='None', 'NA'='None')), get.coeffs("Other FE", "Other FE", c('NA'='None')),
             get.coeffs("Other Controls", "Other Controls", c('NA'='None')), get.coeffs("Growth Lags", "Growth Lags"),
             get.coeffs("f.first.year", "First Data Year", c('1955'='1950', '1961'='1960', '1962'='1960')),
             get.coeffs("f.last.year", "Last Data Year", c('2015'='2014', '2012'='2010', '2004'='2003')),
             get.coeffs("f.year.length", "Data Year Length", c('42'='25', '49'='25', '53'='50', '66'='65')))
pdf$pred <- factor(pdf$pred, levels=c('Predictor weighting', 'Rich/Poor Distinction', 'Temperature', 'Precipitation', 'Year FE', 'Trends', 'Other FE', 'Other Controls', 'Growth Lags', 'First Data Year', 'Last Data Year', 'Data Year Length'))
pdf$option <- factor(pdf$option, levels=pdf$option[!duplicated(pdf$option)])

pdf2 <- pdf %>% left_join(pdf %>% filter(models > 1 & !is.na(option) & option != 'NA') %>% group_by(pred) %>% summarize(lhs=min(ci25), rhs=max(ci75)))

gp <- ggplot(subset(pdf2, models > 1 & !is.na(option) & option != 'NA'), aes(option, mu)) +
    facet_wrap(~ pred, scales='free', ncol=2) + coord_flip() +
    geom_errorbar(aes(ymin=ci25, ymax=ci75)) + geom_point() +
    geom_text(aes(y=rhs + (rhs - lhs) * .1, label=paste0(models, '/', papers)), size=2) +
    theme_bw() + xlab(NULL) + ylab("Global population-weighted GDP loss") +
    theme(plot.margin = margin(.1, .5, .1, .1, "cm"))
ggsave("figures/bypredictor.pdf", width=6.5, height=8)

library(lfe)
lfedat <- allres2 %>% group_by(paper, name) %>% summarize(mu=mean(gloimpact), invvar=1 / var(gloimpact)) %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))
lfedat$`Rich/Poor` <- factor(lfedat$`Rich/Poor`, levels=c('NA', unique(lfedat$`Rich/Poor`)[unique(lfedat$`Rich/Poor`) != 'NA']))
lfedat$Temp <- factor(lfedat$Temp, levels=c('Linear', unique(lfedat$Temp)[unique(lfedat$Temp) != 'Linear']))
lfedat$Prec. <- factor(lfedat$Prec., levels=c('Linear', unique(lfedat$Prec.)[unique(lfedat$Prec.) != 'Linear']))
lfedat$`Year FE` <- factor(lfedat$`Year FE`, levels=c('No', unique(lfedat$`Year FE`)[unique(lfedat$`Year FE`) != 'No']))
lfedat$Trends <- factor(lfedat$Trends, levels=c('No', unique(lfedat$Trends)[unique(lfedat$Trends) != 'No']))
lfedat$`Other FE` <- factor(lfedat$`Other FE`, levels=c('NA', unique(lfedat$`Other FE`)[unique(lfedat$`Other FE`) != 'NA']))
lfedat$`Other Controls` <- factor(lfedat$`Other Controls`, levels=c('NA', unique(lfedat$`Other Controls`)[unique(lfedat$`Other Controls`) != 'NA']))
lfedat$`Growth Lags` <- factor(lfedat$`Growth Lags`, levels=c('0', unique(lfedat$`Growth Lags`)[unique(lfedat$`Growth Lags`) != '0']))
mod <- felm(mu ~ `Weather weight` + `Rich/Poor` + Temp + `Prec.` + `Year FE` + Trends + `Other FE` + `Other Controls` + `Growth Lags` + first.year + last.year + year.length | paper, data=lfedat, weights=lfedat$invvar)
summary(mod)

library(stargazer)
stargazer(mod, single.row=T)
