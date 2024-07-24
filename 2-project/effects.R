## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(PBSmapping)
library(readxl)
library(dplyr)
library(ggplot2)

persist <- 0.21
load("data/mcres.RData")
load("data/mcres-decumul.RData")

allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[as.character(persist)]])

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

allres2 <- allres %>% filter(!is.na(dimpact) & Year > 2013) %>%
    group_by(paper, name, ISO, mc) %>% summarize(dimpact=mean(dimpact)) %>%
    left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST) / sum(POP_EST))

metadata <- read_xlsx("data/Current Losses Estimate Metadata.xlsx")
metadata <- subset(metadata, !is.na(Paper) & Paper != "Rising & Tahmid")
metadata <- metadata[!duplicated(metadata[, c('Paper', 'Name')]),]

metadata$Name[is.na(metadata$Name)] <- "NA"
metadata$Dependent[is.na(metadata$Dependent)] <- "NA"
metadata$`Weather weight`[is.na(metadata$`Weather weight`)] <- "NA"
metadata$`Weather weight`[grep("Pop.", metadata$`Weather weight`)] <- "Pop. weight"
metadata$`Rich/Poor`[is.na(metadata$`Rich/Poor`)] <- "NA"
metadata$`Rich/Poor`[metadata$`Rich/Poor` == "Project poor only"] <- "Interact"
metadata$Temp[is.na(metadata$Temp)] <- "Quad" # This is Acevedo et al. 2020, always modeled as quad
metadata$Prec....13[is.na(metadata$Prec....13)] <- "NA"
metadata$Prec....13[metadata$Prec....13 == "NA" & metadata$Paper == "Acevedo et al. 2020"] <- "Quad"
metadata$`Year FE`[is.na(metadata$`Year FE`)] <- "No"
metadata$`Trends`[is.na(metadata$`Trends`)] <- "No"
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
metadata$year.length <- metadata$last.year - metadata$first.year + 1
metadata$f.first.year <- factor(metadata$first.year)
metadata$f.last.year <- factor(metadata$last.year)
metadata$f.year.length <- factor(metadata$year.length)

allres3 <- allres2 %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))

summary(lm(gloimpact ~ 0 + Dependent + `Weather weight` + `Rich/Poor` + Temp + Prec....13 + `Year FE` + Trends + `Other FE` + `Other Controls` + `Growth Lags` + f.first.year + f.year.length, data=allres3)) # Don't believe it: mcs

get.coeffs <- function(pred, predname, renames=c()) {
    formpred <- ifelse(grepl(' |/', pred), paste0('`', pred, '`'), pred)
    coeffsmc <- data.frame()
    for (mcii in 1:30) {
        mod <- lm(as.formula(paste("gloimpact ~ 0 +", formpred)), data=subset(allres3, mc == mcii))
        coeffs <- coef(mod)
        coeffsmc <- rbind(coeffsmc, data.frame(pred=predname, option=substring(names(coeffs), nchar(formpred) + 1), coef=as.numeric(coeffs)))
    }
    results <- coeffsmc %>% group_by(pred, option) %>% summarize(mu=mean(coef), ci25=quantile(coef, .25), ci75=quantile(coef, .75)) %>%
        left_join(allres3 %>% group_by(across(all_of(pred))) %>% summarize(papers=length(unique(paper)),
                                                                           models=length(unique(paste(paper, name)))), by=c('option'=pred))
    for (old in names(renames))
        results$option[results$option == old] <- renames[old]
    results %>% arrange(models)
}

pdf <- rbind(get.coeffs('Weather weight', "Predictor weighting"),
             get.coeffs("Rich/Poor", "Rich/Poor Distinction", c('NA'='Pooled')),
             get.coeffs("Temp", "Temperature", c("VarT, DT, LDT, DT:T, LDT:LT"="Interact with lags", 'Quad'='Quadratic')),
             get.coeffs("Prec....13", "Precipitation", c('Quad'='Quadratic', 'NA'='None')), get.coeffs("Year FE", "Year FE", c('NA'='None')),
             get.coeffs("Trends", "Trends", c('NA'='None')), get.coeffs("Other FE", "Other FE", c('NA'='None')),
             get.coeffs("Other Controls", "Other Controls", c('NA'='None')), get.coeffs("Growth Lags", "Growth Lags"),
             get.coeffs("f.first.year", "First Data Year"), get.coeffs("f.last.year", "Last Data Year"), get.coeffs("f.year.length", "Data Year Length"))
pdf$pred <- factor(pdf$pred, levels=c('Predictor weighting', 'Rich/Poor Distinction', 'Temperature', 'Precipitation', 'Year FE', 'Trends', 'Other FE', 'Other Controls', 'Growth Lags', 'First Data Year', 'Last Data Year', 'Data Year Length'))
pdf$option <- factor(pdf$option, levels=pdf$option[!duplicated(pdf$option)])

pdf2 <- pdf %>% left_join(pdf %>% filter(models > 1) %>% group_by(pred) %>% summarize(lhs=min(ci25), rhs=max(ci75)))

ggplot(subset(pdf2, models > 1), aes(option, mu)) +
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
lfedat$Prec....13 <- factor(lfedat$Prec....13, levels=c('Linear', unique(lfedat$Prec....13)[unique(lfedat$Prec....13) != 'Linear']))
lfedat$`Year FE` <- factor(lfedat$`Year FE`, levels=c('No', unique(lfedat$`Year FE`)[unique(lfedat$`Year FE`) != 'No']))
lfedat$Trends <- factor(lfedat$Trends, levels=c('No', unique(lfedat$Trends)[unique(lfedat$Trends) != 'No']))
lfedat$`Other FE` <- factor(lfedat$`Other FE`, levels=c('NA', unique(lfedat$`Other FE`)[unique(lfedat$`Other FE`) != 'NA']))
lfedat$`Other Controls` <- factor(lfedat$`Other Controls`, levels=c('NA', unique(lfedat$`Other Controls`)[unique(lfedat$`Other Controls`) != 'NA']))
lfedat$`Growth Lags` <- factor(lfedat$`Growth Lags`, levels=c('0', unique(lfedat$`Growth Lags`)[unique(lfedat$`Growth Lags`) != '0']))
mod <- felm(mu ~ `Weather weight` + `Rich/Poor` + Temp + Prec....13 + `Year FE` + Trends + `Other FE` + `Other Controls` + `Growth Lags` + first.year + last.year + year.length | paper, data=lfedat, weights=lfedat$invvar)
summary(mod)

library(stargazer)
stargazer(mod, single.row=T)
