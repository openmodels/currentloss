setwd("~/research/currentloss")

library(dplyr)
library(PBSmapping)
library(ggplot2)

polydata <- attr(importShapefile("~/research/fishnets/shapefiles/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

allres <- read.csv("allres.csv")

allres2 <- allres %>% group_by(paper, name, contemp.only, ISO) %>%
    mutate(disum=cumsum(dimpact), di75p=tail(stats::filter(c(rep(0, 30), dimpact), (1 - .25)^(0:30), sides=1), length(dimpact)))
results <- allres2 %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year, contemp.only) %>%
    summarize(dimpact.pop=sum(dimpact * POP_EST) / sum(POP_EST),
              disum.pop=sum(disum * POP_EST) / sum(POP_EST),
              di75p.pop=sum(di75p * POP_EST) / sum(POP_EST))
results2 <- results %>% group_by(Year, contemp.only) %>%
    summarize(dimpact.pop=median(dimpact.pop, na.rm=T),
              disum.pop=median(disum.pop, na.rm=T),
              di75p.pop=median(di75p.pop, na.rm=T))

ggplot(subset(results, contemp.only == F), aes(Year, dimpact.pop)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    geom_line(data=subset(results2, contemp.only == F), size=2, colour='black') +
    theme_bw() + scale_y_continuous("Impact (percentage point change in growth rate)", labels=scales::percent)
ggsave("figures/allimpacts.pdf", width=8, height=4)

ggplot(subset(results, contemp.only == F), aes(Year, disum.pop)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    geom_line(data=subset(results2, contemp.only == F), size=2, colour='black') +
    theme_bw() + scale_y_continuous("Impact (percentage point change in growth rate)", labels=scales::percent)
ggsave("figures/allimpacts-sum.pdf", width=8, height=4)

ggplot(subset(results, contemp.only == F), aes(Year, di75p.pop)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    geom_line(data=subset(results2, contemp.only == F), size=2, colour='black') +
    theme_bw() + scale_y_continuous("Impact (percentage point change in growth rate)", labels=scales::percent)
ggsave("figures/allimpacts-75p.pdf", width=8, height=4)

## Try to make a model to predict
library(readxl)

metadata <- read_xlsx("Current Losses Estimate Metadata.xlsx")
metadata$Name[is.na(metadata$Name)] <- "NA"
metadata$`Rich/Poor`[is.na(metadata$`Rich/Poor`)] <- "NA"
metadata$`Trends`[is.na(metadata$`Trends`)] <- "NA"
metadata$`Other FE`[is.na(metadata$`Other FE`)] <- "NA"
lastres <- subset(allres, Year == max(Year) & contemp.only == F)
lastres$name[is.na(lastres$name)] <- "NA"

lastres2 <- lastres %>% left_join(metadata, by=c('paper'='Paper', 'name'='Name'))
apply(lastres2, 2, function(ss) sum(is.na(ss)))

library(lfe)

summary(felm(dimpact ~ `Weather weight` + `Rich/Poor` + `Temp` + `Prec....13` + `Year FE` + `Trends` + `Other FE` + `Growth Lags` | ISO, data=lastres2))

## What do I need to add to get in line with the ideal
lastres2$correct <- ifelse(lastres2$`Weather weight` != 'Pop. weight', -0.0038075, 0) +
    ifelse(lastres2$`Rich/Poor` == 'NA', -0.0005447, ifelse(lastres2$`Rich/Poor` == 'Project poor only', 0.0018909, 0)) +
    ifelse(lastres2$Temp %in% c('5 Lags', 'Z-score'), NA,
    ifelse(lastres2$Temp == 'Linear', -0.0092101 - 0.0003836,
    ifelse(lastres2$Temp != 'Quad', -0.0092101, 0))) + # incomplete, and what is my preferred lags?
    ifelse(lastres2$`Year FE` == 'Yes', -0.0107510 - -0.0015189, ifelse(lastres2$`Year FE` == 'No', -0.0107510 - -0.0009133, 0)) +
    ifelse(lastres2$Trends != 'Quad, By Country', -0.0004546, 0) # incomplete

lastres3 <- lastres2 %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name) %>% summarize(dimpact.pop=sum(dimpact * POP_EST) / sum(POP_EST),
                                        dimpact.pop.corr=sum((dimpact + correct) * POP_EST) / sum(POP_EST))
library(reshape2)
lastres4 <- melt(lastres3, c('paper', 'name'))
lastres5 <- rbind(lastres4,
                  cbind(lastres4 %>% group_by(variable) %>% summarize(value=median(value, na.rm=T)), paper='Combined', name='Combined'),
                  data.frame(paper='Combined', name='Combined', variable="Random Forest", value=-0.00664612))

lastres5$label <- "Original"
lastres5$label[lastres5$variable == 'dimpact.pop.corr'] <- "Corrected"
lastres5$label[lastres5$variable == 'Random Forest'] <- "Random Forest"
lastres5$label <- factor(lastres5$label, c('Original', 'Corrected', 'Random Forest'))

ggplot(lastres5, aes(x=label, y=value, colour=paper, group=paste(paper, name))) +
    geom_point(aes(size=paper == 'Combined')) +
    theme_bw() + xlab(NULL) +
    scale_y_continuous("Impact in 2022 (percentage point change in growth rate)", labels=scales::percent)
