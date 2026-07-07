## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
source("src/lib/myPBSmapping.R")
library(ggplot2)
library(Hmisc)
library(xtable)
library(mice)

do.fullmc <- T
do.lagonly <- T

remove.lags <- function(dimpact, lags, omega) { # lags is K-1 in my notation
    if (lags == 0)
        return(dimpact)
    result <- dimpact[1]
    withpersist <- dimpact[1]
    for (tt in 1:lags) {
        result[tt+1] <- dimpact[tt+1] - (1 - omega) * withpersist[tt]
        withpersist[tt+1] <- dimpact[tt+1]
    }
    for (tt in (lags + 1):(length(dimpact)-1)) {
        nn <- tt - lags
        result[tt+1] <- dimpact[tt+1] + sum((1 - omega)^(lags + 1 + (1:nn)) * dimpact[nn - (1:nn) + 1]) - (1 - omega) * withpersist[tt]
        withpersist[tt+1] <- result[tt+1] + (1 - omega) * withpersist[tt]
    }

    return(result)
}

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

if (!do.fullmc) {
    allres <- read.csv("data/allres.csv")
    allres2 <- rbind(allres %>% filter(paper != "Kotz et al. 2022" & preferred) %>% select(!c(projs, bases)),
                     allres %>% filter(paper == "Kotz et al. 2022" & preferred) %>% select(!c(projs, bases)) %>%
                     group_by(name, paper, contemp.only, ISO) %>% mutate(Year=Year, dimpact=c(dimpact[1], diff(dimpact))))
    allres2$mc <- 0
} else {
    mem.maxVSize(Inf)

    load("data/mcres.RData")
    allres2 <- rbind(mcres %>% filter(paper != "Kotz et al. 2022") %>% select(!c(projs, bases)),
                     mcres %>% filter(paper == "Kotz et al. 2022") %>% select(!c(projs, bases)) %>%
                     group_by(name, paper, contemp.only, ISO, mc) %>% mutate(Year=Year, dimpact=c(dimpact[1], diff(dimpact))))
    rm(mcres)
}

source("src/lib/loadmetadata.R")

micemodel <- mice(metadata[, c(grep("Q.", names(metadata)), grep("R2", names(metadata)))])
metadata2 <- complete(micemodel)

if (!do.lagonly) {
    metadata2$lags <- c("1 Lag"=1, "10 Lags"=10, "5 Lags"=5, "Average 1986-2000 -  Average 1970-1985"=15,
                        "Cubic"=0, "Deviations"=1, "DT, LDT"=2, "DT, LDT, DT:T, LDT:T, LT"=2,
                        "DT, LDT, DT:T, LDT:T, LT, LT2"=2, "DT, LDT, DT:T, LDT:T, T"=2, "DT, LDT, DT:T, LDT:T, T, T2"=2,
                        "FD"=1, "Interacted with average"=1, "Linear"=0, "Linear by country, Lag by country"=1,
                        "Linear Spline"=0, "LT, DT"=1, "Quad"=0, "Quad, 1 Lag"=1, "Symmetric Spline"=0,
                        "T1, LT1, T2, LT2"=1, "Tx, Tx T, T, Tvar"=0, "VarT, DT, LDT, DT:T, LDT:LT"=2, "Z-score"=1)[metadata$Temp]
} else {
    metadata2$lags <- c("1 Lag"=1, "10 Lags"=10, "5 Lags"=5, "Average 1986-2000 -  Average 1970-1985"=0,
                        "Cubic"=0, "Deviations"=0, "DT, LDT"=1, "DT, LDT, DT:T, LDT:T, LT"=1,
                        "DT, LDT, DT:T, LDT:T, LT, LT2"=1, "DT, LDT, DT:T, LDT:T, T"=1, "DT, LDT, DT:T, LDT:T, T, T2"=1,
                        "FD"=0, "Interacted with average"=0, "Linear"=0, "Linear by country, Lag by country"=1,
                        "Linear Spline"=0, "LT, DT"=1, "Quad"=0, "Quad, 1 Lag"=1, "Symmetric Spline"=0,
                        "T1, LT1, T2, LT2"=1, "Tx, Tx T, T, Tvar"=0, "VarT, DT, LDT, DT:T, LDT:LT"=1, "Z-score"=0)[metadata$Temp]
}
metadata2$paper <- metadata$Paper
metadata2$name <- metadata$Name

remove.lags(rep(1, 10), 5, 0)

## Drop same models as r2-weighted.R
allres3 <- subset(allres2, (paper != "Zhao et al. 2018" | !(name %in% c("Table 3, Col. 2", "Table 3, Col. 3", "Table 3, Col. 5", "Table 3, Col. 6", "Table 3, Col. 7"))) &
                           (paper != "Kotz et al. 2022" | name != "With Linear Trends"))
rm(allres2)

results <- data.frame()
allisos <- data.frame()
for (persist in c(0.6, 0.36, 0.78, 0, 1)) {
    print(persist)
    allres4 <- allres3 %>% left_join(metadata2[, c('paper', 'name', 'Total R2', 'lags')], by=c('paper', 'name')) %>%
        group_by(name, paper, contemp.only, ISO, mc) %>%
        mutate(dimpact.orig=dimpact, dimpact.xlag=remove.lags(dimpact, lags[1], persist))

    allres5 <- allres4 %>% group_by(ISO, Year, mc) %>%
        dplyr::summarize(dimpact.orig=wtd.mean(dimpact.orig, `Total R2`, na.rm=T),
                         dimpact.xlag=wtd.mean(dimpact.xlag, `Total R2`, na.rm=T))
    rm(allres4)

    allres6 <- allres5 %>%
        group_by(ISO, mc) %>%
        mutate(totimpact.orig=stats::filter(c(rep(0, 30), dimpact.orig), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30],
               totimpact.xlag=stats::filter(c(rep(0, 30), dimpact.xlag), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30])

    allisos <- rbind(allisos, cbind(persist=persist, allres6 %>%
                                                     group_by(ISO, Year) %>%
                                                     dplyr::summarize(mu.orig=mean(totimpact.orig, na.rm=T),
                                                                      mu.xlag=mean(totimpact.xlag, na.rm=T)) %>%
                                                     mutate(mu.orig=stats::filter(c(rep(0, 9), mu.orig), rep(1/10, 10), method='conv')[5:(length(mu.orig)+4)],
                                                            mu.xlag=stats::filter(c(rep(0, 9), mu.xlag), rep(1/10, 10), method='conv')[5:(length(mu.xlag)+4)])))

    allres7 <- allres6 %>%
        left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
        group_by(Year, mc) %>%
        dplyr::summarize(totimpact.orig.pop=sum(totimpact.orig * POP_EST, na.rm=T) / sum(POP_EST[!is.na(totimpact.orig)]),
                         totimpact.xlag.pop=sum(totimpact.xlag * POP_EST, na.rm=T) / sum(POP_EST[!is.na(totimpact.xlag)])) %>%
        group_by(Year) %>%
        dplyr::summarize(mu.orig=mean(totimpact.orig.pop, na.rm=T),
                         ci25.orig=quantile(totimpact.orig.pop, .25, na.rm=T),
                         ci75.orig=quantile(totimpact.orig.pop, .75, na.rm=T),
                         mu.xlag=mean(totimpact.xlag.pop, na.rm=T),
                         ci25.xlag=quantile(totimpact.xlag.pop, .25, na.rm=T),
                         ci75.xlag=quantile(totimpact.xlag.pop, .75, na.rm=T)) %>%
        mutate(mu.orig=stats::filter(c(rep(0, 9), mu.orig), rep(1/10, 10), method='conv')[5:(length(mu.orig)+4)],
               ci25.orig=stats::filter(c(rep(0, 9), ci25.orig), rep(1/10, 10), method='conv')[5:(length(ci25.orig)+4)],
               ci75.orig=stats::filter(c(rep(0, 9), ci75.orig), rep(1/10, 10), method='conv')[5:(length(ci75.orig)+4)],
               mu.xlag=stats::filter(c(rep(0, 9), mu.xlag), rep(1/10, 10), method='conv')[5:(length(mu.xlag)+4)],
               ci25.xlag=stats::filter(c(rep(0, 9), ci25.xlag), rep(1/10, 10), method='conv')[5:(length(ci25.xlag)+4)],
               ci75.xlag=stats::filter(c(rep(0, 9), ci75.xlag), rep(1/10, 10), method='conv')[5:(length(ci75.xlag)+4)])

    results <- rbind(results, cbind(persist=persist, allres7))
}

if (!do.lagonly) {
    allisos$mu.diff <- allisos$mu.xlag - allisos$mu.orig

    tbl <- data.frame()
    for (persist in sort(unique(allisos$persist))) {
        allisos.pp <- allisos[allisos$persist == persist,]
        tbl <- rbind(tbl, cbind(persist=persist, as.data.frame(as.data.frame(t(quantile(allisos.pp$mu.diff * 100, c(0, .05, .5, .95, 1))))),
                                miniso=allisos.pp$ISO[which.min(allisos.pp$mu.diff)],
                                frac.miniso=100 * (allisos.pp$mu.xlag[which.min(allisos.pp$mu.diff)] / allisos.pp$mu.orig[which.min(allisos.pp$mu.diff)] - 1),
                                maxiso=allisos.pp$ISO[which.max(allisos.pp$mu.diff)],
                                frac.maxiso=100 * (allisos.pp$mu.xlag[which.max(allisos.pp$mu.diff)] / allisos.pp$mu.orig[which.max(allisos.pp$mu.diff)] - 1)))
    }

    print(xtable(tbl, digits=2), include.rownames=F)

    results %>% filter(Year == 2023) %>% mutate(delta=1 - mu.xlag / mu.orig)
}

if (do.lagonly) {
    save(results, allisos, file="data/diagnostics/remove-lags-lagonly.RData")
} else {
    save(results, allisos, file="data/diagnostics/remove-lags.RData")
}

load("data/diagnostics/remove-lags-lagonly.RData")
results.lagonly <- results
load("data/diagnostics/remove-lags.RData")

results2 <- rbind(cbind(group="Lag-Aware Persistence -\nPlausible extreme", results), cbind(group="Lag-Aware Persistence -\nExplicit-only", results.lagonly))
group.order <- rev(c("Main Approach", "Lag-Aware Persistence -\nExplicit-only", "Lag-Aware Persistence -\nPlausible extreme"))
results2$group <- factor(results2$group, levels=group.order)

ggplot(results, aes(Year)) +
    facet_wrap(~ paste("omega =", persist), scales='free_y', ncol=1) +
    coord_cartesian(xlim=c(1960, 2023)) +
    geom_line(aes(y=mu.orig, colour=factor("Main Approach", levels=group.order)), linetype='solid') +
    geom_line(data=results2, aes(y=mu.xlag, colour=group), linetype='dashed') +
    geom_ribbon(aes(ymin=ci25.orig, ymax=ci75.orig), alpha=.5) +
    scale_colour_manual(name="Handling of Lags", breaks=group.order, values=rev(c('#d95f02', '#1b9e77', '#7570b3'))) +
    scale_y_continuous("Change in GDP due to climate change (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) +
    theme_bw()
ggsave("figures/remove-lags.pdf", width=6.5, height=7)

results2 %>% filter(Year == 2023) %>% group_by(group) %>% mutate(delta=1 - mu.xlag / mu.orig)
