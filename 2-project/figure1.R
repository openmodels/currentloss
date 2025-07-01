library(PBSmapping)
library(dplyr)

source("lib/loadutils.R")

persist <- 0.21
results <- read.metaanal("mcrfres-0.21")

polydata <- attr(importShapefile("../data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

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

ggplot(results2, aes(Year, mu)) +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + scale_y_continuous("Global population-weighted GDP loss", labels=scales::percent) +
    scale_x_continuous(NULL, limits=c(1950, 2022), expand=c(0, 0)) +
    guides(linetype=F)
ggsave(paste0("../results/randforest-", persist, ".pdf"), width=6.5, height=5)

## Combined figure
load("../data/mcres.RData")
load("../data/mcres-decumul.RData")

allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[as.character(persist)]])

allres2 <- allres %>% filter(!is.na(dimpact)) %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST)) %>%
    group_by(paper, name, Year) %>% summarize(mu=mean(gloimpact))

allres3 <- allres2 %>% group_by(Year) %>% summarize(mu=median(mu, na.rm=T))

labels <- data.frame(Year=c(2018, 2018), xend=c(1997, 1997),
                     y=c(results2$mu[results2$Year == 2018], allres3$mu[allres3$Year == 2018]),
                     yend=c(-.035, .015), label=c("Random Forest", "Median Model"))

ggplot(allres2, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.05, .025)) +
    geom_line(aes(colour=paper, group=paste(paper, name)), linewidth=.3) +
    geom_line(data=allres3, size=2, colour='black', alpha=.75) +
    geom_segment(data=labels[2,], aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels[2,], aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1940, 2023)) +
    scale_colour_discrete("Reference:")
ggsave(paste0("../results/allimpacts-", persist, ".pdf"), width=8, height=4)

ggplot(allres2, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.05, .025)) +
    geom_line(aes(colour=paper, group=paste(paper, name)), linewidth=.3) +
    geom_line(data=allres3, size=2, colour='black', alpha=.75) +
    geom_line(data=results2, size=2, colour='#b15928', alpha=.75) +
    geom_segment(data=labels, aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels, aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1940, 2023)) +
    scale_colour_discrete("Reference:")
ggsave(paste0("../results/allimpacts-withrf-", persist, ".pdf"), width=8, height=4)

ggplot(allres2, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.05, .025)) +
    geom_line(aes(colour=paper, group=paste(paper, name)), linewidth=0) +
    geom_line(data=allres3, size=2, colour='black', alpha=.75) +
    geom_line(data=results2, size=2, colour='#b15928', alpha=.75) +
    geom_ribbon(data=results2, aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_segment(data=labels, aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels, aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1940, 2023)) +
    scale_colour_discrete("Reference:")
ggsave(paste0("../results/allimpacts-withrfci-", persist, ".pdf"), width=8, height=4)

### Figure 1 elements
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(PBSmapping)
library(dplyr)
library(ggplot2)

persist <- 0.21
polydata <- attr(importShapefile("../data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

load("../data/mcres.RData")
load("../data/mcres-decumul.RData")

allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[as.character(persist)]])

allres2 <- allres %>% filter(!is.na(dimpact)) %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST)) %>%
    group_by(paper, name, Year) %>% summarize(mu=mean(gloimpact))

## (a) All individual timeseries

legend.papers <- unique(allres2$paper)
legend.alphas <- rep(1, length(legend.papers))
legend.alphas[legend.papers == "Kotz et al. 2022"] <- .5

allres2.smooth <- rbind(allres2 %>% group_by(paper, name) %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)]))

ggplot(allres2.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.035, .005)) +
    geom_line(aes(colour=paper, group=paste(paper, name), alpha=paper), linewidth=.3) +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_discrete("Reference:") +
    scale_alpha_manual("Reference:", breaks=legend.papers, values=legend.alphas) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.8, 'lines')) +
    ggtitle("(a) Population-weighted mean of model projections")
ggsave("../results/figure1a.pdf", width=5, height=3.5)

## Number for paper:
range((allres2.smooth %>% group_by(paper, name) %>% summarize(mu=tail(mu, 1)))$mu)

## (b) All meta-analysis options
load.metaanal <- function(filebase) {
    results <- read.metaanal(filebase)

    results2 <- results %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
        group_by(Year, mc) %>% filter(!is.na(dimpact)) %>% summarize(gloimpact=sum(dimpact * POP_EST) / sum(POP_EST)) %>%
        group_by(Year) %>% summarize(mu=mean(gloimpact, na.rm=T), ci25=quantile(gloimpact, .25, na.rm=T), ci75=quantile(gloimpact, .75, na.rm=T))
    results2
}

allmeta <- data.frame()

paperweight.names <- list("mainmed"="Median of main spec.", "main"="Monte Carlo over main spec.", "all"="Monte Carlo over all spec.")
for (sample.approach in c("mainmed", "main", "all")) {
    results2 <- load.metaanal(paste0("mcpaperres-", persist, "-", sample.approach))
    results2$name <- paperweight.names[[sample.approach]]
    allmeta <- rbind(allmeta, results2)
}

rf.names <- list("controls"="RF with controls criteria",
                 "nonlinear"="RF with nonlinearity criteria", "dataset"="RF with dataset criteria", "all"="RF with all quality criteria")
for (rf.approach in c("all", "controls", "nonlinear", "dataset")) {
    if (rf.approach == 'all')
        results2 <- load.metaanal(paste0("mcrfres-", persist))
    else
        results2 <- load.metaanal(paste0("mcrfres-", persist, "-", rf.approach))
    results2$name <- rf.names[[rf.approach]]
    allmeta <- rbind(allmeta, results2)
}

allmeta$name <- factor(allmeta$name, levels=as.character(c(paperweight.names, rf.names)))

allmeta.smooth <- rbind(allmeta %>% group_by(name) %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)],
                                                              ci25=stats::filter(c(rep(0, 9), ci25), rep(1/10, 10), method='conv')[5:(length(ci25)+4)],
                                                              ci75=stats::filter(c(rep(0, 9), ci75), rep(1/10, 10), method='conv')[5:(length(ci75)+4)]))

ggplot(allmeta.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.017, .001)) +
    geom_line(aes(colour=name, linetype=grepl("RF", name))) +
    geom_ribbon(data=subset(allmeta.smooth, name == "RF with all quality criteria"), aes(ymin=ci25, ymax=ci75), alpha=.25) +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_discrete("Meta-analysis:") +
    scale_linetype_manual(name="Method:", breaks=c(F, T), values=c('dashed', 'F1'), labels=c("Resampling", "Random forest")) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.8, 'lines')) +
    ggtitle("(b) Population-weighted mean of meta-analyses") +
    theme(legend.box="horizontal", legend.box.just="bottom") +
    guides(colour=guide_legend(order=1),linetype=guide_legend(order=2))
ggsave("../results/figure1b.pdf", width=5, height=3.5)

## Range for paper
subset(allmeta.smooth, Year == 2023)

## (c) Countries by reference
polydata$gdppc <- 1e6 * polydata$GDP_MD / polydata$POP_EST
gdplims <- quantile(polydata$gdppc, c(.25, .75), na.rm=T)
polydata$gdppc[polydata$ADM0_A3 == "KAS"] <- NA # Disallow
gdpisos <- c(polydata$ADM0_A3[which.min(abs(polydata$gdppc - gdplims[1]))], polydata$ADM0_A3[which.min(abs(polydata$gdppc - gdplims[2]))])

temps <- read.csv("../data/era5-t2m-combo-adm0.csv") %>% group_by(ISO) %>% dplyr::summarize(t2m=mean(t2m))
t2mlims <- quantile(temps$t2m, c(.25, .75))
t2misos <- c(temps$ISO[which.min(abs(temps$t2m - t2mlims[1]))], temps$ISO[which.min(abs(temps$t2m - t2mlims[2]))])

baarsch <- subset(allres, paper == "Baarsch et al. 2020")
baarsch <- unique(baarsch$ISO[!is.na(baarsch$dimpact)])
polydata[polydata$ADM0_A3 %in% baarsch, c('ADMIN', 'gdppc')]
as.data.frame(temps[temps$ISO %in% baarsch, c('ISO', 't2m')])

gdpisos.baarsch <- c(polydata$ADM0_A3[which.min(ifelse(polydata$ADM0_A3 %in% baarsch, abs(polydata$gdppc - gdplims[1]), NA))],
                     polydata$ADM0_A3[which.min(ifelse(polydata$ADM0_A3 %in% baarsch, abs(polydata$gdppc - gdplims[2]), NA))])
t2misos.baarsch <- c(temps$ISO[which.min(ifelse(temps$ISO %in% baarsch, abs(temps$t2m - t2mlims[1]), NA))],
                     temps$ISO[which.min(ifelse(temps$ISO %in% baarsch, abs(temps$t2m - t2mlims[2]), NA))])

allres.end <- allres %>% filter(Year > 2013) %>% group_by(paper, name, ISO, mc) %>%
    summarize(dimpact=mean(dimpact, na.rm=T)) %>% group_by(paper, name, ISO) %>%
    summarize(dimpact=mean(dimpact, na.rm=T)) # can do as 1 step, but clearer

allres.end$is.main <- F
main.models <- list("Dell et al. 2012"="Main 2.3", "Burke et al. 2015"="Main", "Callahan & Mankin 2022"="Main",
                    "Pretis et al. 2018"="M2", #"Baarsch et al. 2020"="Current",
                    "Acevedo et al. 2020"="column_5",
                    "Kahn et al. 2021"="Table 2, Spec. 1, m = 30, HPJ-FE", "Kotz et al. 2022"="Main",
                    "Kalkuhl & Wenz 2020"="Table 4, Spec. 5",
                    "Sequeira et al. 2018"="Table 5, Spec. 1 & 2, 4 & 5")
for (ii in 1:length(main.models))
    allres.end$is.main[allres.end$paper == names(main.models)[ii] & allres.end$name == main.models[[ii]]] <- T

allres.end %>% filter(is.main & paper != "Baarsch et al. 2020") %>% group_by(paper) %>%
    summarize(num=max(table(ISO)))

allres.end.sum <- rbind(allres.end %>% filter(is.main & paper != "Baarsch et al. 2020") %>% group_by(paper) %>%
                        summarize(ymin=min(dimpact, na.rm=T), ymax=max(dimpact, na.rm=T), gdp.lo=dimpact[ISO == gdpisos[1]], gdp.hi=dimpact[ISO == gdpisos[2]],
                                  t2m.lo=dimpact[ISO == t2misos[1]], t2m.hi=dimpact[ISO == t2misos[2]], yy=tail(allres2.smooth$mu[allres2.smooth$paper == paper[1] & allres2.smooth$name == name[1]], 1)),
                        allres.end %>% filter(is.main & paper == "Baarsch et al. 2020") %>% group_by(paper) %>%
                        summarize(ymin=min(dimpact, na.rm=T), ymax=max(dimpact, na.rm=T), gdp.lo=dimpact[ISO == gdpisos.baarsch[1]],
                                  gdp.hi=dimpact[ISO == gdpisos.baarsch[2]], t2m.lo=dimpact[ISO == t2misos.baarsch[1]],
                                  t2m.hi=dimpact[ISO == t2misos.baarsch[2]], yy=tail(allres2.smooth$mu[allres2.smooth$paper == paper[1] & allres2.smooth$name == name[1]], 1)))

allres.end.sum$paper <- factor(allres.end.sum$paper, levels=rev(unique(allres2.smooth$paper)))

ggplot(allres.end.sum, aes(paper, yy)) +
    coord_flip() +
    geom_pointrange(aes(ymin=ymin, ymax=ymax)) +
    geom_point(aes(y=yy, colour="Pop-weighted")) +
    geom_point(aes(y=gdp.lo, colour="Income Q1")) +
    geom_point(aes(y=gdp.hi, colour="Income Q3")) +
    geom_point(aes(y=t2m.lo, colour="Temperature Q1")) +
    geom_point(aes(y=t2m.hi, colour="Temperature Q3")) +
    theme_bw() +
    scale_x_discrete(NULL) +
    scale_y_continuous("Direct Impact (change in growth rate)", limits=c(-.075, .065), labels=scales::percent) +
    scale_colour_manual("Statistic:", breaks=c("Pop-weighted", "Income Q1", "Income Q3", "Temperature Q1", "Temperature Q3"),
                        values=c('black', '#a6cee3', '#1f78b4', '#b2df8a', '#33a02c')) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.8, 'lines'))
ggsave("../results/figure1c.pdf", width=5, height=3.5)

## Number for paper: Range of expected losses
(exp(range(allres.end.sum$yy)) - 1) * 100

load.metaanal2 <- function(filebase) {
    results <- read.metaanal(filebase)

    results2 <- results %>% filter(Year > 2013) %>% group_by(ISO, mc) %>%
        summarize(dimpact=mean(dimpact, na.rm=T)) %>% group_by(ISO) %>%
        summarize(dimpact=mean(dimpact, na.rm=T)) # can do as 1 step, but clearer

    results2
}

allmeta2 <- data.frame()
for (sample.approach in c("mainmed", "main", "all")) {
    results2 <- load.metaanal2(paste0("mcpaperres-", persist, "-", sample.approach))
    results2$name <- paperweight.names[[sample.approach]]
    allmeta2 <- rbind(allmeta2, results2)
}

for (rf.approach in c("all", "controls", "nonlinear", "dataset")) {
    if (rf.approach == 'all')
        results2 <- load.metaanal2(paste0("mcrfres-", persist))
    else
        results2 <- load.metaanal2(paste0("mcrfres-", persist, "-", rf.approach))
    results2$name <- rf.names[[rf.approach]]
    allmeta2 <- rbind(allmeta2, results2)
}

allmeta2.sum <- allmeta2 %>% group_by(name) %>%
    summarize(ymin=min(dimpact, na.rm=T), ymax=max(dimpact, na.rm=T), gdp.lo=dimpact[ISO == gdpisos[1]], gdp.hi=dimpact[ISO == gdpisos[2]],
              t2m.lo=dimpact[ISO == t2misos[1]], t2m.hi=dimpact[ISO == t2misos[2]],
              yy=tail(allmeta$mu[allmeta$name == name[1]], 1))

allmeta2.sum$name <- factor(allmeta2.sum$name, levels=rev(levels(allmeta$name)))

## ggplot(allmeta2.sum, aes(name, yy)) +
##     coord_flip() +
##     geom_pointrange(aes(ymin=ymin, ymax=ymax)) +
##     geom_point(aes(y=yy, colour="Pop-weighted")) +
##     geom_point(aes(y=gdp.lo, colour="Income Q1")) +
##     geom_point(aes(y=gdp.hi, colour="Income Q3")) +
##     geom_point(aes(y=t2m.lo, colour="Temperature Q1")) +
##     geom_point(aes(y=t2m.hi, colour="Temperature Q3")) +
##     theme_bw() +
##     scale_x_discrete(NULL) +
##     scale_y_continuous("Direct Impact (change in growth rate)", limits=c(-.12, .065), labels=scales::percent) +
##     scale_colour_manual("Statistic:", breaks=c("Pop-weighted", "Income Q1", "Income Q3", "Temperature Q1", "Temperature Q3"),
##                         values=c('black', '#a6cee3', '#1f78b4', '#b2df8a', '#33a02c')) +
##     theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.8, 'lines'))
## ggsave("../results/figure1d.pdf", width=5, height=3.5)

allres.end.sum2 <- allres.end.sum
names(allres.end.sum2)[1] <- 'name'

bothspans <- rbind(cbind(panel="Main specifications", allres.end.sum2), cbind(panel="Meta-analyses", allmeta2.sum))

## ggplot(bothspans, aes(name, yy)) +
##     facet_wrap(~ panel, ncol=1, scales="free_y") +
##     coord_flip() +
##     geom_pointrange(aes(ymin=ymin, ymax=ymax)) +
##     geom_point(aes(y=yy, colour="Pop-weighted")) +
##     geom_point(aes(y=gdp.lo, colour="Income Q1")) +
##     geom_point(aes(y=gdp.hi, colour="Income Q3")) +
##     geom_point(aes(y=t2m.lo, colour="Temperature Q1")) +
##     geom_point(aes(y=t2m.hi, colour="Temperature Q3")) +
##     theme_bw() +
##     scale_x_discrete(NULL) +
##     scale_y_continuous("Direct Impact, 2014 - 2023 (change in growth rate)", limits=c(-.12, .065), labels=scales::percent) +
##     scale_colour_manual("Statistic:", breaks=c("Pop-weighted", "Income Q1", "Income Q3", "Temperature Q1", "Temperature Q3"),
##                         values=c('black', '#a6cee3', '#1f78b4', '#b2df8a', '#33a02c')) +
##     theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.8, 'lines'))
## ggsave("../results/figure1cd.pdf", width=5, height=7)

allres.end2 <- subset(allres.end, is.main)[, -c(2, 5)]
names(allres.end2)[1] <- 'name'
botheachs <- rbind(cbind(panel="Main specifications", allres.end2),
                   cbind(panel="Meta-analyses", allmeta2))

ggplot(bothspans, aes(name, yy)) +
    facet_wrap(~ panel, ncol=1, scales="free_y") +
    coord_flip() +
    geom_boxplot(data=botheachs, aes(y=dimpact)) +
    geom_point(aes(y=yy, colour="Pop-weighted")) +
    geom_point(aes(y=gdp.lo, colour="Income Q1")) +
    geom_point(aes(y=gdp.hi, colour="Income Q3")) +
    geom_point(aes(y=t2m.lo, colour="Temperature Q1")) +
    geom_point(aes(y=t2m.hi, colour="Temperature Q3")) +
    theme_bw() +
    scale_x_discrete(NULL, limits=rev) +
    scale_y_continuous("Direct Impact, 2014-2023 (change in growth rate)", limits=c(-.085, .065), labels=scales::percent) +
    scale_colour_manual("Statistic:", breaks=c("Pop-weighted", "Income Q1", "Income Q3", "Temperature Q1", "Temperature Q3"),
                        values=c('#fb9a99', '#a6cee3', '#1f78b4', '#b2df8a', '#33a02c')) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.8, 'lines')) +
    ggtitle("(c) Distribution of end-of-period country impacts")
ggsave("../results/figure1cd.pdf", width=5, height=7)
