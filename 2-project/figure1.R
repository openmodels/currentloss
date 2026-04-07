## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

source("src/lib/myPBSmapping.R")
library(dplyr)
library(ggplot2)
source("src/lib/loadutils.R")

persist <- 0.6
results <- read.metaanal("mcrfres-0.6")

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

results2 <- results %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(Year, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST) / sum(POP_EST)) %>%
    group_by(Year) %>% summarize(mu=mean(gloimpact),
                                 ci25=quantile(gloimpact, .25),
                                 ci75=quantile(gloimpact, .75))

ggplot(results2, aes(Year, mu)) +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + scale_y_continuous("Global population-weighted GDP loss", labels=scales::percent) +
    scale_x_continuous(NULL, limits=c(1950, 2022), expand=c(0, 0)) +
    guides(linetype=F)
ggsave(paste0("figures/randforest-", persist, ".pdf"), width=6.5, height=5)

## Combined figure
allres <- load.allres(persist)

allres2 <- allres %>% filter(!is.na(dimpact)) %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST)) %>%
    group_by(paper, name, Year) %>% summarize(mu=mean(gloimpact))

allres3 <- allres2 %>% group_by(Year) %>% summarize(mu=median(mu, na.rm=T))

allres2.smooth <- rbind(allres2 %>% group_by(paper, name) %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)]))
allres3.smooth <- allres3 %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)])
results2.smooth <- results2 %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)],
                                       ci25=stats::filter(c(rep(0, 9), ci25), rep(1/10, 10), method='conv')[5:(length(ci25)+4)],
                                       ci75=stats::filter(c(rep(0, 9), ci75), rep(1/10, 10), method='conv')[5:(length(ci75)+4)])

load.metaanal <- function(filebase) {
    results <- read.metaanal(filebase)

    results2 <- results %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
        group_by(Year, mc) %>% filter(!is.na(dimpact)) %>% summarize(gloimpact=sum(dimpact * POP_EST) / sum(POP_EST)) %>%
        group_by(Year) %>% summarize(mu=mean(gloimpact, na.rm=T), ci25=quantile(gloimpact, .25, na.rm=T), ci75=quantile(gloimpact, .75, na.rm=T))
    results2
}

results2b <- load.metaanal(paste0("mcr2res-", persist, "-Total R2"))
results2b.smooth <- results2b %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)],
                                         ci25=stats::filter(c(rep(0, 9), ci25), rep(1/10, 10), method='conv')[5:(length(ci25)+4)],
                                         ci75=stats::filter(c(rep(0, 9), ci75), rep(1/10, 10), method='conv')[5:(length(ci75)+4)])

labels <- data.frame(Year=c(2010, 2000, 1990), xend=c(2010, 2000, 1990),
                     y=c(results2.smooth$mu[results2.smooth$Year == 2010], allres3.smooth$mu[allres3.smooth$Year == 2000],
                         results2b.smooth$mu[results2b.smooth$Year == 1990]),
                     yend=c(-.02, .003, -.02), label=c("Random Forest", "Median Model", "R²-Weighted"))

ggplot(allres2, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.035, .005)) +
    geom_line(aes(colour=paper, linetype=paper, group=paste(paper, name)), linewidth=.3) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_colour_manual("Reference:", values=rep(RColorBrewer::brewer.pal(9, "Set1"), 3)) +
    scale_linetype_manual("Reference:", values=rep(c('solid', 'twodash', 'dotted'), each=9)) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.5, 'lines'), legend.text=element_text(size=7)) +
    guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))
ggsave(paste0("figures/allimpacts-nosmooth-", persist, ".pdf"), width=6.5, height=4)

ggplot(allres2.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.035, .005)) +
    geom_line(aes(colour=paper, linetype=paper, group=paste(paper, name)), linewidth=.3) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_colour_manual("Reference:", values=rep(RColorBrewer::brewer.pal(9, "Set1"), 3)) +
    scale_linetype_manual("Reference:", values=rep(c('solid', 'twodash', 'dotted'), each=9)) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.5, 'lines'), legend.text=element_text(size=7)) +
    guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))
ggsave(paste0("figures/allimpacts-", persist, ".pdf"), width=6.5, height=4)

ggplot(subset(allres2.smooth, Year == 2023), aes(mu)) +
    coord_flip(xlim=c(-.035, .005)) +
    geom_histogram(bins=100) + geom_boxplot(aes(y=-5), width=5, outliers=F, coef=1.58) + ylab(NULL) +
    theme_bw() + theme(axis.line.y = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(),
                       axis.title.y = element_blank(),
                       axis.line.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.x = element_blank())
ggsave(paste0("figures/allimpacts-", persist, "-hist.pdf"), width=0.75, height=4)

ggplot(allres2.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.035, .005)) +
    geom_line(aes(colour=paper, linetype=paper, group=paste(paper, name)), linewidth=.3) +
    geom_line(data=allres3.smooth, size=2, colour='black', alpha=.75) +
    geom_segment(data=labels[2,], aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels[2,], aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_manual("Reference:", values=rep(RColorBrewer::brewer.pal(9, "Set1"), 3)) +
    scale_linetype_manual("Reference:", values=rep(c('solid', 'twodash', 'dotted'), each=9)) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.5, 'lines'), legend.text=element_text(size=7)) +
    guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))
ggsave(paste0("figures/allimpacts-withmed-", persist, ".pdf"), width=6.5, height=4)

ggplot(allres2.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.035, .005)) +
    geom_line(aes(colour=paper, linetype=paper, group=paste(paper, name)), linewidth=.3) +
    geom_line(data=allres3.smooth, size=2, colour='black', alpha=.75) +
    geom_line(data=results2.smooth, size=2, colour='#b15928', alpha=.75) +
    geom_segment(data=labels[1:2,], aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels[1:2,], aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_manual("Reference:", values=rep(RColorBrewer::brewer.pal(9, "Set1"), 3)) +
    scale_linetype_manual("Reference:", values=rep(c('solid', 'twodash', 'dotted'), each=9)) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.5, 'lines'), legend.text=element_text(size=7)) +
    guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))
ggsave(paste0("figures/allimpacts-withrf-", persist, ".pdf"), width=6.5, height=4)

ggplot(allres2.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.035, .005)) +
    geom_line(aes(colour=paper, linetype=paper, group=paste(paper, name)), linewidth=.3) +
    geom_line(data=allres3.smooth, size=2, colour='black', alpha=.75) +
    geom_line(data=results2.smooth, size=2, colour='#b15928', alpha=.75) +
    geom_ribbon(data=results2.smooth, aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_segment(data=labels[1:2,], aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels[1:2,], aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_manual("Reference:", values=rep(RColorBrewer::brewer.pal(9, "Set1"), 3)) +
    scale_linetype_manual("Reference:", values=rep(c('solid', 'twodash', 'dotted'), each=9)) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.5, 'lines'), legend.text=element_text(size=7)) +
    guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))
ggsave(paste0("figures/allimpacts-withrf-", persist, "-ci.pdf"), width=6.5, height=4)

ggplot(allres2.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.035, .005)) +
    geom_line(aes(colour=paper, linetype=paper, group=paste(paper, name)), linewidth=.3) +
    geom_line(data=allres3.smooth, size=2, colour='black', alpha=.75) +
    geom_line(data=results2.smooth, size=2, colour='#b15928', alpha=.75) +
    geom_ribbon(data=results2b.smooth, aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_line(data=results2b.smooth, size=2, colour='#ffed6f', alpha=.75) +
    geom_segment(data=labels, aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels, aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_manual("Reference:", values=rep(RColorBrewer::brewer.pal(9, "Set1"), 3)) +
    scale_linetype_manual("Reference:", values=rep(c('solid', 'twodash', 'dotted'), each=9)) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.5, 'lines'), legend.text=element_text(size=7)) +
    guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))
ggsave(paste0("figures/allimpacts-withall-", persist, ".pdf"), width=6.5, height=4)

### Figure 1 elements
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

source("src/lib/myPBSmapping.R")
library(dplyr)
library(ggplot2)
source("src/lib/loadutils.R")

persist <- 0.6
polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

allres <- load.allres(persist)

allres2 <- allres %>% filter(!is.na(dimpact)) %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST)) %>%
    group_by(paper, name, Year) %>% summarize(mu=mean(gloimpact))

## (a) All individual timeseries

legend.papers <- unique(allres2$paper)

allres2.smooth <- rbind(allres2 %>% group_by(paper, name) %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)]))

ggplot(allres2.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.035, .005)) +
    geom_line(aes(colour=paper, linetype=paper, group=paste(paper, name)), linewidth=.3) +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_manual("Reference:", values=rep(RColorBrewer::brewer.pal(9, "Set1"), 3)) +
    scale_linetype_manual("Reference:", values=rep(c('solid', 'twodash', 'dotted'), each=9)) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.5, 'lines'), legend.text=element_text(size=7)) +
    ggtitle("(a) Population-weighted mean of model projections") +
    guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))
ggsave("figures/figure1a.pdf", width=5, height=3.5)

## Number for paper:
(exp(range((allres2.smooth %>% group_by(paper, name) %>% summarize(mu=tail(mu, 1)))$mu)) - 1) * 100
## -6.566008 14.247714

## (b) All meta-analysis options
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

r2.names <- list("Total R2"="R² Filled", "Raw R2"="R² Unfilled")
for (r2col in names(r2.names)) {
    results2 <- load.metaanal(paste0("mcr2res-", persist, "-", r2col))
    results2$name <- r2.names[[r2col]]
    allmeta <- rbind(allmeta, results2)
}

allmeta$name <- factor(allmeta$name, levels=as.character(c(paperweight.names, rf.names, r2.names)))

allmeta.smooth <- rbind(allmeta %>% group_by(name) %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)],
                                                              ci25=stats::filter(c(rep(0, 9), ci25), rep(1/10, 10), method='conv')[5:(length(ci25)+4)],
                                                              ci75=stats::filter(c(rep(0, 9), ci75), rep(1/10, 10), method='conv')[5:(length(ci75)+4)]))

ggplot(allmeta.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.017, .001)) +
    geom_line(aes(colour=name, linetype=ifelse(grepl("RF", name), "Random forest", ifelse(grepl("R²", name), "R²-based", "Resampling")))) +
    geom_ribbon(data=subset(allmeta.smooth, name == "R² Filled"), aes(ymin=ci25, ymax=ci75), alpha=.25) +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_discrete("Meta-analysis:") +
    scale_linetype_manual(name="Method:", breaks=c("Resampling", "Random forest", "R²-based"), values=c('dotted', 'dashed', 'F1')) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.7, 'lines')) +
    ggtitle("(b) Population-weighted mean of meta-analyses") +
    theme(legend.box="horizontal", legend.box.just="bottom") +
    guides(colour=guide_legend(order=1),linetype=guide_legend(order=2))
ggsave("figures/figure1b-r2.pdf", width=5, height=3.5)

ggplot(allmeta.smooth, aes(Year, mu)) +
    coord_cartesian(ylim=c(-.025, .001)) +
    geom_line(aes(colour=name, linetype=ifelse(grepl("RF", name), "Random forest", ifelse(grepl("R²", name), "R²-based", "Resampling")))) +
    geom_ribbon(data=subset(allmeta.smooth, name == "RF with all quality criteria"), aes(ymin=ci25, ymax=ci75), alpha=.25) +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_discrete("Meta-analysis:") +
    scale_linetype_manual(name="Method:", breaks=c("Resampling", "Random forest", "R²-based"), values=c('dotted', 'dashed', 'F1')) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.7, 'lines')) +
    ggtitle("(b) Population-weighted mean of meta-analyses") +
    theme(legend.box="horizontal", legend.box.just="bottom") +
    guides(colour=guide_legend(order=1),linetype=guide_legend(order=2))
ggsave("figures/figure1b-rf.pdf", width=5, height=3.5)

## Range for paper
subset(allmeta.smooth, Year == 2023)
##    Year       mu     ci25     ci75 name
## 1  2023 -0.00338 -0.00394 -0.00257 Median of main spec.
## 2  2023 -0.00654 -0.00720  0.00319 Monte Carlo over main spec.
## 3  2023 -0.00481 -0.00918  0.00151 Monte Carlo over all spec.
## 4  2023 -0.0141  -0.0214  -0.00667 RF with all quality criteria
## 5  2023 -0.0143  -0.0187  -0.00991 RF with controls criteria
## 6  2023 -0.00883 -0.0129  -0.00454 RF with nonlinearity criteria
## 7  2023 -0.00621 -0.00911 -0.00240 R² Filled
## 8  2023 -0.00810 -0.0114  -0.00414 R² Unfilled

library(xtable)
xtable(subset(allmeta.smooth, Year == 2023))

## (c) Countries by reference
polydata$gdppc <- 1e6 * polydata$GDP_MD / polydata$POP_EST
gdplims <- quantile(polydata$gdppc, c(.25, .75), na.rm=T)
polydata$gdppc[polydata$ADM0_A3 == "KAS"] <- NA # Disallow
gdpisos <- c(polydata$ADM0_A3[which.min(abs(polydata$gdppc - gdplims[1]))], polydata$ADM0_A3[which.min(abs(polydata$gdppc - gdplims[2]))])

temps <- read.csv("data/era5-t2m-combo-adm0.csv") %>% group_by(ISO) %>% dplyr::summarize(t2m=mean(t2m))
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
                    "Sequeira et al. 2018"="Table 5, Spec. 1 & 2, 4 & 5",
		    "Zhao et al. 2018"="Table 3, Col. 3",
		    "Damania et al. 2020"="Table 1, Col 1",
		    "Henseler & Schumacher 2019"="Main spec.",
		    "Burke et al. 2018"="Main spec.",
		    "De Vos & Everaert 2021"="Table 5, CCEPbc",
		    "Yang et al. 2023"="Table 6, FE-NLS, 6",
                    "Bareille et al. 2024" = "Table 3, Model 4",
                    "Zhang et al. 2024" = "Table A3",
                    "Meierrieks & Stadelmann 2024" = "Table 2, Column 6",
                    "Apergis & Rehman 2024" = "Table 2",
                    "Brown et al. 2013" = "Table 2, T2W",
                    #"Kahn et al. 2017" = NULL, # Preferred in model 3, with no temperature
                    "Liu et al. 2023" = "Table S1, Lag 1",
                    "Yang et al. 2025" = "Panel B, Covariate-dependent threshold",
                    "Gupta et al. 2024" = "Table 1, Split",
                    "Jiao et al. 2024" = "Adaptation IIS",
                    "Benhamed et al. 2023" = "Table 4, LMI/HI, Contiguity",
                    "Desbordes & Eberhardt 2024" = "Table 3, CCE3, Col 6")
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
    coord_flip(ylim=c(-.075, .065)) +
    geom_pointrange(aes(ymin=ymin, ymax=ymax)) +
    geom_point(aes(y=yy, colour="Pop-weighted")) +
    geom_point(aes(y=gdp.lo, colour="Income Q1")) +
    geom_point(aes(y=gdp.hi, colour="Income Q3")) +
    geom_point(aes(y=t2m.lo, colour="Temperature Q1")) +
    geom_point(aes(y=t2m.hi, colour="Temperature Q3")) +
    theme_bw() +
    scale_x_discrete(NULL) +
    scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_colour_manual("Statistic:", breaks=c("Pop-weighted", "Income Q1", "Income Q3", "Temperature Q1", "Temperature Q3"),
                        values=c('black', '#a6cee3', '#1f78b4', '#b2df8a', '#33a02c')) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.03), legend.key.size=unit(0.8, 'lines'))
ggsave("figures/figure1c.pdf", width=5, height=3.5)

## Number for paper: Range of expected losses
(exp(range(allres.end.sum$yy)) - 1) * 100
## [1] -3.278083  2.176113

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

for (r2col in c("Total R2", "Raw R2")) {
    results2 <- load.metaanal2(paste0("mcr2res-", persist, "-", r2col))
    results2$name <- r2.names[[r2col]]
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
## ggsave("figures/figure1d.pdf", width=5, height=3.5)

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
## ggsave("figures/figure1cd.pdf", width=5, height=7)

allres.end2 <- subset(allres.end, is.main)[, -c(2, 5)]
names(allres.end2)[1] <- 'name'
botheachs <- rbind(cbind(panel="Main specifications", allres.end2),
                   cbind(panel="Meta-analyses", allmeta2))

mylevels <- c(" ", "  ", "   ", "    ", rev(as.character(bothspans$name)))
bothspans$name <- factor(bothspans$name, levels=mylevels)
botheachs$name <- factor(botheachs$name, levels=mylevels)

## ggplot(rbind(data.frame(panel="Meta-analyses", name=factor(c(" ", "  ", "   ", "    "), levels=mylevels), ymin=NA, ymax=NA, gdp.lo=NA, gdp.hi=NA, t2m.lo=NA, t2m.hi=NA, yy=NA), bothspans), aes(name, yy)) +
ggplot(bothspans, aes(name, yy)) +
    facet_wrap(~ panel, ncol=1, scales="free_y") +
    coord_flip() +
    geom_boxplot(data=botheachs, aes(y=dimpact)) +
    geom_point(aes(y=yy, colour="Pop-weighted"), position=position_nudge(x=.2)) +
    geom_point(aes(y=gdp.lo, colour="Income Q1")) +
    geom_point(aes(y=gdp.hi, colour="Income Q3")) +
    geom_point(aes(y=t2m.lo, colour="Temperature Q1"), position=position_nudge(x=-.2)) +
    geom_point(aes(y=t2m.hi, colour="Temperature Q3"), position=position_nudge(x=-.2)) +
    theme_bw() +
    scale_x_discrete(NULL) +
    scale_y_continuous("Direct Impact, 2014-2023 (growth rate change)", limits=c(-.085, .065), labels=scales::percent) +
    scale_colour_manual("Statistic:", breaks=c("Pop-weighted", "Income Q1", "Income Q3", "Temperature Q1", "Temperature Q3"),
                        values=c('#fb9a99', '#a6cee3', '#1f78b4', '#b2df8a', '#33a02c')) +
    ##theme(legend.justification=c(0,0), legend.position=c(.1,.001), legend.key.size=unit(0.8, 'lines'), plot.margin=margin(t=5, r=5, b=20, l=5, unit="pt")) +
    theme(legend.justification=c(0,0), legend.position="bottom", legend.key.size=unit(0.8, 'lines'), plot.margin=margin(t=5, r=5, b=20, l=5, unit="pt")) +
    ggtitle("(c) End-of-period country impacts") +
    guides(colour=guide_legend(ncol=2))
ggsave("figures/figure1cd.pdf", width=5, height=8.15)

## SI figure with panel for each paper
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

source("src/lib/myPBSmapping.R")
library(dplyr)
library(ggplot2)
source("src/lib/loadutils.R")

persist <- 0.6
polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

allres <- load.allres(persist)

allres2 <- allres %>% filter(!is.na(dimpact)) %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year, mc) %>% summarize(gloimpact=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST)) %>%
    group_by(paper, name, Year) %>% summarize(mu=mean(gloimpact), ci25=quantile(gloimpact, .25, na.rm=T), ci75=quantile(gloimpact, .75, na.rm=T))

allres2.smooth <- allres2 %>% group_by(paper, name) %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)],
                                                               ci25=stats::filter(c(rep(0, 9), ci25), rep(1/10, 10), method='conv')[5:(length(ci25)+4)],
                                                               ci75=stats::filter(c(rep(0, 9), ci75), rep(1/10, 10), method='conv')[5:(length(ci75)+4)])
allres2.smooth.label <- allres2.smooth %>% group_by(paper, name) %>% summarize(mu=tail(mu, 1), ci25=tail(ci25, 1), ci75=tail(ci75, 1)) %>%
    group_by(paper) %>% mutate(nnum=1:length(name)) %>% # grab the original order before reordering
    arrange(mu) %>% group_by(paper) %>% mutate(paper.split=ifelse(rep(max(nnum) < 10, length(name)), paper, paste(paper, c('Group 1', 'Group 2')[1:max(nnum) %% 2 + 1]))) %>%
    arrange(nnum) %>% group_by(paper.split) %>% mutate(nnum.split=1:length(name))

max(allres2.smooth.label$nnum.split) # 9
median(allres2.smooth.label$nnum.split) # 3

allres2.smooth.label.dots <- allres2.smooth.label %>% group_by(paper.split, name) %>% reframe(paper=paper[1], nnum.split=nnum.split[1], Year=seq(1960, 2019, by=30) + 3 * (nnum.split * 4) %% 9 + 3) %>%
    left_join(allres2.smooth)
allres2.smooth2 <- allres2.smooth %>% left_join(allres2.smooth.label[, c('paper', 'name', 'paper.split')])

gp <- ggplot(allres2.smooth2, aes(Year, mu)) +
    facet_wrap(~ paper.split, scales='free_y', ncol=3) +
    geom_line(aes(group=paste(paper, name))) +
    geom_label(data=allres2.smooth.label.dots, aes(label=nnum.split), size=1.5) +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023))
ggsave("figures/figure1a-sibig.pdf", width=6.5, height=8)

format.percent <- function(xx) {
    ifelse(is.na(xx), NA, paste0(round(xx * 100, 2), "%"))
}

format.range <- function(x0, x1, ispercent=T) {
    ifelse(is.na(x0), NA, paste0(floor(x0 * 1000) / 10, " - ", ceiling(x1 * 1000) / 10, "%"))
}

allres2.smooth.label2 <- allres2.smooth.label[, c('paper.split', 'nnum.split', 'name')] %>% arrange(paper.split)
allres2.smooth.label2$mu <- format.percent(allres2.smooth.label$mu)
allres2.smooth.label2$ci <- format.range(allres2.smooth.label$ci25, allres2.smooth.label$ci75)
names(allres2.smooth.label2) <- c("Paper Panel", "Index", "Estimate Name", "2014 - 2023 Mean", "2014 - 2023 IQR")

library(xtable)
print(xtable(allres2.smooth.label2), include.rownames=F)

