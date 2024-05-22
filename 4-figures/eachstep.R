## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(PBSmapping)
library(ggplot2)

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

pdf <- data.frame()
for (persist in c(0, 0.08, 0.21, 1)) {
    if (persist == 0) {
        load(file.path("data/", paste0("mcrfres-0.08.RData")))
        results2 <- results
        results2$totimpact <- results$dimpact
    } else if (persist == 1) {
        load(file.path("data/", paste0("mcrfres-0.08.RData")))
        results2 <- results %>% group_by(ISO, mc) %>%
            mutate(totimpact=cumsum(dimpact))
    } else {
        load(file.path("data/", paste0("mcrfres-", persist, ".RData")))
        results2 <- results %>% group_by(ISO, mc) %>%
            mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - persist)^(0:30), sides=1)[-1:-30])
    }

    results3 <- results2 %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
        group_by(Year, mc) %>% filter(!is.na(totimpact)) %>% summarize(gloimpact=sum(totimpact * POP_EST) / sum(POP_EST))
    results4 <- results3 %>% group_by(Year) %>%
        summarize(mu=mean(gloimpact), ci25=quantile(gloimpact, .25), ci75=quantile(gloimpact, .75))

    pdf <- rbind(pdf, cbind(persist=persist, results4))
}

ggplot(pdf, aes(Year, mu, group=factor(persist))) +
    geom_line(aes(colour=factor(persist))) +
    geom_ribbon(data=subset(pdf, persist == 0.08), aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
    scale_colour_discrete(expression(omega~':')) + xlab(NULL) +
    scale_y_continuous("Direct Impact (% GDP)", labels=scales::percent)
ggsave("figures/eachstep-cumul.pdf", width=2.5, height=2.5)

source("src/lib/loadutils.R")

pdf <- data.frame()
for (slrconf in c('Market-only', 'All Damages', 'Optimal Adapt.', 'No Adaptation')) {
    if (slrconf == 'No SLR') {
        pdf <- rbind(pdf, data.frame(slrconf='No SLR', year=1960:2023, mu=0, ci25=0, ci75=0))
    } else {
        if (slrconf == 'Market-only') {
            slr <- read.csv("data/slrbyadm0-final.csv")
        } else if (slrconf == 'All Damages') {
            slr <- read.csv("data/slrbyadm0-final-all.csv")
            ## ## Impose that this is strictly greater than market-only
            slr.market <- read.csv("data/slrbyadm0-final.csv")
            slr$q17 <- pmax(slr$q17, slr.market$q17)
            slr$q83 <- pmax(slr$q83, slr.market$q83)
            slr$mu <- pmax(slr$mu, slr.market$mu)
        } else if (slrconf == 'Optimal Adapt.') {
            slr <- read.csv("data/slrbyadm0-final-optimalfixed.csv")
        } else if (slrconf == 'No Adaptation') {
            slr <- read.csv("data/slrbyadm0-final-noAdaptation.csv")
            ## slr.optim <- read.csv("data/slrbyadm0-final-optimalfixed.csv")
            ## slr$q17 <- pmax(slr$q17, slr.optim$q17)
            ## slr$q83 <- pmax(slr$q83, slr.optim$q83)
            ## slr$mu <- pmax(slr$mu, slr.optim$mu)
        }

        if (slrconf == 'Market-only') {
            slr2 <- slr %>% group_by(ISO, year) %>% reframe(mc=1:1000, slrloss=rnorm(1000, mu, ((q83 - q17) / diff(qnorm(c(.17, .83))))))
        } else {
            slr2 <- slr %>% group_by(ISO, year) %>% reframe(mc=1:100, slrloss=rnorm(100, mu, ((q83 - q17) / diff(qnorm(c(.17, .83))))))
        }

        slr3 <- slr2 %>%
            group_by(year, mc) %>% summarize(gloslrloss=sum(slrloss, na.rm=T))

        slr4 <- slr3 %>% group_by(year) %>%
            summarize(mu=mean(gloslrloss), ci25=quantile(gloslrloss, .25), ci75=quantile(gloslrloss, .75))
        pdf <- rbind(pdf, cbind(slrconf=slrconf, slr4))
    }
}

ggplot(pdf, aes(year, mu / 1e9, group=factor(slrconf))) +
    geom_line(aes(colour=factor(slrconf))) +
    geom_ribbon(data=subset(pdf, slrconf == 'Market-only'), aes(ymin=ci25 / 1e9, ymax=ci75 / 1e9), alpha=.5) +
    theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0.01,0.99)) +
    scale_colour_discrete("SLR:") + xlab(NULL) + ylab("SLR Damages (2019 USD)")
ggsave("figures/eachstep-slr.pdf", width=2.5, height=2.5)

trade.names <- list('fd'="Final demand", 'dd'="Domar dist.", 'li'="Leontief Inv. / 10")

pdf <- data.frame()
for (trade.method in c('fd', 'dd', 'li')) {
    tradeloss <- load.tradeloss(trade.method, 0.08)
    tradeloss2 <- tradeloss %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
        group_by(year, mc) %>% filter(!is.na(tradeloss)) %>% summarize(gloloss=sum(tradeloss * POP_EST) / sum(POP_EST))
    tradeloss3 <- tradeloss2 %>% group_by(year) %>%
        summarize(mu=mean(gloloss), ci25=quantile(gloloss, .25), ci75=quantile(gloloss, .75))

    pdf <- rbind(pdf, cbind(trade.method=trade.names[[trade.method]], tradeloss3))
}

pdf$mu[pdf$trade.method == 'Leontief Inv. / 10'] <- pdf$mu[pdf$trade.method == 'Leontief Inv. / 10'] / 10

ggplot(pdf, aes(year, mu, group=trade.method)) +
    geom_line(aes(colour=trade.method)) +
    geom_ribbon(data=subset(pdf, trade.method == 'Final demand'), aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0.01,0.99)) +
    scale_colour_discrete("Method") + xlab(NULL) +
    scale_y_continuous("Spill-over Losses (% GDP)", labels=scales::percent)
ggsave("figures/eachstep-trade.pdf", width=2.5, height=2.5)

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

pdf <- data.frame()
for (solow.conf in c('', '-prodonly')) { # '-additive',
    load(paste0("data/allyr-ww-0.08-fd", solow.conf, ".RData"))

    allyr2 <- allyr.ww %>%
        mutate(solow=ifelse(is.na(product.chg), NA, product.chg - totimpact - -tradeloss - -slrloss)) %>%
        filter(weight.norm > 1e-9 & !is.na(solow)) %>%
        group_by(Year, ISO) %>% reframe(mc=1:30, solow=sample(solow, 30, replace=T, prob=weight.norm)) %>%
        left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
        group_by(Year, mc) %>% dplyr::summarize(glosolow=sum(solow * POP_EST) / sum(POP_EST)) %>%
        group_by(Year) %>% dplyr::summarize(mu=mean(glosolow), ci25=quantile(glosolow, .25), ci75=quantile(glosolow, .75))

    pdf <- rbind(pdf, cbind(solow.conf=list('X'="Preferred", 'X-additive'="Additive", 'X-prodonly'="Production-only")[[paste0('X', solow.conf)]], allyr2))
}

ggplot(pdf, aes(Year, mu, group=solow.conf)) +
    geom_line(aes(colour=solow.conf)) +
    geom_ribbon(data=subset(pdf, solow.conf == 'Preferred'), aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
    scale_colour_discrete("Assumptions") + xlab(NULL) +
    scale_y_continuous("Capital-based losses (% GDP)", labels=scales::percent)
ggsave("figures/eachstep-solow.pdf", width=2.5, height=2.5)
