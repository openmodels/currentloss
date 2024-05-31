## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(PBSmapping)
library(ggplot2)

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

alttable <- data.frame(Persistence=c(), SLR=c(), Trade=c(), Growth=c(), mu=c(), ci25=c(), ci75=c())

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

    alttable <- rbind(alttable, cbind(Persistence=persist, SLR=NA, Trade=NA, Growth=NA,
                                      results3 %>% filter(Year >= 2014) %>% group_by(mc) %>% summarize(gloimpact=mean(gloimpact)) %>%
                                      summarize(mu=mean(gloimpact), ci25=quantile(gloimpact, .25), ci75=quantile(gloimpact, .75))))
}

ggplot(pdf, aes(Year, mu, group=factor(persist))) +
    geom_line(aes(colour=factor(persist))) +
    geom_ribbon(data=subset(pdf, persist == 0.21), aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
    scale_colour_discrete(expression(omega~':')) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_y_continuous("Direct Impact (% GDP)", labels=scales::percent)
ggsave("figures/eachstep-cumul.pdf", width=2.5, height=2.5)

source("src/lib/loadutils.R")

df.gdp3 <- load.gdp3()

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

        alttable <- rbind(alttable, cbind(Persistence=NA, SLR=slrconf, Trade=NA, Growth=NA,
                                          df.gdp3 %>% filter(Year >= 2014) %>% left_join(slr2, by=c('Year'='year', 'Country Code'='ISO')) %>%
                                          mutate(slrfrac=ifelse(is.na(slrloss), 0, slrloss) / GDP.2019.est) %>%
                                          left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('Country Code'='ADM0_A3')) %>%
                                          group_by(Year, mc) %>% summarize(gloimpact=sum(slrfrac * POP_EST, na.rm=T) / sum(POP_EST, na.rm=T)) %>%
                                          group_by(mc) %>% summarize(gloimpact=-mean(gloimpact)) %>%
                                          summarize(mu=mean(gloimpact), ci25=quantile(gloimpact, .25), ci75=quantile(gloimpact, .75))))
    }
}

ggplot(pdf, aes(year, mu / 1e9, group=factor(slrconf))) +
    geom_line(aes(colour=factor(slrconf))) +
    geom_ribbon(data=subset(pdf, slrconf == 'Market-only'), aes(ymin=ci25 / 1e9, ymax=ci75 / 1e9), alpha=.5) +
    theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0.01,0.99)) +
    scale_colour_discrete("SLR:") + ylab("SLR Damages (2019 USD)") +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023))
ggsave("figures/eachstep-slr.pdf", width=2.5, height=2.5)

trade.names <- list('fd'="Final demand", 'dd'="Domar dist.", 'li'="Leontief Inv.")

df.gdp3 <- load.gdp3()
slr2 <- load.slr2(df.gdp3)

for (persist in c(0.08, 0.21)) {
    ## Load for alttable
    load(paste0("data/mcrfres-", persist, ".RData"))

    results2 <- results %>% group_by(ISO, mc) %>%
        mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30]) %>%
        left_join(slr2, by=c('ISO', 'Year'='year', 'mc'))
    results2$slrloss[is.na(results2$slrloss)] <- 0

    pdf <- data.frame()
    for (trade.method in c('fd', 'dd', 'li')) {
        tradeloss <- load.tradeloss(trade.method, persist)
        tradeloss2 <- tradeloss %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
            group_by(year, mc) %>% filter(!is.na(tradeloss)) %>% summarize(gloloss=sum(tradeloss * POP_EST) / sum(POP_EST))
        tradeloss3 <- tradeloss2 %>% group_by(year) %>%
            summarize(mu=mean(gloloss), ci25=quantile(gloloss, .25), ci75=quantile(gloloss, .75))

        pdf <- rbind(pdf, cbind(trade.method=trade.names[[trade.method]], tradeloss3))

        tradeloss.global <- tradeloss %>% group_by(year) %>% dplyr::summarize(tradeloss=mean(tradeloss, na.rm=T))

        alttable <- rbind(alttable, cbind(Persistence=persist, SLR='Market-only',
                                          Trade=trade.names[[trade.method]],
                                          Growth='None',
                                          results2 %>% filter(Year >= 2014) %>%
                                          left_join(tradeloss, by=c('ISO', 'Year'='year', 'mc')) %>%
                                          left_join(tradeloss.global, by=c('Year'='year'), suffix=c('.local', '.global')) %>%
                                          mutate(tradeloss=ifelse(is.na(tradeloss.local), tradeloss.global, tradeloss.local),
                                                 total=totimpact - slrloss - tradeloss) %>%
                                          left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
                                          group_by(Year, mc) %>% summarize(glototal=sum(total * POP_EST) / sum(POP_EST)) %>%
                                          group_by(mc) %>% summarize(glototal=mean(glototal)) %>%
                                          summarize(mu=mean(glototal), ci25=quantile(glototal, .25), ci75=quantile(glototal, .75))))

    }

    ggplot(pdf, aes(year, mu, group=trade.method)) +
        geom_line(aes(colour=trade.method)) +
        geom_ribbon(data=subset(pdf, trade.method == ifelse(persist == 0.08, 'Final demand', 'Domar dist.')), aes(ymin=ci25, ymax=ci75), alpha=.5) +
        theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0.01,0.99)) +
        scale_colour_discrete("Method") +
        scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
        scale_y_continuous("Spill-over Losses (% GDP)", labels=scales::percent)
    ggsave(paste0("figures/eachstep-trade-", persist, ".pdf"), width=2.5, height=2.5)
}

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

library(Hmisc)

for (persist in c(0.08, 0.21)) {
    trade.method <- list('0.08'='fd', '0.21'='dd')[[as.character(persist)]]

    pdf <- data.frame() #solow.conf='None', Year=1961:2023, mu=0, ci25=0, ci75=0)
    for (solow.conf in c('', '-prodonly', '-additive')) {
        if (!file.exists(paste0("data/allyr-ww-", persist, "-", trade.method, solow.conf, ".RData")))
            next
        load(paste0("data/allyr-ww-", persist, "-", trade.method, solow.conf, ".RData"))

        allyr2 <- allyr.ww %>%
            mutate(solow=ifelse(is.na(product.chg), NA, product.chg - totimpact - -tradeloss - -slrloss),
                   total=ifelse(is.na(product.chg), totimpact + -tradeloss + -slrloss, product.chg))

        allyr3 <- allyr2 %>%
            filter(weight.norm > 1e-9 & !is.na(solow)) %>%
            group_by(Year, ISO) %>% reframe(mc=1:30, solow=sample(solow, 30, replace=T, prob=weight.norm)) %>%
            left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
            group_by(Year, mc) %>% dplyr::summarize(glosolow=sum(solow * POP_EST) / sum(POP_EST))

        allyr4 <- allyr3 %>%
            group_by(Year) %>% dplyr::summarize(mu=mean(glosolow), ci25=quantile(glosolow, .25), ci75=quantile(glosolow, .75))

        pdf <- rbind(pdf, cbind(solow.conf=list('X'="All capital", 'X-additive'="Additive", 'X-prodonly'="Produced-only")[[paste0('X', solow.conf)]], allyr4))

        alttable <- rbind(alttable, cbind(Persistence=persist, SLR='Market-only',
                                          Trade=ifelse(trade.method == 'li', 'Leontief Inv.', trade.names[[trade.method]]),
                                          Growth=list('X'="All capital", 'X-additive'="Additive", 'X-prodonly'="Produced-only")[[paste0('X', solow.conf)]],
                                          allyr2 %>% filter(Year >= 2014, weight.norm > 1e-9) %>%
                                          left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
                                          group_by(Year, mc) %>% dplyr::summarize(glototal=sum(total * POP_EST) / sum(POP_EST),
                                                                                  weight2 = wtd.mean(weight.norm, weights = POP_EST)) %>%
                                          group_by(mc) %>% dplyr::summarize(glototal=mean(glototal), weight2=mean(weight2)) %>%
                                          dplyr::summarize(mu=wtd.mean(glototal, weights=weight2), ci25=wtd.quantile(glototal, .25, weights=weight2), ci75=wtd.quantile(glototal, .75, weights=weight2))))

    }

    pdf$solow.conf <- factor(pdf$solow.conf, levels=c("None", "Produced-only", "All capital", "Additive"))

    ggplot(pdf, aes(Year, mu, group=solow.conf)) +
        geom_line(aes(colour=solow.conf)) +
        geom_ribbon(data=subset(pdf, solow.conf == 'All capital'), aes(ymin=ci25, ymax=ci75), alpha=.5) +
        theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
        scale_colour_discrete("Capital:") +
        scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
        scale_y_continuous("Capital-based losses (% GDP)", labels=scales::percent)
    ggsave(paste0("figures/eachstep-solow-", persist, "-", trade.method, ".pdf"), width=2.5, height=2.5)
}

alttable$Impact <- paste0(round((exp(alttable$mu) - 1) * 100, 1), '%')
alttable$Range <- paste0(round((exp(alttable$ci25) - 1) * 100, 1), ' - ', round((exp(alttable$ci75) - 1) * 100, 1), '%')

library(xtable)
print(xtable(alttable[, c(1:4, 8:9)]), include.rownames=F)
