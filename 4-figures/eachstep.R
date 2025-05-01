## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(ggplot2)
source("~/projects/research-common/R/myPBSmapping.R")

source("src/lib/synth.R")
source("src/lib/loadutils.R")

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

alttable <- data.frame(MetaAnalysis=c(), Persistence=c(), SLR=c(), Trade=c(), Growth=c(), mu=c(), ci25=c(), ci75=c())

metaanaltitle <- list('mcpaperres-PERSIST-mainmed'="Main Median",
                      'mcpaperres-PERSIST-main'="Main MC",
                      'mcpaperres-PERSIST-all'="All MC",
                      'mcr2res-PERSIST-Adjusted R2'="R² Adjusted",
                      'mcr2res-PERSIST-Within R2'="R² Within",
                      'mcr2res-PERSIST-Raw R2'="R² Unfilled",
                      'mcr2res-PERSIST-Total R2'="R² Filled",
                      "mcrfres-PERSIST-controls"="RF Controls",
                      "mcrfres-PERSIST-nonlinear"="RF Nonlinear",
                      "mcrfres-PERSIST-dataset"="RF Dataset",
                      "mcrfres-PERSIST"="RF All")

for (metaanal in c(paste0('mcpaperres-PERSIST-', c("mainmed", "main", "all")),
                   paste0('mcr2res-PERSIST-', c('Adjusted R2', 'Raw R2', 'Total R2', 'Within R2')),
                   'mcrfres-PERSIST', paste0('mcrfres-PERSIST-', c("controls", "nonlinear", "dataset")))) {

    pdf <- data.frame()
    for (persist in c(0, 0.21, 0.36, 0.47, 1)) {
        if (persist == 1) {
            results2 <- read.metaanal(gsub("PERSIST", "0.36", metaanal))
            results2$totimpact <- results2$dimpact
        } else if (persist == 0) {
            results <- read.metaanal(gsub("PERSIST", "0", metaanal))
            results2 <- results %>% group_by(ISO, mc) %>%
                mutate(totimpact=cumsum(dimpact))
        } else {
            results <- read.metaanal(gsub("PERSIST", persist, metaanal))
            results2 <- results %>% group_by(ISO, mc) %>%
                mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - persist)^(0:30), sides=1)[-1:-30])
        }

        results3 <- results2 %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
            filter(!is.na(totimpact)) %>% group_by(Year, mc) %>% dplyr::summarize(gloimpact=sum(totimpact * POP_EST) / sum(POP_EST))
        results4 <- results3 %>% group_by(Year) %>%
            dplyr::summarize(mu=mean(gloimpact), ci25=quantile(gloimpact, .25), ci75=quantile(gloimpact, .75))

        pdf <- rbind(pdf, cbind(persist=persist, results4))

        alttable <- rbind(alttable, cbind(MetaAnalysis=metaanaltitle[[metaanal]],
                                          Persistence=persist, SLR=NA, Trade=NA, Growth=NA,
                                          results3 %>% filter(Year >= 2014) %>% group_by(mc) %>% dplyr::summarize(gloimpact=mean(gloimpact)) %>%
                                          dplyr::summarize(mu=mean(gloimpact), ci25=quantile(gloimpact, .25), ci75=quantile(gloimpact, .75))))
    }

    pdf2 <- pdf %>% group_by(persist) %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)],
                                                 ci25=stats::filter(c(rep(0, 9), ci25), rep(1/10, 10), method='conv')[5:(length(ci25)+4)],
                                                 ci75=stats::filter(c(rep(0, 9), ci75), rep(1/10, 10), method='conv')[5:(length(ci75)+4)])
    ggplot(pdf2, aes(Year, mu, group=factor(persist))) +
        geom_line(aes(colour=factor(persist))) +
        geom_ribbon(data=subset(pdf2, persist == 0.36), aes(ymin=ci25, ymax=ci75), alpha=.5) +
        theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
        scale_colour_discrete(expression(omega~':')) +
        scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
        scale_y_continuous("Direct Impact (% GDP)", labels=scales::percent)
    ggsave(paste0("figures/eachstep-cumul-", metaanal, ".pdf"), width=2.5, height=2.5)
    ggsave(paste0("figures/eachstep-cumul-big-", metaanal, ".pdf"), width=5.5, height=5.5)
}

alttable$Impact <- paste0(round((exp(alttable$mu) - 1) * 100, 1), '%')
alttable$Range <- paste0(round((exp(alttable$ci25) - 1) * 100, 1), ' - ', round((exp(alttable$ci75) - 1) * 100, 1), '%')

library(xtable)
print(xtable(alttable[, c(1:5, 9:10)]), include.rownames=F)

alttable <- data.frame()

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

        slr3 <- df.gdp3 %>% left_join(slr2, by=c('Year'='year', 'Country Code'='ISO')) %>%
            mutate(slrfrac=ifelse(is.na(slrloss), 0, slrloss) / GDP.2019.est) %>%
            left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('Country Code'='ADM0_A3')) %>%
            group_by(Year, mc) %>% dplyr::summarize(gloimpact=-sum(slrfrac * POP_EST, na.rm=T) / sum(POP_EST, na.rm=T))
        slr4 <- slr3 %>% group_by(Year) %>%
            dplyr::summarize(mu=mean(gloimpact), ci25=quantile(gloimpact, .25), ci75=quantile(gloimpact, .75))

        ## slr3 <- slr2 %>%
        ##     group_by(year, mc) %>% dplyr::summarize(gloslrloss=sum(slrloss, na.rm=T))
        ## slr4 <- slr3 %>% group_by(year) %>%
        ##     dplyr::summarize(mu=mean(gloslrloss), ci25=quantile(gloslrloss, .25), ci75=quantile(gloslrloss, .75))
        pdf <- rbind(pdf, cbind(slrconf=slrconf, slr4))

        alttable <- rbind(alttable, cbind(MetaAnalysis='Any', Persistence='Any', SLR=slrconf, Trade='Any', Growth='Any',
                                          slr3 %>% filter(Year >= 2014) %>%
                                          group_by(mc) %>% dplyr::summarize(gloimpact=mean(gloimpact)) %>%
                                          dplyr::summarize(mu=mean(gloimpact), ci25=quantile(gloimpact, .25), ci75=quantile(gloimpact, .75))))
    }
}

ggplot(pdf, aes(Year, mu, group=factor(slrconf))) +
    coord_cartesian(ylim=c(-.003, 0)) +
    geom_line(aes(colour=factor(slrconf))) +
    geom_ribbon(data=subset(pdf, slrconf == 'Market-only'), aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
    scale_colour_discrete("SLR:") + scale_y_continuous("SLR Impact (% GDP)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023))
ggsave("figures/eachstep-slr-big.pdf", width=5.5, height=5.5)

pdf$slrconf2 <- ifelse(pdf$slrconf == "Market-only", "MC",
                ifelse(pdf$slrconf == "Optimal Adapt.", "Opt.",
                ifelse(pdf$slrconf == "No Adaptation", "None", NA)))

ggplot(subset(pdf, !is.na(slrconf2)), aes(Year, mu, group=factor(slrconf2))) +
    coord_cartesian(ylim=c(-.003, 0)) +
    geom_line(aes(colour=factor(slrconf2))) +
    geom_ribbon(data=subset(pdf, slrconf == 'Market-only'), aes(ymin=ci25, ymax=ci75), alpha=.5) +
    theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
    scale_colour_discrete("SLR:") + scale_y_continuous("SLR Impact (% GDP)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023))
ggsave("figures/eachstep-slr.pdf", width=2.5, height=2.5)

trade.names <- list('fd'="Final demand", 'dd'="Domar dist.", 'li'="Leontief Inv.")

trademethodsuffixtitle <- list('X'='RF All', 'X-mcr2all'="R² Filled", 'X-mcpaperall'='All MC')

df.gdp3 <- load.gdp3()
slr2 <- load.slr2(df.gdp3)

for (trade.method.suffix in c('', '-mcr2all', '-mcpaperall')) {
    for (persist in c(0.21, 0.47, 0.36)) {
        ## Load for alttable
        results <- read.metaanal.trade(trade.method.suffix, persist)

        results2 <- results %>% group_by(ISO, mc) %>%
            mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30]) %>%
            left_join(slr2, by=c('ISO', 'Year'='year', 'mc'))
        results2$slrloss[is.na(results2$slrloss)] <- 0

        pdf <- data.frame()
        for (trade.method in c('fd', 'dd', 'li')) {
            tradeloss <- load.tradeloss(paste0(trade.method, trade.method.suffix), persist)
            tradeloss2 <- tradeloss %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
                filter(!is.na(tradeloss)) %>% group_by(year, mc) %>% dplyr::summarize(gloloss=sum(tradeloss * POP_EST) / sum(POP_EST))
            tradeloss3 <- tradeloss2 %>% group_by(year) %>%
                dplyr::summarize(mu=mean(gloloss), ci25=quantile(gloloss, .25), ci75=quantile(gloloss, .75))

            pdf <- rbind(pdf, cbind(trade.method=trade.names[[trade.method]], tradeloss3))

            tradeloss.global <- tradeloss %>% group_by(year) %>% dplyr::summarize(tradeloss=mean(tradeloss, na.rm=T))

            alttable <- rbind(alttable, cbind(MetaAnalysis=trademethodsuffixtitle[[paste0("X", trade.method.suffix)]],
                                              Persistence=persist, SLR='Market-only',
                                              Trade=trade.names[[trade.method]],
                                              Growth='None',
                                              results2 %>% filter(Year >= 2014) %>%
                                              dplyr::left_join(tradeloss, by=c('ISO', 'Year'='year', 'mc')) %>%
                                              dplyr::left_join(tradeloss.global, by=c('Year'='year'), suffix=c('.local', '.global')) %>%
                                              mutate(tradeloss=ifelse(is.na(tradeloss.local), tradeloss.global, tradeloss.local),
                                                     total=totimpact - slrloss - tradeloss) %>%
                                              filter(!is.na(tradeloss) & !is.na(dimpact)) %>%
                                              dplyr::left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
                                              group_by(Year, mc) %>% dplyr::summarize(glototal=sum(total * POP_EST, na.rm=T) / sum(POP_EST * !is.na(total))) %>%
                                              group_by(mc) %>% dplyr::summarize(glototal=mean(glototal, na.rm=T)) %>%
                                              dplyr::summarize(mu=mean(glototal, na.rm=T), ci25=quantile(glototal, .25, na.rm=T), ci75=quantile(glototal, .75, na.rm=T))))

        }

        pdf2 <- pdf %>% group_by(trade.method) %>%
            mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)],
                   ci25=stats::filter(c(rep(0, 9), ci25), rep(1/10, 10), method='conv')[5:(length(ci25)+4)],
                   ci75=stats::filter(c(rep(0, 9), ci75), rep(1/10, 10), method='conv')[5:(length(ci75)+4)])
        ggplot(pdf2, aes(year, -mu, group=trade.method)) +
            coord_cartesian(ylim=c(-.1, 0)) +
            geom_line(aes(colour=trade.method)) +
            geom_ribbon(data=subset(pdf2, trade.method == ifelse(persist == 0.08, 'Final demand', 'Domar dist.')), aes(ymin=-ci25, ymax=-ci75), alpha=.5) +
            theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
            scale_colour_discrete("Method") +
            scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
            scale_y_continuous("Spill-over Impact (% GDP)", labels=scales::percent)
        ggsave(paste0("figures/eachstep-trade-", persist, "-", trade.method.suffix, ".pdf"), width=2.5, height=2.5)
        ggsave(paste0("figures/eachstep-trade-", persist, "-big-", trade.method.suffix, ".pdf"), width=5.5, height=5.5)
    }
}

library(Hmisc)
wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

for (trade.method.suffix in c('', '-mcr2all', '-mcpaperall')) {
    for (persist in c(0.21, 0.36, 0.47)) {
        for (trade.method in c('fd', 'dd', 'li')) {
            pdf <- data.frame(solow.conf='None', Year=1960:2022, mu=0, ci25=0, ci75=0)
            for (solow.conf in c('', '-prodonly')) { # '-noadd', , '-additive'
                if (!file.exists(paste0("data/allyr-ww-", persist, "-", trade.method, trade.method.suffix, solow.conf, ".RData")))
                    next

                load(paste0("data/allyr-ww-", persist, "-", trade.method, trade.method.suffix, solow.conf, ".RData"))
                allyr.ww[allyr.ww$ISO == 'SDN', which(is.na(allyr.ww[allyr.ww$ISO == 'ABW', ][1, ]))] <- NA # country change affects

                allyr2 <- allyr.ww %>%
                    mutate(solow=ifelse(is.na(product.chg), NA, log2lev(product.chg - totimpact - -tradeloss - -slrloss)),
                           total=ifelse(is.na(product.chg), log2lev(totimpact + -tradeloss + -slrloss), log2lev(product.chg)))

                allyr3 <- allyr2 %>%
                    filter(weight.norm > 1e-9 & !is.na(solow)) %>%
                    group_by(Year, ISO) %>% reframe(mc=1:30, source=sample(1:length(weight.norm), 30, replace=T, prob=weight.norm),
                                                    solow=solow[source], total=total[source], weight.norm=weight.norm[source]) %>%
                    left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
                    group_by(Year, mc) %>% dplyr::summarize(glosolow=sum(solow * POP_EST) / sum(POP_EST),
                                                            glototal=sum(total * POP_EST) / sum(POP_EST),
                                                            weight2=sum(weight.norm * POP_EST) / sum(POP_EST))

                allyr4 <- allyr3 %>%
                    group_by(Year) %>% dplyr::summarize(mu=mean(glosolow), ci25=quantile(glosolow, .25), ci75=quantile(glosolow, .75))
                ## group_by(Year) %>% dplyr::summarize(mu=mean(glototal), ci25=quantile(glototal, .25), ci75=quantile(glototal, .75))

                pdf <- rbind(pdf, cbind(solow.conf=list('X'="All capital", 'X-additive'="Additive", 'X-prodonly'="Prod.-only", 'X-noadd'="No Addition")[[paste0('X', solow.conf)]], allyr4))

                allyr3 <- get.weighted.mcts(allyr.ww, 'pop', 'global')

                alttable <- rbind(alttable, cbind(MetaAnalysis=trademethodsuffixtitle[[paste0("X", trade.method.suffix)]],
                                                  Persistence=persist, SLR='Market-only',
                                                  Trade=trade.names[[trade.method]],
                                                  Growth=list('X'="All capital", 'X-additive'="Additive", 'X-prodonly'="Prod.-only", 'X-noadd'="No Addition")[[paste0('X', solow.conf)]],
                                                  allyr3 %>% filter(Year >= 2014) %>%
                                                  group_by(mc) %>% dplyr::summarize(glototal=mean(total, na.rm=T), weight2=mean(weight2)) %>%
                                                  dplyr::summarize(mu=wtd.median(glototal, weights=weight2, normwt=T), ci25=wtd.quantile(glototal, .25, weights=weight2, normwt=T), ci75=wtd.quantile(glototal, .75, weights=weight2, normwt=T))))

            }

            pdf$solow.conf <- factor(pdf$solow.conf, levels=c("None", "Prod.-only", "All capital", "Additive", "No Addition"))
            pdf2 <- pdf %>% group_by(solow.conf) %>%
                mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)],
                       ci25=stats::filter(c(rep(0, 9), ci25), rep(1/10, 10), method='conv')[5:(length(ci25)+4)],
                       ci75=stats::filter(c(rep(0, 9), ci75), rep(1/10, 10), method='conv')[5:(length(ci75)+4)])

            ggplot(pdf2, aes(Year, mu, group=solow.conf)) +
                geom_line(aes(colour=solow.conf)) +
                geom_ribbon(data=subset(pdf2, solow.conf == 'All capital'), aes(ymin=ci25, ymax=ci75), alpha=.5) +
                theme_bw() + theme(legend.justification=c(0,0), legend.position=c(0.01,0.01)) +
                scale_colour_discrete("Capital:") +
                scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
                scale_y_continuous("Capital-based impact (% GDP)", labels=scales::percent)
            ggsave(paste0("figures/eachstep-solow-", persist, "-", trade.method, trade.method.suffix, ".pdf"), width=2.5, height=2.5)
            ggsave(paste0("figures/eachstep-solow-", persist, "-", trade.method, trade.method.suffix, "-big.pdf"), width=5.5, height=5.5)
        }
    }
}

alttable$Impact <- paste0(round((exp(alttable$mu) - 1) * 100, 1), '%')
alttable$Range <- paste0(round((exp(alttable$ci25) - 1) * 100, 1), ' - ', round((exp(alttable$ci75) - 1) * 100, 1), '%')

library(xtable)
print(xtable(alttable[, c(1:5, 9:10)]), include.rownames=F)
