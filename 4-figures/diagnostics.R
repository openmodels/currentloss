## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

persist <- "0.31"
trade.method <- "dd"
source("src/lib/utils2.R")

solowdone <- load.solowsum(persist, trade.method)

load.solowdata()
load.solowdata.mc(1)

const.draws <- data.frame()
years.draws <- data.frame()
for (mcii in 1:30) {
    for (iso in unique(solowdone$ISO)) {
        if (!any(solowdone$ISO == iso & solowdone$mc == mcii))
            next
        print(c(mcii, iso))

        load(paste0("data/solow-", persist, "-", trade.method, "/v4-", iso, "-", mcii, ".RData"))

        rows <- sample(1000, 10)
        const.draw <- rbind(data.frame(param='tfp', value=la$tfp[rows]),
                            data.frame(param='dtfpdt', value=la$dtfpdt[rows]),
                            data.frame(param='rencap0part', value=la$rencap0part[rows]),
                            data.frame(param='renwarmeffect', value=la$renwarmeffect[rows]),
                            data.frame(param='rickerr', value=la$rickerr[rows]),
                            data.frame(param='rickerb', value=la$rickerb[rows]),
                            data.frame(param='rencap_error', value=la$rencap_error[rows]),
                            data.frame(param='rencapshare_error', value=la$rencapshare_error[rows]),
                            data.frame(param='procap0part', value=la$procap0part[rows]),
                            data.frame(param='saverate0', value=la$saverate0[rows]),
                            data.frame(param='dsaveratedt', value=la$dsaveratedt[rows]),
                            data.frame(param='deprrate', value=la$deprrate[rows]),
                            data.frame(param='procap_error', value=la$procap_error[rows]),
                            data.frame(param='sav_error', value=la$sav_error[rows]),
                            data.frame(param='humcap0part', value=la$humcap0part[rows]),
                            data.frame(param='dloghumcapdt', value=la$dloghumcapdt[rows]),
                            data.frame(param='humcap_error', value=la$humcap_error[rows]),
                            data.frame(param='shares0.ren', value=la$shares0[rows, 1]),
                            data.frame(param='shares0.pro', value=la$shares0[rows, 2]),
                            data.frame(param='shares0.hum', value=la$shares0[rows, 3]),
                            data.frame(param='shares0.pop', value=la$shares0[rows, 4]),
                            data.frame(param='sharesT.ren', value=la$sharesT[rows, 1]),
                            data.frame(param='sharesT.pro', value=la$sharesT[rows, 2]),
                            data.frame(param='sharesT.hum', value=la$sharesT[rows, 3]),
                            data.frame(param='sharesT.pop', value=la$sharesT[rows, 4]),
                            data.frame(param='shares_error', value=la$shares_error[rows]),
                            data.frame(param='gdp_error', value=la$gdp_error[rows]))
        years.draw <- rbind(data.frame(param='cumulpart', year=1961:2023, value=la$cumulpart[rows[1],]),
                            data.frame(param='product', year=1961:2023, value=la$product[rows[1],]),
                            data.frame(param='rencap_model', year=1961:2024, value=la$rencap_model[rows[1],]),
                            data.frame(param='procap_model', year=1961:2024, value=la$procap_model[rows[1],]),
                            data.frame(param='humcap_univ', year=1961:2024, value=la$humcap_univ[rows[1],]),
                            data.frame(param='product_nocc', year=1961:2023, value=la$product_nocc[rows[1],]),
                            data.frame(param='rencap_nocc', year=1961:2024, value=la$rencap_nocc[rows[1],]),
                            data.frame(param='procap_nocc', year=1961:2024, value=la$procap_nocc[rows[1],]))

        const.draws <- rbind(const.draws, cbind(mc=mcii, ISO=iso, const.draw))
        years.draws <- rbind(years.draws, cbind(mc=mcii, ISO=iso, years.draw))
    }
}

const.draws2 <- const.draws %>% filter(param %in% c('tfp', 'dtfpdt', 'renwarmeffect', 'rickerr', 'rickerb',
                                         'saverate0', 'dsaveratedt', 'deprrate',
                                         'shares0.ren', 'shares0.pro', 'shares0.hum', 'shares0.pop',
                                         'sharesT.ren', 'sharesT.pro', 'sharesT.hum', 'sharesT.pop')) %>%
    group_by(ISO, param) %>% reframe(value=value[value > quantile(value, .25) & value < quantile(value, .75)])

const.draws2$param <- factor(const.draws2$param, levels=c('tfp', 'dtfpdt', 'renwarmeffect', 'rickerr', 'rickerb',
                                                          'saverate0', 'dsaveratedt', 'deprrate',
                                                          'shares0.ren', 'shares0.pro', 'shares0.hum', 'shares0.pop',
                                                          'sharesT.ren', 'sharesT.pro', 'sharesT.hum', 'sharesT.pop'))
library(ggplot2)

ggplot(const.draws2,
       aes(value)) +
    facet_wrap(~ param, scales='free') +
    geom_histogram() + theme_bw()

ggplot(subset(years.draws, param == 'cumulpart'), aes(value)) +
    facet_wrap(~ year) +
    geom_histogram()

preds <- years.draws %>% group_by(ISO, year, param) %>%
    reframe(value=value[value > quantile(value, .25) & value < quantile(value, .75)]) %>%
    group_by(ISO, year, param) %>% summarize(q05=quantile(value, .05), mu=mean(value), q95=quantile(value, .95))
preds2.mu <- dcast(preds, ISO + year ~ param, value.var='mu')
preds2.q05 <- dcast(preds, ISO + year ~ param, value.var='q05')
preds2.q95 <- dcast(preds, ISO + year ~ param, value.var='q95')

df3 <- df2 %>% left_join(preds2.mu, by=c('ISO', 'Year'='year')) %>%
    left_join(preds2.q05, by=c('ISO', 'Year'='year'), suffix=c('', '.q05')) %>%
    left_join(preds2.q95, by=c('ISO', 'Year'='year'), suffix=c('.mu', '.q95'))

ggplot(df3, aes(denom * `GDP.2005`, denom * product.mu)) +
    geom_point() + geom_linerange(aes(ymin=denom * product.q05, ymax=denom * product.q95)) +
    geom_abline(yintercept=0, slope=1) + scale_x_log10() + scale_y_log10() +
    theme_bw() + xlab("Reported Gross domestic product (2005 USD)") +
    ylab("Modeled Gross domestic product (2005 USD)")

ggplot(df3, aes(denom * `Produced Capital`, denom * procap_model.mu)) +
    geom_point() + geom_linerange(aes(ymin=denom * procap_model.q05, ymax=denom * procap_model.q95)) +
    geom_abline(yintercept=0, slope=1) + scale_x_log10() + scale_y_log10() +
    theme_bw() + xlab("Reported Produced Capital (2005 USD)") +
    ylab("Modeled Produced Capital (2005 USD)")

ggplot(df3, aes(denom * `Renewable Capital`, denom * rencap_model.mu)) +
    geom_point() + geom_linerange(aes(ymin=denom * rencap_model.q05, ymax=denom * rencap_model.q95)) +
    geom_abline(yintercept=0, slope=1) + scale_x_log10() + scale_y_log10(limits=c(1e8, 1e13)) +
    theme_bw() + xlab("Reported Renewable Capital (2005 USD)") +
    ylab("Modeled Renewable Capital (2005 USD)")

ggplot(df3, aes(denom * `Per Person Human Capital`, denom * humcap_univ.mu)) +
    geom_point() + geom_linerange(aes(ymin=denom * humcap_univ.q05, ymax=denom * humcap_univ.q95)) +
    geom_abline(yintercept=0, slope=1) + scale_x_log10() + scale_y_log10() +
    theme_bw() + xlab("Reported Per Person Human Capital (2005 USD)") +
    ylab("Modeled Per Person Human Capital (2005 USD)")

