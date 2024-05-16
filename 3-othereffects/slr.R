## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

load("data/totalcosts.RData")

library(dplyr)

do.market.only <- T

if (do.market.only) {
    df2 = df %>% filter(year <= 2023 & costtype %in% c('inundation', 'protection', 'stormCapital')) %>%
        group_by(adm0, quantile, ssp, year, case) %>%
        summarize(costs=sum(costs)) %>% # sum to case
        group_by(adm0, year, case) %>%
        summarize(q17=quantile(costs, .17), mu=mean(costs), q83=quantile(costs, .83))
} else {
    df2 = df %>% filter(year <= 2023) %>%
        group_by(adm0, quantile, ssp, year, case) %>%
        summarize(costs=sum(costs)) %>% # sum to case
        group_by(adm0, year, case) %>%
        summarize(q17=quantile(costs, .17), mu=mean(costs), q83=quantile(costs, .83))
}

library(ggplot2)

df3 <- df2 %>% group_by(year, case) %>% summarize(q17=sum(q17), mu=sum(mu), q83=sum(q83))
df4 <- df3 %>% filter(year >= 2010) %>% group_by(year) %>% summarize(q17=min(mu), q83=max(mu), mu=mean(mu))

ggplot(df3, aes(year, mu, group=case)) +
    geom_line(aes(colour=case)) + geom_ribbon(aes(ymin=q17, ymax=q83), alpha=.5) +
    scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_log10() + theme_bw() +
    scale_colour_discrete("Adaptation\nScenario:", breaks=c('optimalfixed', 'noAdaptation'), labels=c('Optimal', 'No Adapt.')) +
    ylab("Global Sea Level Damages (USD)")

ggplot(rbind(df3, cbind(df4, case='mean')), aes(year, mu, group=case)) +
    geom_line(aes(colour=case)) + geom_ribbon(aes(ymin=q17, ymax=q83), alpha=.5) +
    scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_log10() + theme_bw() +
    scale_colour_discrete("Adaptation\nScenario:", breaks=c('min', 'max', 'mean'), labels=c('Min Cost', 'Max Cost', 'Average')) +
    ylab("Global Sea Level Damages (USD)")

df3.iso <- df2 %>% filter(year >= 2010) %>% group_by(adm0, year) %>% summarize(q17=min(mu), q83=max(mu), mu=mean(mu))

pred <- data.frame()
for (ISO in unique(df3.iso$adm0)) {
    print(ISO)
    mdf <- subset(df3.iso, adm0 == ISO & year >= 2010)
    if (all(mdf$mu == 0)) {
        pred <- rbind(pred, data.frame(ISO, year=1960:2023, q17=0, mu=0, q83=0))
        next
    }
    y1960 <- mdf$year - 1960
    mu <- exp(predict(lm(log(mu) ~ y1960, data=mdf), data.frame(y1960=0:(2023 - 1960))))
    if (all(mdf$q17 == 0))
        q17 <- 0
    else
        q17 <- exp(predict(lm(log(q17) ~ y1960, data=mdf), data.frame(y1960=0:(2023 - 1960))))
    q83 <- exp(predict(lm(log(q83) ~ y1960, data=mdf), data.frame(y1960=0:(2023 - 1960))))
    pred <- rbind(pred, data.frame(ISO, year=1960:2023, q17, mu, q83))
}

pred3 <- pred %>% group_by(year) %>% summarize(q17=sum(q17), mu=sum(mu), q83=sum(q83))

pdf <- rbind(cbind(source='CIAM', df4), cbind(source='Emulated', pred3))

ggplot(pdf, aes(year, mu, group=source)) +
    geom_line(aes(colour=source)) + geom_ribbon(aes(ymin=q17, ymax=q83), alpha=.5) +
    scale_x_continuous(expand=c(0, 0)) + scale_y_log10() + theme_bw() +
    scale_colour_discrete("Damages Source:") +
    ylab("Global Sea Level Damages (USD)")

ggplot(pdf, aes(year, mu - mu[year == 1960], group=source)) +
    geom_line(aes(colour=source)) + geom_ribbon(aes(ymin=q17 - q17[year == 1960], ymax=q83 - q83[year == 1960]), alpha=.5) +
    scale_x_continuous(expand=c(0, 0)) + theme_bw() +
    scale_colour_discrete("Damages Source:") +
    ylab("Global Sea Level Damages (USD)")

## tosave <- rbind(cbind(source='Emulated', pred[pred$year < 2010,]), cbind(source='CIAM', ISO=df3.iso$adm0, df3.iso[, -1]))
## write.csv(tosave, "data/slrbyadm0-final.csv", row.names=F)

pred2 <- pred %>% group_by(ISO) %>% mutate(q17=q17 - q17[year == 1960], mu=mu - mu[year == 1960], q83=q83 - q83[year == 1960])
if (do.market.only) {
    write.csv(pred2, "data/slrbyadm0-final.csv", row.names=F)
} else {
    write.csv(pred2, "data/slrbyadm0-final-all.csv", row.names=F)
    stop()
}

## Compare to GDP
source("src/lib/loadutils.R")

df.gdp3 <- load.gdp3()

pred3 <- pred2 %>% left_join(df.gdp3, by=c('ISO'='Country Code', 'year'='Year'))

pred3$q17.frac <- pred3$q17 / pred3$GDP.2019.est
pred3$mu.frac <- pred3$mu / pred3$GDP.2019.est
pred3$q83.frac <- pred3$q83 / pred3$GDP.2019.est

labels <- subset(pred3, year == 2023 & mu.frac > 0.002)
labels <- labels[order(labels$GDP.2019.est),]

gp <- ggplot(pred3, aes(year, mu.frac, group=ISO)) +
    geom_ribbon(aes(ymin=q17.frac, ymax=q83.frac), alpha=.1) +
    geom_line(linewidth=.1) +
    geom_label(data=labels, aes(x=2025, y=round(mu.frac * 5e2) / 5e2, label=ISO), size=2) +
    theme_bw() + xlab(NULL) + scale_y_continuous("GDP lost to coastal impacts (%)", expand=c(0, 0), labels=scales::percent)
ggsave("figures/si/slr-byiso.pdf", gp, width=6.5, height=4.5)
