load("/mnt/423808E91650DDB1/shared/currentloss/slr/totalcosts.RData")

library(dplyr)

df$ssp = substr(df$scenario, 1, 4)
df$adm0 = substr(df$adm1, 1, 3)

df2 = df %>% filter(year <= 2023) %>%
  group_by(adm0, iam, quantile, scenario, ssp, year, case_type) %>%
  summarize(costs=sum(costs)) %>% # sum to ADM0
  group_by(adm0, year, case_type) %>% summarize(q05=quantile(costs, .05), mu=mean(costs), q95=quantile(costs, .95))

write.csv(df2, "data/slrbyadm0.csv", row.names=F)

## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(ggplot2)

df2 <- read.csv("data/slrbyadm0.csv")
df3 <- df2 %>% group_by(year, case_type) %>% summarize(q05=sum(q05), mu=sum(mu), q95=sum(q95))
df4 <- df3 %>% filter(year >= 2010) %>% group_by(year) %>% summarize(q05=min(mu), q95=max(mu), mu=mean(mu))

ggplot(df3, aes(year, mu, group=case_type)) +
    geom_line(aes(colour=case_type)) + geom_ribbon(aes(ymin=q05, ymax=q95), alpha=.5) +
    scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_log10() + theme_bw() +
    scale_colour_discrete("Adaptation\nScenario:", breaks=c('min', 'max'), labels=c('Min Cost', 'Max Cost')) +
    ylab("Global Sea Level Damages (USD)")

ggplot(rbind(df3, cbind(df4, case_type='mean')), aes(year, mu, group=case_type)) +
    geom_line(aes(colour=case_type)) + geom_ribbon(aes(ymin=q05, ymax=q95), alpha=.5) +
    scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_log10() + theme_bw() +
    scale_colour_discrete("Adaptation\nScenario:", breaks=c('min', 'max', 'mean'), labels=c('Min Cost', 'Max Cost', 'Average')) +
    ylab("Global Sea Level Damages (USD)")

df3.iso <- df2 %>% filter(year >= 2010) %>% group_by(adm0, year) %>% summarize(q05=min(mu), q95=max(mu), mu=mean(mu))

pred <- data.frame()
for (ISO in unique(df3.iso$adm0)) {
    print(ISO)
    mdf <- subset(df3.iso, adm0 == ISO & year >= 2010)
    y1960 <- mdf$year - 1960
    mu <- exp(predict(lm(log(mu) ~ y1960, data=mdf), data.frame(y1960=0:(2010 - 1960))))
    if (all(mdf$q05 == 0))
        q05 <- 0
    else
        q05 <- exp(predict(lm(log(q05) ~ y1960, data=mdf), data.frame(y1960=0:(2010 - 1960))))
    q95 <- exp(predict(lm(log(q95) ~ y1960, data=mdf), data.frame(y1960=0:(2010 - 1960))))
    pred <- rbind(pred, data.frame(ISO, year=1960:2010, q05, mu, q95))
}

pred3 <- pred %>% group_by(year) %>% summarize(q05=sum(q05), mu=sum(mu), q95=sum(q95))

pdf <- rbind(cbind(source='CIAM', df4), cbind(source='Emulated', pred3))

ggplot(pdf, aes(year, mu, group=source)) +
    geom_line(aes(colour=source)) + geom_ribbon(aes(ymin=q05, ymax=q95), alpha=.5) +
    scale_x_continuous(expand=c(0, 0)) + scale_y_log10() + theme_bw() +
    scale_colour_discrete("Damages Source:") +
    ylab("Global Sea Level Damages (USD)")

ggplot(pdf, aes(year, mu - mu[year == 1960], group=source)) +
    geom_line(aes(colour=source)) + geom_ribbon(aes(ymin=q05 - q05[year == 1960], ymax=q95 - q95[year == 1960]), alpha=.5) +
    scale_x_continuous(expand=c(0, 0)) + theme_bw() +
    scale_colour_discrete("Damages Source:") +
    ylab("Global Sea Level Damages (USD)")

tosave <- rbind(cbind(source='Emulated', pred[pred$year < 2010,]), cbind(source='CIAM', ISO=df3.iso$adm0, df3.iso[, -1]))
write.csv(tosave, "data/slrbyadm0-final.csv", row.names=F)
