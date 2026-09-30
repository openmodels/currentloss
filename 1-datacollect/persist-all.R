## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(tidyverse)
library(lfe)
source("src/lib/persist.R")

## Bosello study:
df <- read.csv("papers/models/coacch-d4.3.csv")

## df2 = subset(df, Model == 'MIMOSA')
find.persist <- function(df2) {
    direct <- approx(c(2020, df2$Year[df2$Datum == 'Direct']), c(0, df2$Damages[df2$Datum == 'Direct']), 2020:2100)$y
    soln <- optimize(function(persist) {
        total <- stats::filter(c(rep(0, 30), direct), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30]
        error <- total[2020:2100 %in% c(2030, 2050, 2100)] - df2$Damages[df2$Datum == 'Total']
        sum(error^2)
    }, c(0, 1))

    soln$minimum
}

find.persist(subset(df, Model == 'MIMOSA'))
find.persist(subset(df, Model == 'WITCH'))
find.persist(subset(df, Model == 'REMIND'))

## Construct averages

omegas1 <- c(0.47, 0.79, 0.70, 0.77, 0.87, 0.11, 0.47)
mean(omegas1) # 0.5971429

omegas2 <- c(0.08, 0.21, 0.47, 0.79, 0.11, 0.47)
mean(omegas2) # 0.355

omegas3 <- c(0.70, 0.77, 0.87)
mean(omegas3) # 0.78

## Try out multiple options with lags

time <- 1:12
shck <- .01 * c(0, 0, 1, rep(0, 10))
shck.L1 <- c(0, shck[1:(length(shck) - 1)])
shck.L2 <- c(rep(0, 2), shck[1:(length(shck) - 2)])

df <- data.frame()
for (lags in 0:2) {
    if (lags == 0)
        dlogy <- shck
    else if (lags == 1)
        dlogy <- shck + shck.L1
    else if (lags == 2)
        dlogy <- shck + shck.L1 + shck.L2

    for (do.waidelich in c(F, T)) {
        for (drop.lags in c(F, T)) {
            logy <- persist.general(dlogy, 0.6, ifelse(drop.lags, lags, 0), do.waidelich)
            logz <- persist.general(cumsum(dlogy), 0.6, ifelse(drop.lags, lags, 0), do.waidelich)
            df <- rbind(df, data.frame(do.waidelich, drop.lags, lags, time=time-1, logy=logy[-1], logz=logz[-1]))
        }
    }
}

df$waidelich <- ifelse(df$do.waidelich, "Waidelich-style", "Decay-style")
df$laghandle <- ifelse(df$drop.lags, "Remove Lags", "Ignore Lags")
ggplot(df, aes(time, exp(-logy), group=lags, colour=factor(lags))) +
    facet_grid(waidelich ~ laghandle) +
    geom_line(aes(linetype="Single Year")) + geom_line(aes(y=exp(-logz), linetype="Step Change")) +
    scale_x_continuous("Years (shock at 1)", breaks=seq(0, 10, by=2), expand=c(0, 0)) +
    coord_cartesian(xlim=c(0, 10)) +
    scale_y_continuous("Change relative to baseline", labels=scales::percent) +
    scale_colour_discrete(name="Number of lags:") + scale_linetype(name="Shock type:") +
    theme_bw() + theme(panel.spacing.x=unit(0.4, "cm"))
ggsave("figures/diagnostic-persist.pdf", width=6.5, height=5)
