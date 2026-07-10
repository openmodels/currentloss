## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(tidyverse)
library(lfe)

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
