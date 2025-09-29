## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(tidyverse)
library(lfe)

df <- read.csv("papers/models/Kotz et al. 2024/coeffs.csv")
df2.est <- df %>% filter(Datum == "Est") %>% pivot_longer(cols=starts_with("Lag"), names_to="Lag",
                                                          names_prefix="Lag.", values_to="Est")
df2.se <- df %>% filter(Datum == "SE") %>% pivot_longer(cols=starts_with("Lag"), names_to="Lag",
                                                          names_prefix="Lag.", values_to="SE")
df2 <- df2.est %>% left_join(df2.se, by=c('Variable', 'Formula', 'Lag')) %>% select(!c(Datum.x, Datum.y))
df2$Lag <- as.numeric(df2$Lag)

decays <- c()
for (mc in 1:1000) {
    df2$value <- rnorm(nrow(df2), df2$Est, df2$SE)
    df2$logval <- ifelse(df2$value < 0, log(abs(df2$value)), NA)

    mod <- felm(logval ~ Lag | Formula, data=df2)
    decays <- c(decays, coef(mod))
}

ggplot(data.frame(decays), aes(decays)) +
    geom_histogram() + theme_bw() +
    xlab("Estimated decay rate (log per lag)")
## saved to kotz2014-decay.pdf

mean(decays[decays < 0]) # -0.05005528
## Decays to exp(N log(1 - beta)) after N lags
## So log(1 - beta) = -0.05005528 => beta = 0.04877058


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
