setwd("~/Dropbox/UK Economic Risks/climate/gsat")

df <- read.csv("ssps_26_70_ukproject_allmembers.csv")
library(dplyr)
## Get baseline
df0 <- df %>% group_by(run_id) %>% summarize(baseline=mean(value[year >= 1995 & year <= 2014]))

NN <- length(unique(df$run_id))
dfc <- data.frame(run_id=unique(df$run_id), t2coeff=1.25 * (rnorm(NN, 0.595, 0.190) + rnorm(NN, 0.113, 0.125)) + rnorm(NN, 0.260, 0.267))

df2 <- df %>% left_join(df0) %>% left_join(dfc)
df2$t <- df2$value - df2$baseline + 0.85
df3 <- df2 %>% group_by(year) %>% summarize(tmu=mean(t), tci05=quantile(t, .05), tci95=quantile(t, .95), dmu=mean(t2coeff * t^2), dci05=quantile(t2coeff * t^2, .05), dci95=quantile(t2coeff * t^2, .95), dtmu=mean(t2coeff * tmu^2), dtci05=mean(t2coeff * tci05^2), dtci95=mean(t2coeff * tci95^2))

subset(df3, year == 2023)
subset(df3, year == 1960)

