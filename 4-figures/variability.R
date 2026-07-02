## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(PBSmapping)
library(ggplot2)
library(sf)

persist <- "0.6"
trade.method <- "dd-mcr2all"
source("src/lib/utils2.R")
source("src/lib/synth.R")

load(paste0("data/allyr-ww-", persist, "-", trade.method, ".RData"))
allyr.ww[allyr.ww$ISO == 'SDN', which(is.na(allyr.ww[allyr.ww$ISO == 'ABW', ][1, ]))] <- NA # country change affects

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

devs <- allyr.ww %>% filter(Year > 2013 & weight.norm > 1e-9) %>% group_by(ISO, mc) %>%
    mutate(total=ifelse(is.na(product.chg), totimpact - tradeloss - slrloss, product.chg)) %>%
    dplyr::summarize(stddev=sd(total, na.rm=T), weight=sum(weight.norm)) %>%
    group_by(ISO) %>% dplyr::summarize(stddev=wtd.median(stddev, weights=weight))

median(devs$stddev)
