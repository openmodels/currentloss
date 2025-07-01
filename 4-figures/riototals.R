## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(PBSmapping)
library(ggplot2)
library(sf)

do.for.subset <- "L+MIC" # "global" or "L+MIC"

persist <- "0.36"
trade.method <- "dd-mcr2all"
source("src/lib/utils2.R")
source("src/lib/synth.R")

wtd.median <- function(xx, weights=NULL, normwt=F) {
    if (all(is.na(xx)))
        return(NA)
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

load(paste0("data/allyr-ww-", persist, "-", trade.method, ".RData"))
allyr.ww[allyr.ww$ISO == 'SDN', which(is.na(allyr.ww[allyr.ww$ISO == 'ABW', ][1, ]))] <- NA # country change affects
for2023 <- subset(allyr.ww, Year == 2022)
stopifnot(all(for2023$ISO == allyr.ww$ISO[allyr.ww$Year == 2023]))
for2023[, 1:8] <- subset(allyr.ww, Year == 2023)[, 1:8]
allyr.ww <- rbind(subset(allyr.ww, Year <= 2022), for2023)
allyr.ww <- allyr.ww %>% group_by(ISO, mc) %>% arrange(Year) %>%
    mutate(across(dimpact:weight.norm, ~ stats::filter(., rep(1 / 10, 10), sides=1)))

## RIOTOTALS

levelprep <- prep.levels.allyr.ww(allyr.ww)

if (do.for.subset == "L+MIC") {
    levelprep <- levelprep %>%
        filter(INCOME_GRP %in% c("5. Low income", "4. Lower middle income", "3. Upper middle income"))
}

pdf2 <- levelprep %>% group_by(Year, mc) %>%
    dplyr::summarize(totimpact.usd=sum(totimpact.usd, na.rm=T),
                     tradeimpact.usd=sum(tradeimpact.usd, na.rm=T),
                     slrimpact.usd=sum(slrimpact.usd, na.rm=T),
                     solow.usd=sum(solow.usd, na.rm=T),
                     total.usd=sum(total.usd, na.rm=T),
                     allcap.usd=sum(allcap.usd, na.rm=T),
                     totalandcap.usd=sum(total.usd, na.rm=T) + allcap.usd,
                     weight2=sum(weight.norm))

## Numbers for report
pdf2 %>% filter(Year > 1993) %>% group_by(mc) %>%
    dplyr::summarize(totimpact.usd=sum(totimpact.usd), tradeimpact.usd=sum(tradeimpact.usd),
                     slrimpact.usd=sum(slrimpact.usd), solow.usd=sum(solow.usd),
                     allcap.usd=sum(allcap.usd), totalandcap.usd=sum(totalandcap.usd), weight2=sum(weight2)) %>%
        dplyr::summarize(totimpact.usd.mu=wtd.median(totimpact.usd, weights=weight2, normwt=T),
                         totimpact.usd.ci25=wtd.quantile(totimpact.usd, .25, weights=weight2, normwt=T),
                         totimpact.usd.ci75=wtd.quantile(totimpact.usd, .75, weights=weight2, normwt=T),
                         totalandcap.usd.mu=wtd.median(totalandcap.usd, weights=weight2, normwt=T),
                         totalandcap.usd.ci25=wtd.quantile(totalandcap.usd, .25, weights=weight2, normwt=T),
                         totalandcap.usd.ci75=wtd.quantile(totalandcap.usd, .75, weights=weight2, normwt=T))
## R2-Total:
## L+MIC:
##   totimpact.usd.mu totimpact.usd.ci25 totimpact.usd.ci75 totalandcap.usd.mu totalandcap.usd.ci25 totalandcap.usd.ci75
## 1           -9710.            -12319.             -6325.            -16918.              -20180.              -11528.

## Random Forest:
## Global:
##   totimpact.usd.mu totimpact.usd.ci25 totimpact.usd.ci75 totalandcap.usd.mu totalandcap.usd.ci25 totalandcap.usd.ci75
## 1          -36976.            -53605.            -18994.            -55282.              -77724.              -34295.
## L+MIC:
##   totimpact.usd.mu totimpact.usd.ci25 totimpact.usd.ci75 totalandcap.usd.mu totalandcap.usd.ci25 totalandcap.usd.ci75
## 1          -15843.            -26233.             -6783.            -26247.              -44527.              -15447.

## How much more by year
levelprep %>% group_by(Year, mc) %>%
    dplyr::summarize(totimpact.usd=sum(totimpact.usd, na.rm=T),
                     tradeimpact.usd=sum(tradeimpact.usd, na.rm=T),
                     slrimpact.usd=sum(slrimpact.usd, na.rm=T),
                     solow.usd=sum(solow.usd, na.rm=T),
                     allcap.usd=sum(allcap.usd, na.rm=T),
                     totalandcap.usd=sum(total.usd, na.rm=T) + allcap.usd,
                     weight2=sum(weight.norm)) %>%
        filter(Year >= 2014) %>% group_by(mc) %>%
        dplyr::summarize(totalandcap.usd=mean(totalandcap.usd, na.rm=T), weight2=mean(weight2)) %>%
        dplyr::summarize(ci25=wtd.quantile(totalandcap.usd, .25, weights=weight2, normwt=T),
                         ci75=wtd.quantile(totalandcap.usd, .75, weights=weight2, normwt=T), totalandcap.usd=wtd.mean(totalandcap.usd, weights=weight2, normwt=T))
## R2-Total:
## L+MIC:
##    ci25  ci75 totalandcap.usd
## 1 -1215. -665.          -1131.

## Random Forest:
## Global:
##     ci25   ci75 totalandcap.usd
## 1 -4175. -1583.          -3410.
## L+MIC:
##     ci25  ci75 totalandcap.usd
## 1 -2704. -882.          -1932.

## Calculate a total of produce capital, as a comparison

df.pro2 <- read.iw("data/capital/tabula-C-produced.csv", 'Produced Capital')
isos <- unique(levelprep$ISO)
sum(df.pro2$`Produced Capital`[df.pro2$ISO %in% isos & df.pro2$Year == 2014], na.rm=T) # Billion 2005 USD

df.prowb <- read_excel("data/capital/World Bank Produced.xlsx", 1)
sum(df.prowb$`2020 [YR2020]`[df.prowb$`Country Code` %in% isos]) / 1e9

## R2-Total:
## L+MIC:
## 16918 / 52233.76

## Rich countries
sum(df.prowb$`2020 [YR2020]`[!(df.prowb$`Country Code` %in% isos)], na.rm=T) / 1e9
##
