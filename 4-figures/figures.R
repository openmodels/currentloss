## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")
## setwd("~/research/currentloss")

library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(PBSmapping)
library(countrycode)

persist = 0.08
source("src/lib/utils2.R")

load("data/allyr-ww.RData")

sumbymc2 <- allyr.ww %>% filter(Year > 2013) %>% group_by(ISO, mc) %>%
    dplyr::summarize(totimpact=mean(totimpact), slrloss=mean(slrloss), tradeloss=mean(tradeloss), product.chg=mean(product.chg),
                     rencap.chg.ccpc=mean(rencap.chg.ccpc), rencap.chg=mean(1 - rencap.nocc / rencap.true),
                     allcap.chg=mean(1 - allcap.nocc / allcap.true), procap.chg=mean(allcap.chg - rencap.chg),
                     weight=mean(weight.norm))

wtd.median <- function(xx, weights=NULL, normwt=F) {
    if (all(is.na(xx)))
        return(NA)
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

my.wtd.quantile <- function(xx, qq, weights=NULL, normwt=T) {
    if (all(is.na(xx)))
        return(NA)
    wtd.quantile(xx, qq, weights=weights, normwt=normwt, na.rm=T)
}

sumbyiso <- sumbymc2 %>% filter(is.na(weight) | weight > 1e-10) %>% group_by(ISO) %>%
    dplyr::summarize(solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact - -tradeloss - -slrloss, weights=weight, normwt=T)), slrloss=wtd.median(slrloss, weights=weight, normwt=T), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + -tradeloss - slrloss, weights=weight, normwt=T), wtd.median(product.chg, weights=weight, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .25, weights=weight, normwt=T), wtd.quantile(product.chg, .25, weights=weight, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .75, weights=weight, normwt=T), wtd.quantile(product.chg, .75, weights=weight, normwt=T)), rencap.chg.direct=wtd.median(rencap.chg.ccpc, weights=weight, normwt=T), rencap.chg=wtd.median(rencap.chg, weights=weight, normwt=T), procap.chg=wtd.median(procap.chg, weights=weight, normwt=T), totimpact=wtd.median(totimpact, weights=weight, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight, normwt=T), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight, normwt=T))

## ## Add missing ISOs
## toadd <- presolow[!(presolow$ISO %in% sumbyiso$ISO),] %>% group_by(ISO) %>%
##     dplyr::summarize(totimpact=median(totimpact), tradeloss=median(tradeloss), slrloss=median(slrloss),
##                      prod25=quantile(totimpact - tradeloss - slrloss, .25), prod75=quantile(totimpact - tradeloss - slrloss, .75)) %>%
##     mutate(total=totimpact + -tradeloss + -slrloss)

## sumbyiso2 <- rbind(sumbyiso,
##                    cbind(toadd, solow=NA, rencap.chg.direct=NA, rencap.chg=NA, procap.chg=NA, allcap.chg=NA, cap25=NA, cap75=NA))

shp <- importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
polydata <- attr(shp, 'PolyData')

## Construct table

format.percent <- function(xx) {
    ifelse(is.na(xx), NA, paste0(round(xx * 100, 1), "%"))
}

format.range <- function(x0, x1, ispercent=T) {
    if (ispercent)
        ifelse(is.na(x0), NA, paste0(floor(x0 * 100), " - ", ceil(x1 * 100), "%"))
    else
        ifelse(is.na(x0), NA, paste0(floor(x0), " - ", ceil(x1)))
}

df.pop2 <- read.wb("data/capital/API_SP.POP.TOTL_DS2_en_excel_v2_5871620.xls", 'Population')
df.pop3 <- subset(df.pop2, `Country Code` %in% unique(df.pop2$`Country Code`[!is.na(df.pop2$Population)]) & !(`Country Code` %in% c("LIE", 'NCL'))) %>% group_by(`Country Code`) %>%
    reframe(Year=Year, Population.est=approx(Year, Population, Year, rule=2)$y)
df.pro2 <- read.iw("data/capital/tabula-C-produced.csv", 'Produced Capital')
df.ren2 <- read.iw("data/capital/tabula-A2-renewable.csv", 'Renewable Capital')

log2lev <- function(xx) {
    exp(xx) - 1
}

allyr.ww$total <- ifelse(is.na(allyr.ww$product.chg), allyr.ww$totimpact + allyr.ww$itlimpact - allyr.ww$slrloss, allyr.ww$product.chg)
isotot <- allyr.ww %>% left_join(df.gdp3, by=c('ISO'='Country Code', 'Year')) %>%
    ## want f * NoCC, But (1+f) * NoCC = Obs, So (f / (1+f)) Obs
    mutate(total.usd=(log2lev(total) / (1 + log2lev(total))) * GDP.2015.est) %>%
    filter(Year > 1992) %>% # Only years after Rio
    group_by(ISO, mc) %>% dplyr::summarize(total.usd=sum(total.usd, na.rm=T), weight.norm=mean(weight.norm))
isotot2 <- isotot %>% filter(!is.na(total.usd)) %>% group_by(ISO) %>%
    dplyr::summarize(prod25=wtd.quantile(total.usd, .25, weights=weight.norm, normwt=T), prod75=wtd.quantile(total.usd, .75, weights=weight.norm, normwt=T), total.sum=wtd.median(total.usd, weights=weight.norm, normwt=T))

tbldf <- sumbyiso %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
    left_join(subset(df.gdp3, Year == 2022), by=c('ISO'='Country Code')) %>%
    left_join(subset(df.pop3, Year == 2022), by=c('ISO'='Country Code')) %>%
    left_join(subset(df.pro2, Year == 2014), by='ISO', suffix=c('', '.pro')) %>%
    left_join(subset(df.ren2, Year == 2014), by='ISO', suffix=c('', '.pro')) %>%
    left_join(isotot2, by='ISO', suffix=c('', '.sum')) %>%
    dplyr::arrange(ADMIN)

tbldf$gdppc <- tbldf$GDP.2015.est / tbldf$Population.est

range(tbldf$gdppc[tbldf$ECONOMY == "2. Developed region: nonG7"], na.rm=T)
range(tbldf$gdppc[tbldf$ECONOMY == "6. Developing region"], na.rm=T)
range(tbldf$gdppc[tbldf$ECONOMY == "7. Least developed region"], na.rm=T)

tbl <- data.frame(country=tbldf$ADMIN,
                  totimpact=format.percent(log2lev(tbldf$totimpact)),
                  tradeloss=format.percent(log2lev(tbldf$tradeloss)),
                  slrloss=format.percent(log2lev(tbldf$slrloss)),
                  solow=format.percent(log2lev(tbldf$solow)),
                  total=format.percent(log2lev(tbldf$total)),
                  prodiqr=format.range(log2lev(tbldf$prod25), log2lev(tbldf$prod75)),
                  procap.chg=format.percent(log2lev(tbldf$procap.chg)),
                  rencap.chg.direct=format.percent(log2lev(tbldf$rencap.chg.direct)),
                  rencap.chg.feedback=format.percent(log2lev(tbldf$rencap.chg) - log2lev(tbldf$rencap.chg.direct)),
                  allcap.chg=format.percent(log2lev(tbldf$allcap.chg)),
                  capiqr=format.range(log2lev(tbldf$cap25), log2lev(tbldf$cap75)),
                  ## want f * NoCC, But (1+f) * NoCC = Obs, So (f / (1+f)) Obs
                  prodchg.2015=round((log2lev(tbldf$total) / (1 + log2lev(tbldf$total))) * tbldf$GDP.2015.est / 1e9),
                  procapchg.2015=round((log2lev(tbldf$procap.chg) / (1 + log2lev(tbldf$procap.chg))) * tbldf$`Produced Capital` * 100 / 83.6),
                  rencapchg.direct.2015=round((log2lev(tbldf$rencap.chg.direct) / (1 + log2lev(tbldf$rencap.chg.direct))) * tbldf$`Renewable Capital` * 100 / 83.6),
                  rencapchg.feedback.2015=round((log2lev(tbldf$rencap.chg) / (1 + log2lev(tbldf$rencap.chg))) * tbldf$`Renewable Capital` * 100 / 83.6) - round((log2lev(tbldf$rencap.chg.direct) / (1 + log2lev(tbldf$rencap.chg.direct))) * tbldf$`Renewable Capital` * 100 / 83.6),
                  total.sum=round(tbldf$total.sum / 1e9),
                  total.sum.iqr=format.range(floor(tbldf$prod25.sum / 1e10) * 10, ceil(tbldf$prod75.sum / 1e10) * 10, ispercent=F),
                  INCOME_GRP=tbldf$INCOME_GRP, ISO=tbldf$ISO)

library(flextable)
tbl2 <- tbl %>% group_by(INCOME_GRP) %>% dplyr::summarize(prodchg.2015=round(sum(prodchg.2015, na.rm=T), -1),
                                                          prod.sum=round(sum(total.sum, na.rm=T), -1),
                                                          procapchg.2015=round(sum(procapchg.2015, na.rm=T), -1),
                                                          rencapchg.direct.2015=round(sum(rencapchg.direct.2015, na.rm=T), -1),
                                                          rencapchg.feedback.2015=round(sum(rencapchg.feedback.2015, na.rm=T), -1),
                                                          landd.sum=-(prod.sum + procapchg.2015 + rencapchg.direct.2015 + rencapchg.feedback.2015))

tbl2 <- rbind(tbl2, data.frame(INCOME_GRP=c("High income (total)", "Low and middle income"),
                               prodchg.2015=round(c(sum(tbl2$prodchg.2015[1:2]), sum(tbl2$prodchg.2015[3:5])), -1),
                               prod.sum=round(c(sum(tbl2$prod.sum[1:2]), sum(tbl2$prod.sum[3:5])), -1),
                               procapchg.2015=round(c(sum(tbl2$procapchg.2015[1:2]), sum(tbl2$procapchg.2015[3:5])), -1),
                               rencapchg.direct.2015=round(c(sum(tbl2$rencapchg.direct.2015[1:2]), sum(tbl2$rencapchg.direct.2015[3:5])), -1),
                               rencapchg.feedback.2015=round(c(sum(tbl2$rencapchg.feedback.2015[1:2]), sum(tbl2$rencapchg.feedback.2015[3:5])), -1),
                               landd.sum=round(c(sum(tbl2$landd.sum[1:2]), sum(tbl2$landd.sum[3:5])), -1)))

names(tbl2) <- c("Income Group", "2022 GDP Change ($billion)", "30-year GDP Change ($billion)", "Produced Capital Change ($billion)", "Renewable Capital Direct Change ($billion)", "Renewable Capital Feedback Change ($billion)", "Total Loss ($billion)")

print(flextable(tbl2))

## Construct a voting bloc table

tbl2grp <- data.frame()
for (grouping in names(groupings)) {
    isos <- countryname(groupings[[grouping]], 'iso3c')
    tbl2grp.row <- tbl %>% filter(ISO %in% isos) %>%
        dplyr::summarize(prodchg.2015=round(sum(prodchg.2015, na.rm=T), -1),
                         prod.sum=round(sum(total.sum, na.rm=T), -1),
                         procapchg.2015=round(sum(procapchg.2015, na.rm=T), -1),
                         rencapchg.direct.2015=round(sum(rencapchg.direct.2015, na.rm=T), -1),
                         rencapchg.feedback.2015=round(sum(rencapchg.feedback.2015, na.rm=T), -1),
                         landd.sum=-(prod.sum + procapchg.2015 + rencapchg.direct.2015 + rencapchg.feedback.2015))
    tbl2grp <- rbind(tbl2grp, cbind(Group=grouping, tbl2grp.row))
}

names(tbl2grp) <- c("Party", "2022 GDP Change ($billion)", "30-year GDP Change ($billion)", "Produced Capital Change ($billion)", "Renewable Capital Direct Change ($billion)", "Renewable Capital Feedback Change ($billion)", "Total Loss ($billion)")

print(flextable(tbl2grp[order(tbl2grp$Party),]))

incl <- T #(!is.na(tbl$prodchg.2015) & abs(tbl$prodchg.2015) > 0) | !is.na(tbl$rencap.chg.feedback)
tbl$prodchg.2015 <- as.character(tbl$prodchg.2015)
tbl$rencapchg.direct.2015 <- as.character(tbl$rencapchg.direct.2015)
tbl$rencapchg.feedback.2015 <- as.character(tbl$rencapchg.feedback.2015)
tbl$procapchg.2015 <- as.character(tbl$procapchg.2015)
tbl$total.sum <- as.character(tbl$total.sum)

names(tbl) <- c("Country", "Direct", "International", "Capital", "Total", "IQR", "Produced", "Direct", "Feedback", "Total", "IQR", "GDP ($) - DROP", "Prod. Cap. ($)", "Direct ($)", "Feedback ($)", "Loss ($)", "IQR", "DROP", "DROP")
tbl$Country <- as.character(tbl$Country)
tbl$Country[tbl$Country == "Democratic Republic of the Congo"] <- "DR Congo"
tbl$Country[tbl$Country == "Central African Republic"] <- "Central African Rep."
tbl$Country[tbl$Country == "United Republic of Tanzania"] <- "United Rep. of Tanzania"
tbl$Country[tbl$Country == "United States of America"] <- "USA"
library(xtable)
print(xtable(tbl[incl, -grep("DROP", names(tbl))]), tabular.environment='longtable', floating=F, include.rownames=F)

## Bars by ISO

sumprod <- melt(sumbyiso[, c('ISO', 'totimpact', 'tradeloss', 'slrloss', 'solow')], 'ISO')
sumcap <- melt(sumbyiso[, c('ISO', 'rencap.chg', 'procap.chg')], 'ISO')

sumprod$label <- ifelse(sumprod$variable == 'totimpact', "Direct Impact",
                 ifelse(sumprod$variable == 'tradeloss', "International Impact",
                 ifelse(sumprod$variable == 'slrloss', "Coastal Impact", "Capital Impact")))
sumprod$label <- factor(sumprod$label, levels=rev(c("Direct Impact", "International Impact", "Capital Impact", "Coastal Impact")))

ggplot(sumbyiso, aes(ISO)) +
    coord_flip(ylim=c(-.25, 0.6)) +
    geom_col(data=sumprod, aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=log2lev(prod25), ymax=log2lev(prod75))) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw()
ggsave("figures/finalprod-byiso.pdf", width=6.5, height=26)

sumcap$label <- ifelse(sumcap$variable == 'rencap.chg', "Renewable Capital", "Produced Capital")

ggplot(subset(sumbyiso, !is.na(allcap.chg)), aes(ISO)) +
    coord_flip() + #ylim=c(-.25, 0.6)) +
    geom_col(data=subset(sumcap, !is.na(value)), aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=cap25, ymax=cap75)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Cumulative change in Capital (%)") + xlab(NULL) + theme_bw()
ggsave("figures/finalcap-byiso.pdf", width=6.5, height=15)

## NUMBERS FOR REPORT
subset(sumbyiso, ISO == "USA")
subset(sumbyiso, ISO == "CHN")
subset(sumbyiso, ISO == "RUS")
subset(sumbyiso, ISO == "SAU")

## Grouping

newweight <- sumbymc2 %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
    group_by(ISO) %>% reframe(weight2=weight / sum(weight))
sumbymc2$weight.norm <- newweight$weight2

toaddmc <- !(presolow$ISO %in% sumbyiso$ISO)

sumbymc3 <- rbind(sumbymc2[, c('ISO', 'mc', 'totimpact', 'tradeloss', 'slrloss', 'rencap.end.true', 'rencap.end.nocc',
                               'procap.end.true', 'procap.end.nocc',
                               'product.chg', 'rencap.chg', 'procap.chg', 'weight.norm', 'product.chg')],
                  data.frame(ISO=presolow$ISO[toaddmc], mc=presolow$mc[toaddmc], totimpact=presolow$totimpact[toaddmc],
                             tradeloss=presolow$tradeloss[toaddmc], slrloss=-presolow$slrloss[toaddmc], rencap.end.true=NA, rencap.end.nocc=NA,
                             procap.end.true=NA, procap.end.nocc=NA,
                             product.chg=presolow$totimpact[toaddmc] - presolow$tradeloss[toaddmc], rencap.chg=NA,
                             procap.chg=NA, weight.norm=1/30, product.chg=presolow$totimpact[toaddmc] - presolow$tradeloss[toaddmc] - presolow$slrloss[toaddmc]))

sumbymc4 <- sumbymc3 %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
    mutate(weight.pop = weight.norm * POP_EST)

sumbymc4$MY_INCOME_GRP <- as.character(sumbymc4$INCOME_GRP)
sumbymc4$MY_INCOME_GRP[sumbymc4$INCOME_GRP == "2. High income: nonOECD" & sumbymc4$REGION_WB == "East Asia & Pacific"] <- "2. High income: East Asia & Pacific"
sumbymc4$MY_INCOME_GRP[sumbymc4$INCOME_GRP == "2. High income: nonOECD" & sumbymc4$REGION_WB == "Middle East & North Africa"] <- "2. High income: Mid. East & N. Africa"

tbldf$ADMIN[tbldf$INCOME_GRP == "2. High income: nonOECD" & tbldf$REGION_WB == "East Asia & Pacific"]
tbldf$POP_EST[tbldf$INCOME_GRP == "2. High income: nonOECD" & tbldf$REGION_WB == "East Asia & Pacific"] / sum(tbldf$POP_EST[tbldf$INCOME_GRP == "2. High income: nonOECD" & tbldf$REGION_WB == "East Asia & Pacific"])

tbldf$ADMIN[tbldf$INCOME_GRP == "2. High income: nonOECD" & tbldf$REGION_WB == "Middle East & North Africa"]
tbldf$POP_EST[tbldf$INCOME_GRP == "2. High income: nonOECD" & tbldf$REGION_WB == "Middle East & North Africa"] / sum(tbldf$POP_EST[tbldf$INCOME_GRP == "2. High income: nonOECD" & tbldf$REGION_WB == "Middle East & North Africa"])

tbldf$ADMIN[tbldf$INCOME_GRP == "2. High income: nonOECD" & !(tbldf$REGION_WB %in% c("East Asia & Pacific", "Middle East & North Africa"))]
tbldf$POP_EST[tbldf$INCOME_GRP == "2. High income: nonOECD" & !(tbldf$REGION_WB %in% c("East Asia & Pacific", "Middle East & North Africa"))] / sum(tbldf$POP_EST[tbldf$INCOME_GRP == "2. High income: nonOECD" & !(tbldf$REGION_WB %in% c("East Asia & Pacific", "Middle East & North Africa"))])

tbldf$ADMIN[tbldf$INCOME_GRP == "3. Upper middle income"]
tbldf$POP_EST[tbldf$INCOME_GRP == "3. Upper middle income"] / sum(tbldf$POP_EST[tbldf$INCOME_GRP == "3. Upper middle income"])

tbldf$ADMIN[tbldf$INCOME_GRP == "4. Lower middle income"]
tbldf$POP_EST[tbldf$INCOME_GRP == "4. Lower middle income"] / sum(tbldf$POP_EST[tbldf$INCOME_GRP == "4. Lower middle income"])

tbldf$ADMIN[tbldf$INCOME_GRP == "5. Low income"]
tbldf$POP_EST[tbldf$INCOME_GRP == "5. Low income"] / sum(tbldf$POP_EST[tbldf$INCOME_GRP == "5. Low income"])

sumbyeconomy <- sumbymc4 %>% filter(is.na(weight.pop) | weight.pop > 1e-9) %>% group_by(ECONOMY) %>% dplyr::summarize(solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact - -tradeloss - -slrloss, weights=weight.pop, normwt=T)), slrloss=wtd.median(slrloss, weights=weight.pop, normwt=T), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + -tradeloss - slrloss, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), totimpact=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T))
sumbyincgrp <- sumbymc4 %>% filter(is.na(weight.pop) | weight.pop > 1e-9) %>% group_by(MY_INCOME_GRP) %>% dplyr::summarize(solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact - -tradeloss - -slrloss, weights=weight.pop, normwt=T)), slrloss=wtd.median(slrloss, weights=weight.pop, normwt=T), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + -tradeloss - slrloss, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), totimpact=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T))
sumbycontinent <- sumbymc4 %>% filter(is.na(weight.pop) | weight.pop > 1e-9) %>% group_by(CONTINENT) %>% dplyr::summarize(solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact - -tradeloss - -slrloss, weights=weight.pop, normwt=T)), slrloss=wtd.median(slrloss, weights=weight.pop, normwt=T), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + -tradeloss - slrloss, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), totimpact=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T))
sumbysubreg <- sumbymc4 %>% filter(is.na(weight.pop) | weight.pop > 1e-9) %>% group_by(SUBREGION) %>% dplyr::summarize(solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact - -tradeloss - -slrloss, weights=weight.pop, normwt=T)), slrloss=wtd.median(slrloss, weights=weight.pop, normwt=T), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + -tradeloss - slrloss, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), totimpact=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T))
names(sumbyeconomy)[1] <- "Group"
names(sumbyincgrp)[1] <- "Group"
names(sumbycontinent)[1] <- "Group"
names(sumbysubreg)[1] <- "Group"

allsums <- rbind(cbind(sumbyeconomy, panel='Economy'),
                 cbind(sumbyincgrp, panel='Income Group'),
                 cbind(sumbycontinent, panel='Continent'),
                 cbind(sumbysubreg, panel='Sub-region'))

allsums$Group <- as.character(allsums$Group)
allsums$Group[allsums$panel == 'Economy'] <- sapply(allsums$Group[allsums$panel == 'Economy'], function(ss) substring(ss, 4, nchar(ss)))
allsums$Group[allsums$Group == 'Developed region: nonG7'] <- "Developed region: non-G7"
allsums$Group[allsums$panel == 'Income Group'] <- sapply(allsums$Group[allsums$panel == 'Income Group'], function(ss) substring(ss, 4, nchar(ss)))
allsums$Group[allsums$Group == 'High income: nonOECD'] <- "High income: Remaining"

## NUMBERS FOR REPORT
subset(allsums, Group == "Least developed region") # -0.08310059
subset(allsums, Group == "South-Eastern Asia")
log2lev(allsums$total[allsums$Group == "South-Eastern Asia"]) # -0.3386061
subset(allsums, Group == "Southern Africa") # -0.1121914
log2lev(allsums$total[allsums$Group == "Southern Africa"]) # -0.0625672
subset(allsums, Group == "Africa") # -0.08088869
subset(allsums, Group == "Europe") # 0.04706247
log2lev(allsums$total[allsums$Group == "Europe"]) # 0.009311007
subset(allsums, Group == "Central Asia") # 0.0471803
subset(allsums, Group == "1. High income: OECD") # 0.0471803
log2lev(allsums$total[allsums$Group == "South America"]) # -0.1024196

## Drop sub-regions thar are continents
allsums <- subset(allsums, panel != "Sub-region" | !(Group %in% unique(sumbycontinent$Group)))
allsums$Group <- factor(allsums$Group, levels=rev(unique(allsums$Group)))

allsumprod <- melt(allsums[, c('panel', 'Group', 'totimpact', 'tradeloss', 'slrloss', 'solow')], c('panel', 'Group'))
allsumcap <- melt(allsums[, c('panel', 'Group', 'rencap.chg', 'procap.chg')], c('panel', 'Group'))

## Bars by group

allsumprod$label <- ifelse(allsumprod$variable == 'totimpact', "Direct",
                    ifelse(allsumprod$variable == 'tradeloss', "International",
                    ifelse(allsumprod$variable == 'slrloss', "Coastal", "Capital")))
allsumprod$label <- factor(allsumprod$label, levels=rev(c("Direct", "International", "Capital", "Coastal")))

ggplot(subset(allsums2, panel == 'Economy'), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(allsumprod, panel == 'Economy'), aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=log2lev(prod25), ymax=log2lev(prod75)), width=.5) +
    geom_point(aes(y=log2lev(total))) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-byeco.pdf", width=5, height=2.7)

ggplot(subset(allsums2, panel == 'Income Group'), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(allsumprod, panel == 'Income Group'), aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=log2lev(prod25), ymax=log2lev(prod75)), width=.5) +
    geom_point(aes(y=log2lev(total))) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-byeco2.pdf", width=6.2, height=2.7)

ggplot(subset(allsums2, panel %in% c('Continent', 'Sub-region')), aes(Group)) +
    coord_flip() +
    facet_grid(panel ~ ., scales='free', space='free') +
    geom_col(data=subset(allsumprod, panel %in% c('Continent', 'Sub-region')), aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=log2lev(prod25), ymax=log2lev(prod75)), width=.5) +
    geom_point(aes(y=log2lev(total))) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-byreg.pdf", width=5, height=6)

allsumcap$label <- ifelse(allsumcap$variable == 'rencap.chg', "Renewable Capital", "Produced Capital")

ggplot(subset(allsums, !is.na(allcap.chg) & panel == 'Economy'), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(allsumcap, !is.na(value) & panel == 'Economy'), aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=cap25, ymax=cap75)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Cumulative change in Capital (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalcap-byeco.pdf", width=5, height=2.7)

ggplot(subset(allsums, !is.na(allcap.chg) & panel %in% c('Continent', 'Sub-region')), aes(Group)) +
    coord_flip() +
    facet_grid(panel ~ ., scales='free', space='free') +
    geom_col(data=subset(allsumcap, !is.na(value) & panel %in% c('Continent', 'Sub-region')), aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=cap25, ymax=cap75)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Cumulative change in Capital (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalcap-byreg.pdf", width=5, height=6)

## Other groupings

sumbygroup <- data.frame()
for (grouping in names(groupings)) {
    isos <- countryname(groupings[[grouping]], 'iso3c')
    sumbysubgroup <- sumbymc4 %>% filter(ISO %in% isos & !is.na(weight.pop) & weight.pop > 1e-9) %>% group_by(Group=grouping) %>% dplyr::summarize(solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact - -tradeloss - -slrloss, weights=weight.pop, normwt=T)), slrloss=wtd.median(slrloss, weights=weight.pop, normwt=T), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + -tradeloss - slrloss, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + -tradeloss - slrloss, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), totimpact=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T))
    sumbygroup <- rbind(sumbygroup, sumbysubgroup)
}
sumbygroup$Group <- factor(sumbygroup$Group, levels=rev(sort(sumbygroup$Group)))

sumbygroupprod <- melt(sumbygroup[, c('Group', 'totimpact', 'tradeloss', 'slrloss', 'solow')], 'Group')
sumbygroupcap <- melt(sumbygroup[, c('Group', 'rencap.chg', 'procap.chg')], 'Group')

sumbygroupprod$label <- ifelse(sumbygroupprod$variable == 'totimpact', "Direct",
                        ifelse(sumbygroupprod$variable == 'tradeloss', "International",
                        ifelse(sumbygroupprod$variable == 'slrloss', "Coastal", "Capital")))
sumbygroupprod$label <- factor(sumbygroupprod$label, levels=rev(c("Direct", "International", "Capital", "Coastal")))

ggplot(sumbygroup, aes(Group)) +
    coord_flip() +
    geom_col(data=sumbygroupprod, aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=log2lev(prod25), ymax=log2lev(prod75)), width=.5) +
    geom_point(aes(y=log2lev(total))) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-bygrp.pdf", width=5, height=4)

sumbygroupcap$label <- ifelse(sumbygroupcap$variable == 'rencap.chg', "Renewable Capital", "Produced Capital")

ggplot(subset(sumbygroup, !is.na(allcap.chg)), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(sumbygroupcap, !is.na(value)), aes(y=log2lev(value), fill=label)) +
    geom_errorbar(aes(ymin=cap25, ymax=cap75)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Cumulative change in Capital (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalcap-bygrp.pdf", width=5, height=4)

## NUMBERS FOR REPORT
subset(sumbygroup, Group == "G77") # -0.0548
subset(sumbygroup, Group == "CVF") # -0.0987
subset(sumbygroup, Group == "AOSIS") # -0.0430
subset(sumbygroup, Group == "EU") # -0.0430
subset(sumbygroup, Group == "Africa") # -0.0804
subset(sumbygroup, Group == "LDCs")

## Maps

cents <- calcCentroid(shp, rollup=2)
areas <- calcArea(shp, rollup=2)
centroids <- cents %>% left_join(areas, by=c('PID', 'SID')) %>% group_by(PID) %>%
    dplyr::summarize(X=X[which.max(area)], Y=Y[which.max(area)])

source("~/projects/research-common/R/distance.R")
centroids$show <- F
for (PID in order(polydata$POP_EST, decreasing=T)) {
    dists <- gcd.slc(centroids$X[PID], centroids$Y[PID], centroids$X[centroids$show], centroids$Y[centroids$show])
    if (all(dists > 600))
        centroids$show[PID] <- T
}
centroids$show[centroids$X < -176] <- F
centroids$show[centroids$X > 176] <- F
centroids$show[centroids$Y < -50] <- F
centroids$show[centroids$Y > 65] <- F

sumbyiso$ISO %in% polydata$ADM0_A3

shp2 <- shp %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(sumbyiso, by=c('ADM0_A3'='ISO'))
centroids2 <- centroids %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(sumbyiso, by=c('ADM0_A3'='ISO'))

shpl <- importShapefile("data/regions/ne_10m_land/ne_10m_land.shp")

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
                                        #geom_polygon(aes(fill=pmin(.2, pmax(-.25, exp(total) - 1)), group=paste(PID, SID))) +
    geom_polygon(aes(fill=exp(total) - 1, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=subset(centroids2, show), aes(label=round((exp(total) - 1) * 100)), size=3, label.padding=unit(0.1, "lines")) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_gradient2("Change in GDP (%):", low = scales::muted("red"), high = scales::muted("blue"), labels=scales::percent, guide=guide_colorbar(barwidth=10, barheight=1)) +
    theme_bw() + theme(legend.position="bottom", legend.key.width=unit(10,"in"),
                       legend.key.height=unit(1,"in"))
ggsave("figures/finalprod-map.pdf", width=10, height=5.5)

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=exp(allcap.chg) - 1, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=centroids2, aes(label=round((exp(allcap.chg) - 1) * 100)), size=2, label.padding=unit(0.1, "lines")) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_gradient2("Change in capital (%):", low = scales::muted("red"), high = scales::muted("blue"), labels=scales::percent, guide=guide_colorbar(barwidth=10, barheight=1)) +
    theme_bw() + theme(legend.position="bottom", legend.key.width=unit(10,"in"),
                       legend.key.height=unit(1,"in"))
ggsave("figures/finalcap-map.pdf", width=10, height=4)
