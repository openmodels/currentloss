## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
source("~/projects/research-common/R/myPBSmapping.R")
library(countrycode)

persist = 0.36
trade.method <- 'dd-mcr2all'
source("src/lib/utils2.R")
source("src/lib/synth.R")

allyr.ww <- get.allyr.ww(persist, trade.method)

## FIGURES

sumbymc2 <- allyr.ww %>% group_by(ISO, mc) %>%
    dplyr::summarize(across(dimpact:weight.norm, ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
    group_by(ISO, mc) %>%
    mutate(slrimpact=-slrloss, tradeimpact=-tradeloss,
           rencap.chg=1 - rencap.nocc / rencap.true,
           allcap.chg=1 - allcap.nocc / allcap.true, procap.chg=allcap.chg - rencap.chg,
           weight=weight.norm)

## sumbymc2 <- allyr.ww %>% filter(Year > 2013) %>% group_by(ISO, mc) %>%
##     dplyr::summarize(totimpact=mean(totimpact), slrimpact=-mean(slrloss), tradeimpact=-mean(tradeloss), product.chg=mean(product.chg, na.rm=T),
##                      rencap.chg.ccpc=mean(rencap.chg.ccpc, na.rm=T), rencap.chg=mean(1 - rencap.nocc / rencap.true, na.rm=T),
##                      allcap.chg=mean(1 - allcap.nocc / allcap.true, na.rm=T), procap.chg=mean(allcap.chg - rencap.chg, na.rm=T),
##                      weight=mean(weight.norm))

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
    dplyr::summarize(totimpact.median=wtd.median(totimpact, weights=weight, normwt=T), tradeimpact.median=wtd.median(tradeimpact, weights=weight, normwt=T), slrimpact.median=wtd.median(slrimpact, weights=weight, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact.median - tradeimpact.median - slrimpact.median, weights=weight, normwt=T)), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact.median + tradeimpact.median + slrimpact.median, weights=weight, normwt=T), wtd.median(product.chg, weights=weight, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .25, weights=weight, normwt=T), wtd.quantile(product.chg, .25, weights=weight, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .75, weights=weight, normwt=T), wtd.quantile(product.chg, .75, weights=weight, normwt=T)), rencap.chg.direct=wtd.median(rencap.chg.ccpc, weights=weight, normwt=T), rencap.chg=wtd.median(rencap.chg, weights=weight, normwt=T), procap.chg=wtd.median(procap.chg, weights=weight, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight, normwt=T), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight, normwt=T))

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

levelprep <- prep.levels.allyr.ww(allyr.ww)

isotot <- levelprep %>%
    filter(Year > 1993) %>% # Only years after Rio
    group_by(ISO, mc) %>%
    dplyr::summarize(total.usd=sum(total.usd * 1e9, na.rm=T), weight.norm=mean(weight.norm))
isotot2 <- isotot %>% filter(!is.na(total.usd)) %>% group_by(ISO) %>%
    dplyr::summarize(prod25=wtd.quantile(total.usd, .25, weights=weight.norm, normwt=T), prod75=wtd.quantile(total.usd, .75, weights=weight.norm, normwt=T), total.sum=wtd.median(total.usd, weights=weight.norm, normwt=T))

caplevel <- levelprep %>% group_by(ISO, mc) %>%
    dplyr::summarize(across(c(procapchg.usd, rencapchg.direct.usd, rencapchg.feedback.usd,
                              weight.norm), ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
    group_by(ISO, mc) %>%
    filter(is.na(weight.norm) | weight.norm > 1e-10) %>% group_by(ISO) %>%
    dplyr::summarize(procapchg.usd=wtd.median(procapchg.usd, weights=weight.norm, normwt=T),
                     rencapchg.direct.usd=wtd.median(rencapchg.direct.usd, weights=weight.norm, normwt=T),
                     rencapchg.feedback.usd=wtd.median(rencapchg.feedback.usd, weights=weight.norm, normwt=T))

## caplevel <- levelprep %>% filter(Year > 2013) %>% group_by(ISO, mc) %>%
##     dplyr::summarize(procapchg.usd=mean(procapchg.usd, na.rm=T), rencapchg.direct.usd=mean(rencapchg.direct.usd, na.rm=T),
##                      rencapchg.feedback.usd=mean(rencapchg.feedback.usd, na.rm=T), weight=mean(weight.norm, na.rm=T)) %>%
##         filter(is.na(weight) | weight > 1e-10) %>% group_by(ISO) %>%
##         dplyr::summarize(procapchg.usd=wtd.median(procapchg.usd, weights=weight, normwt=T),
##                          rencapchg.direct.usd=wtd.median(rencapchg.direct.usd, weights=weight, normwt=T),
##                          rencapchg.feedback.usd=wtd.median(rencapchg.feedback.usd, weights=weight, normwt=T))

tbldf <- sumbyiso %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
    left_join(subset(df.gdp3, Year == 2022), by=c('ISO'='Country Code')) %>%
    left_join(subset(df.pop3, Year == 2022), by=c('ISO'='Country Code')) %>%
    left_join(isotot2, by='ISO', suffix=c('', '.sum')) %>%
    left_join(caplevel, by='ISO') %>%
    dplyr::arrange(ADMIN)

tbldf$gdppc <- tbldf$GDP.2015.est / tbldf$Population.est

range(tbldf$gdppc[tbldf$ECONOMY == "2. Developed region: nonG7"], na.rm=T)
## 9502.411 205405.963
range(tbldf$gdppc[tbldf$ECONOMY == "6. Developing region"], na.rm=T)
## 682.3087 67359.7899
range(tbldf$gdppc[tbldf$ECONOMY == "7. Least developed region"], na.rm=T)
## 262.1848 5871.3249

tbl <- data.frame(country=tbldf$ADMIN,
                  totimpact=format.percent(log2lev(tbldf$totimpact.median)),
                  tradeimpact=format.percent(log2lev(tbldf$tradeimpact.median)),
                  slrimpact=format.percent(log2lev(tbldf$slrimpact.median)),
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
                  procapchg.2015=round(tbldf$procapchg.usd),
                  rencapchg.direct.2015=round(tbldf$rencapchg.direct.usd),
                  rencapchg.feedback.2015=round(tbldf$rencapchg.feedback.usd),
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
tbl2 <- subset(tbl2, INCOME_GRP != "-99")

tbl2 <- rbind(tbl2, data.frame(INCOME_GRP=c("High income (total)", "Low and middle income"),
                               prodchg.2015=round(c(sum(tbl2$prodchg.2015[1:2]), sum(tbl2$prodchg.2015[3:5])), -1),
                               prod.sum=round(c(sum(tbl2$prod.sum[1:2]), sum(tbl2$prod.sum[3:5])), -1),
                               procapchg.2015=round(c(sum(tbl2$procapchg.2015[1:2]), sum(tbl2$procapchg.2015[3:5])), -1),
                               rencapchg.direct.2015=round(c(sum(tbl2$rencapchg.direct.2015[1:2]), sum(tbl2$rencapchg.direct.2015[3:5])), -1),
                               rencapchg.feedback.2015=round(c(sum(tbl2$rencapchg.feedback.2015[1:2]), sum(tbl2$rencapchg.feedback.2015[3:5])), -1),
                               landd.sum=round(c(sum(tbl2$landd.sum[1:2]), sum(tbl2$landd.sum[3:5])), -1)))

names(tbl2) <- c("Income Group", "2023 GDP Change ($billion)", "30-year GDP Change ($billion)", "Produced Capital Change ($billion)", "Renewable Capital Direct Change ($billion)", "Renewable Capital Feedback Change ($billion)", "Total Loss ($billion)")

print(flextable(tbl2))
library(xtable)
print(xtable(tbl2, digits=0), include.rownames=F)

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

names(tbl2grp) <- c("Party", "2023 GDP Change ($billion)", "30-year GDP Change ($billion)", "Produced Capital Change ($billion)", "Renewable Capital Direct Change ($billion)", "Renewable Capital Feedback Change ($billion)", "Total Loss ($billion)")

print(flextable(tbl2grp[order(tbl2grp$Party),]))
print(xtable(tbl2grp[order(tbl2grp$Party),], digits=0), include.rownames=F)

bigtbl <- tbl

incl <- T #(!is.na(bigtbl$prodchg.2015) & abs(bigtbl$prodchg.2015) > 0) | !is.na(bigtbl$rencap.chg.feedback)
bigtbl$prodchg.2015 <- as.character(bigtbl$prodchg.2015)
bigtbl$rencapchg.direct.2015 <- as.character(bigtbl$rencapchg.direct.2015 + bigtbl$rencapchg.feedback.2015) # This is not all rencap
bigtbl$procapchg.2015 <- as.character(bigtbl$procapchg.2015)
bigtbl$total.sum <- as.character(bigtbl$total.sum)

names(bigtbl) <- c("Country", "Direct", "Trade", "SLR", "Capital", "Total", "IQR", "Produced", "Direct", "Feedback", "Total", "IQR", "GDP ($) - DROP", "Prod. Cap. ($)", "Ren. Cap. ($)", "DROP", "Loss ($)", "IQR", "DROP", "DROP")
bigtbl$Country <- as.character(bigtbl$Country)
bigtbl$Country[bigtbl$Country == "Democratic Republic of the Congo"] <- "DR Congo"
bigtbl$Country[bigtbl$Country == "Central African Republic"] <- "Central African Rep."
bigtbl$Country[bigtbl$Country == "United Republic of Tanzania"] <- "United Rep. of Tanzania"
bigtbl$Country[bigtbl$Country == "United States of America"] <- "USA"
library(xtable)
print(xtable(bigtbl[incl, -grep("DROP", names(bigtbl))]), tabular.environment='longtable', floating=F, include.rownames=F)

## Bars by ISO

sumprod <- melt(sumbyiso[, c('ISO', 'totimpact.median', 'tradeimpact.median', 'slrimpact.median', 'solow')], 'ISO')
sumcap <- melt(sumbyiso[, c('ISO', 'rencap.chg', 'procap.chg')], 'ISO')

sumprod$label <- ifelse(sumprod$variable == 'totimpact.median', "Direct Impact",
                 ifelse(sumprod$variable == 'tradeimpact.median', "International Impact",
                 ifelse(sumprod$variable == 'slrimpact.median', "Coastal Impact", "Capital Impact")))
sumprod$label <- factor(sumprod$label, levels=rev(c("Direct Impact", "Coastal Impact", "International Impact", "Capital Impact")))

gp <- ggplot(sumbyiso, aes(ISO)) +
    coord_flip(ylim=c(-0.4, 0.2)) +
    geom_col(data=sumprod, aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=prod25, ymax=prod75)) +
    geom_point(aes(y=total)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw()
ggsave("figures/finalprod-byiso.pdf", width=6.5, height=26)

sumcap$label <- ifelse(sumcap$variable == 'rencap.chg', "Renewable Capital", "Produced Capital")

gp <- ggplot(subset(sumbyiso, !is.na(allcap.chg)), aes(ISO)) +
    coord_flip() + #ylim=c(-.25, 0.6)) +
    geom_col(data=subset(sumcap, !is.na(value)), aes(y=value, fill=label)) +
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

sumbymc4 <- sumbymc2 %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
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

sumbyeconomy <- sumbymc4 %>% filter(is.na(weight.pop) | weight.pop > 1e-9) %>% group_by(ECONOMY) %>% dplyr::summarize(totimpact.median=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeimpact.median=wtd.median(tradeimpact, weights=weight.pop, normwt=T), slrimpact.median=wtd.median(slrimpact, weights=weight.pop, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact.median - tradeimpact.median - slrimpact.median, weights=weight.pop, normwt=T)), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact.median + tradeimpact.median + slrimpact.median, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T))
sumbyincgrp <- sumbymc4 %>% filter(is.na(weight.pop) | weight.pop > 1e-9) %>% group_by(MY_INCOME_GRP) %>% dplyr::summarize(totimpact.median=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeimpact.median=wtd.median(tradeimpact, weights=weight.pop, normwt=T), slrimpact.median=wtd.median(slrimpact, weights=weight.pop, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact.median - tradeimpact.median - slrimpact.median, weights=weight.pop, normwt=T)), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact.median + tradeimpact.median + slrimpact.median, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T))
sumbycontinent <- sumbymc4 %>% filter(is.na(weight.pop) | weight.pop > 1e-9) %>% group_by(CONTINENT) %>% dplyr::summarize(totimpact.median=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeimpact.median=wtd.median(tradeimpact, weights=weight.pop, normwt=T), slrimpact.median=wtd.median(slrimpact, weights=weight.pop, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact.median - tradeimpact.median - slrimpact.median, weights=weight.pop, normwt=T)), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact.median + tradeimpact.median + slrimpact.median, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T))
sumbysubreg <- sumbymc4 %>% filter(is.na(weight.pop) | weight.pop > 1e-9) %>% group_by(SUBREGION) %>% dplyr::summarize(totimpact.median=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeimpact.median=wtd.median(tradeimpact, weights=weight.pop, normwt=T), slrimpact.median=wtd.median(slrimpact, weights=weight.pop, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact.median - tradeimpact.median - slrimpact.median, weights=weight.pop, normwt=T)), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact.median + tradeimpact.median + slrimpact.median, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T))
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
allsums$Group[allsums$Group == 'Seven seas (open ocean)'] <- "Open Ocean"

## NUMBERS FOR REPORT
subset(allsums, Group == "Least developed region")
subset(allsums, Group == "South-Eastern Asia")
log2lev(allsums$total[allsums$Group == "South-Eastern Asia"])
## -0.1139581
log2lev(allsums$prod25[allsums$Group == "South-Eastern Asia"])
## -0.214318
log2lev(allsums$prod75[allsums$Group == "South-Eastern Asia"])
## -0.03971981
subset(allsums, Group == "Western Africa")
log2lev(allsums$total[allsums$Group == "Western Africa"])
log2lev(allsums$prod25[allsums$Group == "Western Africa"])
log2lev(allsums$prod75[allsums$Group == "Western Africa"])
subset(allsums, Group == "Africa")
log2lev(allsums$total[allsums$Group == "Africa"])
## -0.06953691
log2lev(allsums$prod25[allsums$Group == "Africa"])
## -0.130407
log2lev(allsums$prod75[allsums$Group == "Africa"])
## -0.03127444
subset(allsums, Group == "Europe")
log2lev(allsums$total[allsums$Group == "Europe"])
## 0.02022758
log2lev(allsums$prod25[allsums$Group == "Europe"])
## -0.0007376302
log2lev(allsums$prod75[allsums$Group == "Europe"])
## 0.04630093
subset(allsums, Group == "Central Asia")
subset(allsums, Group == "High income: OECD")
log2lev(allsums$total[allsums$Group == "South America"])
## -0.03776794

## Drop sub-regions thar are continents
allsums <- subset(allsums, panel != "Sub-region" | !(Group %in% unique(sumbycontinent$Group)))
allsums$Group <- factor(allsums$Group, levels=rev(unique(allsums$Group)))

allsumprod <- melt(allsums[, c('panel', 'Group', 'totimpact.median', 'tradeimpact.median', 'slrimpact.median', 'solow')], c('panel', 'Group'))
allsumcap <- melt(allsums[, c('panel', 'Group', 'rencap.chg', 'procap.chg')], c('panel', 'Group'))

## Bars by group

allsumprod$label <- ifelse(allsumprod$variable == 'totimpact.median', "Direct",
                    ifelse(allsumprod$variable == 'tradeimpact.median', "International",
                    ifelse(allsumprod$variable == 'slrimpact.median', "Coastal", "Capital")))
allsumprod$label <- factor(allsumprod$label, levels=rev(c("Direct", "Coastal", "International", "Capital")))

gp <- ggplot(subset(allsums, panel == 'Economy'), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(allsumprod, panel == 'Economy'), aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=prod25, ymax=prod75), width=.5) +
    geom_point(aes(y=total)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-byeco.pdf", width=5, height=2.7)

gp <- ggplot(subset(allsums, panel == 'Income Group'), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(allsumprod, panel == 'Income Group'), aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=prod25, ymax=prod75), width=.5) +
    geom_point(aes(y=total)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-byeco2.pdf", width=6.2, height=2.7)

gp <- ggplot(subset(allsums, panel %in% c('Continent', 'Sub-region')), aes(Group)) +
    coord_flip() +
    facet_grid(panel ~ ., scales='free', space='free') +
    geom_col(data=subset(allsumprod, panel %in% c('Continent', 'Sub-region')), aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=prod25, ymax=prod75), width=.5) +
    geom_point(aes(y=total)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-byreg.pdf", width=5, height=6)

gp <- ggplot(subset(allsums, panel == 'Continent' & Group != 'Antarctica'), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(allsumprod, panel == 'Continent' & Group != 'Antarctica'), aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=prod25, ymax=prod75), width=.5) +
    geom_point(aes(y=total)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-bycon.pdf", width=4, height=2.7)

allsumcap$label <- ifelse(allsumcap$variable == 'rencap.chg', "Renewable Capital", "Produced Capital")

gp <- ggplot(subset(allsums, !is.na(allcap.chg) & panel == 'Economy'), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(allsumcap, !is.na(value) & panel == 'Economy'), aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=cap25, ymax=cap75)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Cumulative change in Capital (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalcap-byeco.pdf", width=5, height=2.7)

gp <- ggplot(subset(allsums, !is.na(allcap.chg) & panel %in% c('Continent', 'Sub-region')), aes(Group)) +
    coord_flip() +
    facet_grid(panel ~ ., scales='free', space='free') +
    geom_col(data=subset(allsumcap, !is.na(value) & panel %in% c('Continent', 'Sub-region')), aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=cap25, ymax=cap75)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Cumulative change in Capital (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalcap-byreg.pdf", width=5, height=6)

## Other groupings

sumbygroup <- data.frame()
for (grouping in names(groupings)) {
    isos <- countryname(groupings[[grouping]], 'iso3c')
    sumbysubgroup <- sumbymc4 %>% filter(ISO %in% isos & !is.na(weight.pop) & weight.pop > 1e-9) %>% group_by(Group=grouping) %>% dplyr::summarize(totimpact.median=wtd.median(totimpact, weights=weight.pop, normwt=T), tradeimpact.median=wtd.median(tradeimpact, weights=weight.pop, normwt=T), slrimpact.median=wtd.median(slrimpact, weights=weight.pop, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact - tradeimpact - slrimpact, weights=weight.pop, normwt=T)), total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + tradeimpact + slrimpact, weights=weight.pop, normwt=T), wtd.median(product.chg, weights=weight.pop, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .25, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .25, weights=weight.pop, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .75, weights=weight.pop, normwt=T), wtd.quantile(product.chg, .75, weights=weight.pop, normwt=T)), rencap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(rencap.chg, weights=weight.pop, normwt=T)), procap.chg=ifelse(all(is.na(rencap.chg)), NA, wtd.median(procap.chg, weights=weight.pop, normwt=T)), allcap.chg=wtd.median(allcap.chg, weights=weight.pop, normwt=T), cap25=my.wtd.quantile(allcap.chg, .25, weights=weight.pop, normwt=T), cap75=my.wtd.quantile(allcap.chg, .75, weights=weight.pop, normwt=T))
    sumbygroup <- rbind(sumbygroup, sumbysubgroup)
}
sumbygroup$Group <- factor(sumbygroup$Group, levels=rev(sort(sumbygroup$Group)))

sumbygroupprod <- melt(sumbygroup[, c('Group', 'totimpact.median', 'tradeimpact.median', 'slrimpact.median', 'solow')], 'Group')
sumbygroupcap <- melt(sumbygroup[, c('Group', 'rencap.chg', 'procap.chg')], 'Group')

sumbygroupprod$label <- ifelse(sumbygroupprod$variable == 'totimpact.median', "Direct",
                        ifelse(sumbygroupprod$variable == 'tradeimpact.median', "International",
                        ifelse(sumbygroupprod$variable == 'slrimpact.median', "Coastal", "Capital")))
sumbygroupprod$label <- factor(sumbygroupprod$label, levels=rev(c("Direct", "Coastal", "International", "Capital")))

gp <- ggplot(sumbygroup, aes(Group)) +
    coord_flip() +
    geom_col(data=sumbygroupprod, aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=prod25, ymax=prod75), width=.5) +
    geom_point(aes(y=total)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Change in GDP from Climate Change (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalprod-bygrp.pdf", width=5, height=4)

sumbygroupcap$label <- ifelse(sumbygroupcap$variable == 'rencap.chg', "Renewable Capital", "Produced Capital")

gp <- ggplot(subset(sumbygroup, !is.na(allcap.chg)), aes(Group)) +
    coord_flip() +
    geom_col(data=subset(sumbygroupcap, !is.na(value)), aes(y=value, fill=label)) +
    geom_errorbar(aes(ymin=cap25, ymax=cap75)) +
    scale_y_continuous(labels=scales::percent) + scale_fill_discrete(NULL) +
    ylab("Cumulative change in Capital (%)") + xlab(NULL) + theme_bw() + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 25), "pt"), legend.position="bottom")
ggsave("figures/finalcap-bygrp.pdf", width=5, height=4)

## NUMBERS FOR REPORT
subset(sumbygroup, Group == "G77")
subset(sumbygroup, Group == "CVF")
subset(sumbygroup, Group == "AOSIS")
subset(sumbygroup, Group == "EU")
subset(sumbygroup, Group == "Africa")
subset(sumbygroup, Group == "LDCs")

## Maps

cents <- calcCentroid(shp, rollup=2)
areas <- calcArea(shp, rollup=2)
centroids <- cents %>% left_join(areas, by=c('PID', 'SID')) %>% group_by(PID) %>%
    dplyr::summarize(X=X[which.max(area)], Y=Y[which.max(area)])

source("src/lib/distance.R")
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
    geom_polygon(aes(fill=pmin(.1, pmax(-.25, log2lev(total))), group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=subset(centroids2, show), aes(label=round(log2lev(total) * 100)), size=3, label.padding=unit(0.1, "lines")) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_gradient2("Change in GDP (%):", low = scales::muted("red"), high = scales::muted("blue"), labels=scales::percent, guide=guide_colorbar(barwidth=10, barheight=1)) +
    theme_bw() + theme(legend.position="bottom", legend.key.width=unit(10,"in"),
                       legend.key.height=unit(1,"in"))
ggsave("figures/finalprod-map.pdf", width=10, height=5.5)

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=log2lev(allcap.chg), group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=centroids2, aes(label=round(log2lev(allcap.chg) * 100)), size=2, label.padding=unit(0.1, "lines")) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_gradient2("Change in capital (%):", low = scales::muted("red"), high = scales::muted("blue"), labels=scales::percent, guide=guide_colorbar(barwidth=10, barheight=1)) +
    theme_bw() + theme(legend.position="bottom", legend.key.width=unit(10,"in"),
                       legend.key.height=unit(1,"in"))
ggsave("figures/finalcap-map.pdf", width=10, height=4)
