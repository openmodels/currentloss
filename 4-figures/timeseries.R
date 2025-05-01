## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(PBSmapping)
library(ggplot2)
library(sf)

do.for.subset <- "L+MIC" # "global" or "L+MIC"

persist <- "0.36"
trade.method <- "dd"
source("src/lib/utils2.R")
source("src/lib/synth.R")

load.solowdata()

df.gdp2 <- read.wb("data/capital/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_5871893.xls", 'GDP.2015')
df.gdp2.last <- df.gdp2 %>% group_by(`Country Code`) %>%
    dplyr::summarize(GDP.Year=ifelse(any(!is.na(GDP.2015)), Year[tail(which(!is.na(GDP.2015)), 1)], NA),
                     GDP.2015=ifelse(any(!is.na(GDP.2015)), GDP.2015[tail(which(!is.na(GDP.2015)), 1)], NA))

load(paste0("data/allyr-ww-", persist, "-", trade.method, ".RData"))
allyr.ww[allyr.ww$ISO == 'SDN', which(is.na(allyr.ww[allyr.ww$ISO == 'ABW', ][1, ]))] <- NA # country change affects
for2023 <- subset(allyr.ww, Year == 2022)
stopifnot(all(for2023$ISO == allyr.ww$ISO[allyr.ww$Year == 2023]))
for2023[, 1:8] <- subset(allyr.ww, Year == 2023)[, 1:8]
allyr.ww <- rbind(subset(allyr.ww, Year <= 2022), for2023)
allyr.ww <- allyr.ww %>% group_by(ISO, mc) %>% arrange(Year) %>%
    mutate(across(dimpact:weight.norm, ~ stats::filter(., rep(1 / 10, 10), sides=1)))
# (cumsum(.) - c(rep(0, 10), cumsum(.)[1:(length(.)-10)])) / 10)) <-- fails when contains NA

## TIMESERIES

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

polydata <- st_read("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

allyr2.temp <- allyr.ww %>% group_by(ISO, Year) %>%
    filter(weight.norm > 1e-9 & !is.na(totimpact)) %>%
    dplyr::summarize(totimpact.median=wtd.median(totimpact, weights=weight.norm, normwt=T), tradeloss.median=wtd.median(tradeloss, weights=weight.norm, normwt=T), slrloss.median=wtd.median(slrloss, weights=weight.norm, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.median(product.chg - totimpact.median - -tradeloss.median - -slrloss.median, weights=weight.norm, normwt=T)),
                     total=ifelse(all(is.na(product.chg)), wtd.median(totimpact.median - tradeloss.median - slrloss.median, weights=weight.norm, normwt=T), wtd.median(product.chg, weights=weight.norm, normwt=T)),
                     prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact - tradeloss - slrloss, .25, weights=weight.norm, normwt=T), wtd.quantile(product.chg, .25, weights=weight.norm, normwt=T)),
                     prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact - tradeloss - slrloss, .75, weights=weight.norm, normwt=T), wtd.quantile(product.chg, .75, weights=weight.norm, normwt=T)))

if (do.for.subset == "L+MIC") {
    allyr2 <- allyr2.temp %>%
        left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
        filter(INCOME_GRP %in% c("5. Low income", "4. Lower middle income", "3. Upper middle income"))
    y_label <- "Change in GDP due to climate change (%) for \n Low & Middle income countries"
} else if (do.for.subset == "global") {
    allyr2 <- allyr2.temp
    y_label <- "Change in GDP due to climate change (%)"
}

tohighlight <- c('USA', 'CHN', 'IND', 'BEL', 'RUS', 'BRA', 'AUS', 'MDV', 'NGA', 'THA')
allyr2$label <- ifelse(allyr2$ISO %in% tohighlight, allyr2$ISO, 'XXX')

gp <- ggplot(allyr2, aes(Year, total, group=ISO, colour=label)) +
    coord_cartesian(ylim=c(-.15, .1), xlim=c(1959, 2023)) +
    geom_hline(yintercept=0) +
    geom_line(data=subset(allyr2, label == 'XXX' & total != 0), linewidth=.1) +
    geom_line(data=subset(allyr2, label != 'XXX' & total != 0), linewidth=1) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) +
    scale_y_continuous(y_label, labels=scales::percent) +
    scale_colour_manual(NULL, breaks=c(tohighlight, 'XXX'),
                        values=c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#b15928', '#6a3d9a', '#00000080'),
                        labels=c(countrycode(tohighlight, 'iso3c', 'country.name'), 'Others')) +
    theme_bw()
ggsave(paste0("figures/timeseries_", do.for.subset, "-", persist, "-", trade.method, ".pdf"), width = 8, height = 4)

## Create population and GDP-weighted means

allyr3.pop <- get.weighted.ts(allyr.ww, 'pop', do.for.subset)
allyr3.gdp <- get.weighted.ts(allyr.ww, 'gdp', do.for.subset)

allyr4 <- rbind(cbind(allyr3.pop, weights = "Population"), cbind(allyr3.gdp, weights = "Output"))

y_label <- if (do.for.subset == "L+MIC") {
    "Global weighted change in GDP (%) for \n Low & Middle Income countries"
} else {
    "Global weighted change in GDP (%)"
}

gp <- ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
    geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total")) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=.5) +
    theme_bw() + scale_y_continuous(y_label, labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) +
    theme(legend.position=c(.5, .25))
ggsave(paste0("figures/globaltime-noloess_", do.for.subset, "-", persist, "-", trade.method, ".pdf"), width=6.25, height=3.9)

## Pres fig 1: direct-only
gp <- ggplot(allyr3.pop, aes(Year)) +
  geom_line(aes(y=totimpact, colour="Direct Impact")) +
  geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR"), alpha=0) +
  geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade"), alpha=0) +
  geom_line(aes(y=total, colour="Total Impact"), linewidth=1, alpha=0) +
  geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
  geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
  theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
  scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
  scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))
ggsave(paste0("figures/globaltime-noloess_", do.for.subset, "-step1-", persist, "-", trade.method, ".pdf"), width=6.25, height=3.9)

## Pres fig 2: direct + slr
gp <- ggplot(allyr3.pop, aes(Year)) +
  geom_line(aes(y=totimpact, colour="Direct Impact")) +
  geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
  geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade"), alpha=0) +
  geom_line(aes(y=total, colour="Total Impact"), linewidth=1, alpha=0) +
  geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
  geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
  theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
  scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
  scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))
ggsave(paste0("figures/globaltime-noloess_", do.for.subset, "-step2-", persist, "-", trade.method, ".pdf"), width=6.25, height=3.9)

## Pres fig 2.5: direct + slr + trade
gp <- ggplot(allyr3.pop, aes(Year)) +
  geom_line(aes(y=totimpact, colour="Direct Impact")) +
  geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
  geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade")) +
  geom_line(aes(y=total, colour="Total Impact"), linewidth=1, alpha=0) +
  geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
  geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
  theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
  scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
  scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))
ggsave(paste0("figures/globaltime-noloess_", do.for.subset, "-step3-", persist, "-", trade.method, ".pdf"), width=6.25, height=3.9)

## Pres fig 3: total
gp <- ggplot(allyr3.pop, aes(Year)) +
  geom_line(aes(y=totimpact, colour="Direct Impact")) +
  geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
  geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade")) +
  geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
  geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
  geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
  theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
  scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
  scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))
ggsave(paste0("figures/globaltime-noloess_", do.for.subset, "-step4-", persist, "-", trade.method, ".pdf"), width=6.25, height=3.9)

## Pres fig 4: + output-weighted
gp <- ggplot(allyr3.pop, aes(Year)) +
  geom_line(aes(y=totimpact, colour="Direct Impact")) +
  geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
  geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade")) +
  geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
  geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total")) +
  geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
  theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
  scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
  scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))
ggsave(paste0("figures/globaltime-noloess_", do.for.subset, "-step5-", persist, "-", trade.method, ".pdf"), width=6.25, height=3.9)

## Numbers for pres
tail(allyr3.pop, 1)
##    Year   solow  prod25  prod75   total totimpact slrloss tradeloss
## 1  2023 -0.0285 -0.0631 -0.0338 -0.0501   -0.0331 0.00107   0.00779
## RF:
##    Year   solow prod25  prod75   total totimpact slrloss tradeloss
## 1  2023 -0.0368 -0.107 -0.0252 -0.0751   -0.0447 0.00105    0.0131
tail(allyr3.gdp, 1)
##    Year   solow  prod25  prod75   total totimpact  slrloss tradeloss
## 1  2023 -0.0198 -0.0267 -0.0100 -0.0173  -0.00629 0.000392   0.00659
## RF:
##    Year   solow  prod25  prod75   total totimpact  slrloss tradeloss
## 1  2023 -0.0266 -0.0644 -0.0222 -0.0456   -0.0289 0.000375    0.0101


## Numbers for report
allyr3.pop.mc <- get.weighted.mcts(allyr.ww, 'pop', do.for.subset)
allyr3.pop.mc %>% filter(Year == 2023) %>% group_by(mc) %>%
    dplyr::summarize(total=mean(total), weight2=mean(weight2)) %>%
    dplyr::summarize(mu=log2lev(wtd.median(total, weights=weight2, normwt=T)),
              ci25=log2lev(wtd.quantile(total, .25, weights=weight2, normwt=T)),
              ci75=log2lev(wtd.quantile(total, .75, weights=weight2, normwt=T)))
## Global:
##        mu    ci25    ci75
## 1 -0.0489 -0.0611 -0.0333
## L+MIC:
##        mu    ci25    ci75
## 1 -0.0593 -0.0990 -0.0265
## RF:
## Global:
##        mu   ci25    ci75
## 1 -0.0724 -0.101 -0.0249
## L+MIC:
##        mu   ci25    ci75
## 1 -0.0766 -0.114 -0.0255

allyr3.gdp.mc <- get.weighted.mcts(allyr.ww, 'gdp', do.for.subset)
allyr3.gdp.mc %>% filter(Year == 2023) %>% group_by(mc) %>%
    dplyr::summarize(total=mean(total), weight2=mean(weight2)) %>%
    dplyr::summarize(mu=log2lev(wtd.median(total, weights=weight2, normwt=T)),
              ci25=log2lev(wtd.quantile(total, .25, weights=weight2, normwt=T)),
              ci75=log2lev(wtd.quantile(total, .75, weights=weight2, normwt=T)))
## Global:
##        mu    ci25    ci75
## 1 -0.0171 -0.0263 -0.00995
## L+MIC:
##        mu    ci25    ci75
## 1 -0.0463 -0.0737 -0.0185
## RF:
## Global:
##        mu    ci25    ci75
## 1 -0.0445 -0.0624 -0.0220
## L+MIC:
##        mu    ci25    ci75
## 1 -0.0571 -0.0848 -0.0208

## Determine ranges

allyr2 <- allyr.ww %>% group_by(ISO, mc, Year) %>%
  mutate(total=ifelse(all(is.na(product.chg)), totimpact - tradeloss - slrloss, product.chg)) %>%
  group_by(ISO, mc) %>% mutate(smooth=stats::filter(total, rep(1/19, 19), sides=1))

allyr3 <- allyr2 %>% filter(Year >= 2000) %>% group_by(ISO, mc) %>% dplyr::summarize(stddev=sd(smooth - total)) %>% group_by(ISO) %>% dplyr::summarize(sd.mu=median(stddev, na.rm=T), sd.ci25=quantile(stddev, .25, na.rm=T), sd.ci75=quantile(stddev, .75, na.rm=T))

## Total losses per year for global or L+MIC.
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

pdf3 <- pdf2 %>% group_by(Year) %>% filter(!is.na(weight2)) %>%
    dplyr::summarize(totimpact.usd=ifelse(all(is.na(totimpact.usd)), NA, wtd.median(totimpact.usd, weights=weight2, normwt=T)),
                     tradeimpact.usd=ifelse(all(is.na(tradeimpact.usd)), NA, wtd.median(tradeimpact.usd, weights=weight2, normwt=T)),
                     slrimpact.usd=ifelse(all(is.na(slrimpact.usd)), NA, wtd.median(slrimpact.usd, weights=weight2, normwt=T)),
                     solow.usd=ifelse(all(is.na(solow.usd)), NA, wtd.median(solow.usd, weights=weight2, normwt=T)),
                     allcap.usd=ifelse(all(is.na(allcap.usd)), NA, wtd.median(allcap.usd, weights=weight2, normwt=T)),
                     totalandcap.usd.25=ifelse(all(is.na(totalandcap.usd)), NA, wtd.quantile(totalandcap.usd, .25, weights=weight2, normwt=T)),
                     totalandcap.usd.75=ifelse(all(is.na(totalandcap.usd)), NA, wtd.quantile(totalandcap.usd, .75, weights=weight2, normwt=T)),
                     totalandcap.usd=ifelse(all(is.na(totalandcap.usd)), NA, wtd.median(totalandcap.usd, weights=weight2, normwt=T)))

pdf4 <- melt(pdf3[, 1:6], 'Year')
pdf4$variable <- factor(pdf4$variable, levels=c('totimpact.usd', 'slrimpact.usd', 'tradeimpact.usd', 'solow.usd', 'allcap.usd'))

y_axis_label <- if (do.for.subset == "L+MIC") {
    "Total global loss in lower-income countries ($billion)"
} else {
    "Total global loss ($billion)"
}

## Number for report
sum(subset(pdf4, Year == 2023)$value)
## -1864.735
## RF: -4067.772

gp <- ggplot(subset(pdf4, Year >= 1960), aes(Year)) +
    #coord_cartesian(ylim=c(-10000, 0)) +
    geom_col(aes(y=value, fill=variable)) +
    geom_errorbar(data=subset(pdf3, Year >= 1960), aes(ymin=totalandcap.usd.25, ymax=totalandcap.usd.75), alpha=.5) +
    theme_bw() + scale_y_continuous(y_axis_label) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1960, 2023.7)) +
    scale_fill_manual(NULL, breaks=rev(c('totimpact.usd', 'slrimpact.usd', 'tradeimpact.usd', 'solow.usd', 'allcap.usd')), labels=rev(c("Direct Impact", "Coastal Impact", "International Impact", "Capital Impact", "Capital Loss")), values=rev(c("#7570b3", "#1b9e77", "#66a61e", "#d95f02", "#e7298a"))) + theme(legend.position=c(.5, .25))
ggsave(paste0("figures/totalbyyear_", do.for.subset, "-", persist, "-", trade.method, ".pdf"), width=5, height=4)

## Construct table with lots of breakdowns by year
set1 <- levelprep %>% filter(Year == 2023) %>% group_by(ISO, mc) %>%
    dplyr::summarize(dimpact.usd=mean((log2lev(dimpact) / (1 + log2lev(dimpact))) * GDP.2015.est / 1e9, na.rm=T),
                     persist.usd=mean(totimpact.usd, na.rm=T) - dimpact.usd,
                     weight2=mean(weight.norm), CONTINENT=CONTINENT[1]) %>%
        group_by(CONTINENT, mc) %>% dplyr::summarize(dimpact.usd=sum(dimpact.usd, na.rm=T),
                                          persist.usd=sum(persist.usd, na.rm=T),
                                          weight2=sum(weight2)) %>%
        group_by(CONTINENT) %>%
        dplyr::reframe(variable=c('Immediate Impact', 'Remaining Persisting'),
                       mu=c(wtd.median(dimpact.usd, weights=weight2, normwt=T),
                            wtd.median(persist.usd, weights=weight2, normwt=T)),
                       ci.25=c(wtd.quantile(dimpact.usd, .25, weights=weight2, normwt=T),
                               wtd.quantile(persist.usd, .25, weights=weight2, normwt=T)),
                       ci.75=c(wtd.quantile(dimpact.usd, .75, weights=weight2, normwt=T),
                               wtd.quantile(persist.usd, .75, weights=weight2, normwt=T)))

df.gdp3 <- load.gdp3()

set2 <- data.frame()
for (slr.config in c('optimalfixed', 'noAdaptation-inundation', 'noAdaptation-stormCapital')) {
    slr <- read.csv(paste0("data/slrbyadm0-final-", slr.config, ".csv"))
    slr2 <- slr %>% left_join(df.gdp3, by=c('ISO'='Country Code', 'year'='Year')) %>%
        group_by(ISO, year) %>% reframe(mc=1:30, slrloss=rnorm(30, mu / GDP.2019.est, ((q83 - q17) / diff(qnorm(c(.17, .83)))) / GDP.2019.est), GDP.2015.est=GDP.2015.est)
    slr3 <- slr2 %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
        filter(year == 2023) %>% group_by(ISO, mc) %>%
        dplyr::summarize(slrimpact.usd=mean(-(log2lev(slrloss) / (1 + log2lev(slrloss))) * GDP.2015.est / 1e9), CONTINENT=CONTINENT[1]) %>%
        group_by(CONTINENT, mc) %>% dplyr::summarize(slrimpact.usd=sum(slrimpact.usd, na.rm=T)) %>%
        group_by(CONTINENT) %>%
        dplyr::summarize(mu=median(slrimpact.usd), ci.25=quantile(slrimpact.usd, .25),
                             ci.75=quantile(slrimpact.usd, .75))
    set2 <- rbind(set2, cbind(variable=slr.config, slr3))
}
set2$variable[set2$variable == 'optimalfixed'] <- "Forward Planning"
set2$variable[set2$variable == 'noAdaptation-inundation'] <- "Inundation Loss"
set2$variable[set2$variable == 'noAdaptation-stormCapital'] <- "Storm Damage"
set2$mu[set2$mu == "Inundation Loss"] <- set2$mu[set2$mu == "Inundation Loss"] - set2$mu[set2$mu == "Forward Planning"]
set2$ci.25[set2$mu == "Inundation Loss"] <- set2$ci.25[set2$mu == "Inundation Loss"] - set2$ci.25[set2$mu == "Forward Planning"]
set2$ci.75[set2$mu == "Inundation Loss"] <- set2$ci.75[set2$mu == "Inundation Loss"] - set2$ci.75[set2$mu == "Forward Planning"]

tradeloss.rich <- load.tradeloss(trade.method, paste0(persist, '-1-2'))

set3 <- levelprep %>% left_join(tradeloss.rich, by=c('ISO', 'mc', 'Year'='year'), suffix=c('', '.rich')) %>%
    filter(Year == 2023) %>% group_by(ISO, mc) %>%
    dplyr::summarize(tradeimpact.rich.usd=mean(-(log2lev(tradeloss.rich) / (1 + log2lev(tradeloss.rich))) * GDP.2015.est / 1e9, na.rm=T),
                     tradeimpact.poor.usd=mean(tradeimpact.usd, na.rm=T) - tradeimpact.rich.usd,
                     weight2=mean(weight.norm), CONTINENT=CONTINENT[1]) %>%
    group_by(CONTINENT, mc) %>% dplyr::summarize(tradeimpact.rich.usd=sum(tradeimpact.rich.usd, na.rm=T),
                                      tradeimpact.poor.usd=sum(tradeimpact.poor.usd, na.rm=T),
                                      weight2=sum(weight2)) %>%
    group_by(CONTINENT) %>%
    dplyr::reframe(variable=c('L+MIC Exports', 'Rich Country Exports'),
                   mu=c(wtd.median(tradeimpact.rich.usd, weights=weight2, normwt=T),
                        wtd.median(tradeimpact.poor.usd, weights=weight2, normwt=T)),
                   ci.25=c(wtd.quantile(tradeimpact.rich.usd, .25, weights=weight2, normwt=T),
                           wtd.quantile(tradeimpact.poor.usd, .25, weights=weight2, normwt=T)),
                   ci.75=c(wtd.quantile(tradeimpact.rich.usd, .75, weights=weight2, normwt=T),
                           wtd.quantile(tradeimpact.poor.usd, .75, weights=weight2, normwt=T)))

set4 <- levelprep %>%
    mutate(cumul.rencap.ccpc.usd=pmax((rencap.ccpc - rencap.nocc) / 1e9, -`Renewable Capital Est`) * 100 / 83.6,
           cumul.rencap.rest.usd=pmax((rencap.true - rencap.nocc) / 1e9, -`Renewable Capital Est`) * 100 / 83.6 - cumul.rencap.ccpc.usd,
           cumul.procap.usd=pmax((allcap.true - allcap.nocc) / 1e9 - cumul.rencap.rest.usd - cumul.rencap.ccpc.usd, -`Produced Capital Est`) * 100 / 83.6) %>%
    group_by(ISO, mc) %>%
    mutate(rencap.ccpc.usd=cumul.rencap.ccpc.usd - lag(cumul.rencap.ccpc.usd),
           rencap.rest.usd=cumul.rencap.rest.usd - lag(cumul.rencap.rest.usd),
           procap.usd=cumul.procap.usd - lag(cumul.procap.usd)) %>%
    filter(Year == 2022) %>% group_by(ISO, mc) %>%
    dplyr::summarize(allcap.usd=mean(allcap.usd, na.rm=T),
                     rencap.ccpc.usd=mean(rencap.ccpc.usd, na.rm=T),
                     rencap.rest.usd=mean(rencap.rest.usd, na.rm=T),
                     procap.usd=mean(procap.usd, na.rm=T),
                     weight2=mean(weight.norm), CONTINENT=CONTINENT[1]) %>%
    group_by(CONTINENT, mc) %>% dplyr::summarize(allcap.usd=sum(allcap.usd, na.rm=T),
                                                 rencap.ccpc.usd=sum(rencap.ccpc.usd, na.rm=T),
                                                 rencap.rest.usd=sum(rencap.rest.usd, na.rm=T),
                                                 procap.usd=sum(procap.usd, na.rm=T),
                                                 weight2=sum(weight2)) %>%
    group_by(CONTINENT) %>%
    dplyr::reframe(variable=c('Capital Growth Effect', 'Direct Renewable Capital', 'Feedback Renewable Capital', 'Produced Capital'),
                   mu=c(wtd.median(allcap.usd, weights=weight2, normwt=T),
                        wtd.median(rencap.ccpc.usd, weights=weight2, normwt=T),
                        wtd.median(rencap.rest.usd, weights=weight2, normwt=T),
                        wtd.median(procap.usd, weights=weight2, normwt=T)),
                   ci.25=c(wtd.quantile(allcap.usd, .25, weights=weight2, normwt=T),
                           wtd.quantile(rencap.ccpc.usd, .25, weights=weight2, normwt=T),
                           wtd.quantile(rencap.rest.usd, .25, weights=weight2, normwt=T),
                           wtd.quantile(procap.usd, .25, weights=weight2, normwt=T)),
                   ci.75=c(wtd.quantile(allcap.usd, .75, weights=weight2, normwt=T),
                           wtd.quantile(rencap.ccpc.usd, .75, weights=weight2, normwt=T),
                           wtd.quantile(rencap.rest.usd, .75, weights=weight2, normwt=T),
                           wtd.quantile(procap.usd, .75, weights=weight2, normwt=T)))

allsets <- rbind(cbind(set1, panel="Temperature Impact"),
                 cbind(set2, panel="Coastal Impact"),
                 cbind(set3, panel="International Impact"),
                 cbind(set4, panel="Capital Impact"))
allsets$panel <- factor(allsets$panel, levels=rev(c("Temperature Impact", "Coastal Impact", "International Impact", "Capital Impact")))
allsets.sum <- allsets %>% group_by(variable) %>% dplyr::summarize(panel=panel[1], mu=sum(mu),
                                                                   ci.25=sum(ci.25),
                                                                   ci.75=sum(ci.75))
allsets$CONTINENT <- as.character(allsets$CONTINENT)
allsets$CONTINENT[allsets$CONTINENT == "Seven seas (open ocean)"] <- "Open Ocean"

gp <- ggplot(subset(allsets, !is.na(CONTINENT) & CONTINENT != "Antarctica"), aes(variable, mu)) +
    coord_flip() + facet_wrap(~ panel, ncol=1, scales='free') +
    geom_col(aes(fill=CONTINENT)) +
    geom_errorbar(data=allsets.sum, aes(ymin=ci.25, ymax=ci.75)) + geom_point(data=allsets.sum) +
    ylab("Total global loss in lower-income countries ($billion)") +
    xlab(NULL) + scale_fill_discrete(NULL) +
    theme_bw() + theme(legend.position="bottom")
ggsave(paste0("figures/totalbyothers_", do.for.subset, "-", persist, "-", trade.method, ".pdf"), width=6, height=6)

