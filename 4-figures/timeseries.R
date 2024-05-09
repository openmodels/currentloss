## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(PBSmapping)

persist <- "0.08"
source("src/lib/utils2.R")

load.solowdata()
solowsum <- load.solowsum(persist)

df.gdp2.last <- df.gdp2 %>% group_by(`Country Code`) %>%
    dplyr::summarize(GDP.Year=ifelse(any(!is.na(GDP.2015)), Year[tail(which(!is.na(GDP.2015)), 1)], NA),
                     GDP.2015=ifelse(any(!is.na(GDP.2015)), GDP.2015[tail(which(!is.na(GDP.2015)), 1)], NA))

allyr <- results2 %>%
    left_join(slr2, by=c('mc', 'Year'='year', 'ISO')) %>%
    left_join(tradeloss, by=c('mc', 'Year'='year', 'ISO')) %>%
    left_join(tradeloss.global, by=c('Year'='year'), suffix=c('', '.global'))
allyr$tradeloss[is.na(allyr$tradeloss)] <- allyr$tradeloss.global[is.na(allyr$tradeloss)]
allyr$tradeloss[is.na(allyr$tradeloss)] <- 0
allyr$slrloss[is.na(allyr$slrloss)] <- 0

allyr$product.true <- NA
allyr$product.nocc <- NA
allyr$allcap.true <- NA
allyr$allcap.nocc <- NA
allyr$rencap.true <- NA
allyr$rencap.nocc <- NA
allyr$rencap.ccpc <- NA

for (mcii in 1:30) {
    load.solowdata.mc(mcii)

    for (iso in unique(allyr$ISO)) {
        if (!any(solowsum$ISO == iso & solowsum$mc == mcii))
            next
        print(c(mcii, iso))

        stan.data <- make.stan.data(iso)

        load(paste0("data/solow-", persist, "/v4-", iso, "-", mcii, ".RData"))

        solowout <- model.solow(la, stan.data, F, rencaptrue=la$rencap_model)

        denom <- df2$denom[df2$ISO == iso][1]
        product.true <- colMeans(la$product) * denom
        product.nocc <- colMeans(solowout$product) * denom

        allcap.true <- (colMeans(la$rencap_model) + colMeans(la$procap_model)) * denom
        allcap.nocc <- (colMeans(solowout$rencap_model) + colMeans(solowout$procap_model)) * denom
        rencap.true <- colMeans(la$rencap_model) * denom
        rencap.nocc <- colMeans(solowout$rencap_model) * denom

        allyr$product.true[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- product.true
        allyr$product.nocc[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- product.nocc
        allyr$allcap.true[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- allcap.true[2:length(allcap.true)]
        allyr$allcap.nocc[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- allcap.nocc[2:length(allcap.true)]
        allyr$rencap.true[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- rencap.true[2:length(rencap.true)]
        allyr$rencap.nocc[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- rencap.nocc[2:length(rencap.true)]

        solowout2 <- model.solow(la, stan.data, "prodonly")
        rencap.ccpc <- colMeans(solowout2$rencap_model) * denom
        allyr$rencap.ccpc[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- rencap.ccpc[2:length(rencap.true)]
    }
}

allyr$product.chg <- 1 - allyr$product.nocc / allyr$product.true
allyr$rencap.chg.ccpc <- 1 - allyr$rencap.ccpc / allyr$rencap.true

solowsum2 <- solowsum %>% group_by(ISO) %>% mutate(weight.ess=ess / sum(ess), lp.adj=ifelse(lp > 1, lp, exp(lp - 1)), weight.lp=lp.adj / sum(lp.adj), weight=weight.ess * weight.lp)
solowsum3 <- solowsum2[!duplicated(paste(solowsum2$ISO, solowsum2$mc)),]

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

allyr.ww <- allyr %>% left_join(solowsum3, by=c('ISO', 'mc'), suffix=c('', '.solowsum')) %>%
    filter(is.na(weight) | weight > 1e-9) %>% group_by(ISO, Year) %>%
    mutate(weight.norm=case_when(
               all(is.na(weight)) ~ 1/length(weight),
               is.na(weight) ~ 0,
               TRUE ~ weight / sum(weight, na.rm=T)))

save(allyr.ww, file="data/allyr-ww.RData")
## load("data/allyr-ww.RData")

allyr2 <- allyr.ww %>% group_by(ISO, Year) %>%
    filter(weight.norm > 1e-9) %>%
    dplyr::reframe(solow=ifelse(rep(all(is.na(product.chg)), length(product.chg)), NA, wtd.median(product.chg - totimpact - -tradeloss - -slrloss, weights=weight.norm, normwt=T)), total=ifelse(rep(all(is.na(product.chg)), length(product.chg)), wtd.median(totimpact - tradeloss - slrloss, weights=weight.norm, normwt=T), wtd.median(product.chg, weights=weight.norm, normwt=T)), prod25=ifelse(rep(all(is.na(product.chg)), length(product.chg)), wtd.quantile(totimpact - tradeloss - slrloss, .25, weights=weight.norm, normwt=T), wtd.quantile(product.chg, .25, weights=weight.norm, normwt=T)), prod75=ifelse(rep(all(is.na(product.chg)), length(product.chg)), wtd.quantile(totimpact - tradeloss - slrloss, .75, weights=weight.norm, normwt=T), wtd.quantile(product.chg, .75, weights=weight.norm, normwt=T)), totimpact=wtd.median(totimpact, weights=weight.norm, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight.norm, normwt=T), slrloss=wtd.median(slrloss, weights=weight.norm, normwt=T))

tohighlight <- c('USA', 'CHN', 'IND', 'BEL', 'RUS', 'BRA', 'AUS', 'MDV', 'NGA', 'THA')
allyr2$label <- ifelse(allyr2$ISO %in% tohighlight, allyr2$ISO, 'XXX')

ggplot(allyr2, aes(Year, total, group=ISO, colour=label)) +
    coord_cartesian(ylim=c(-.55, .1)) +
    geom_hline(yintercept=0) +
    geom_line(data=subset(allyr2, label == 'XXX' & total != 0), linewidth=.1) +
    geom_line(data=subset(allyr2, label != 'XXX' & total != 0), linewidth=1) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) + scale_y_continuous("Change in GDP due to climate change (%)", labels=scales::percent) +
    scale_colour_manual(NULL, breaks=c(tohighlight, 'XXX'), values=c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#b15928', '#6a3d9a', '#00000080'), labels=c(countrycode(tohighlight, 'iso3c', 'country.name'), 'Others')) +
    theme_bw()
ggsave("figures/timeseries.pdf", width=8, height=4)

## Create population and GDP-weighted means

df.pop2 <- read.wb("data/capital/API_SP.POP.TOTL_DS2_en_excel_v2_5871620.xls", 'POP')
df.pop3 <- df.pop2 %>% group_by(`Country Code`) %>%
    dplyr::summarize(POP.Year=ifelse(any(!is.na(POP)), Year[tail(which(!is.na(POP)), 1)], NA),
                     POP=ifelse(any(!is.na(POP)), POP[tail(which(!is.na(POP)), 1)], NA))

allyr2 <- allyr.ww %>% left_join(df.gdp2.last, by=c('ISO'='Country Code')) %>% left_join(df.pop3, by=c('ISO'='Country Code'))

allyr3.pop <- allyr2 %>% group_by(mc, Year) %>%
    dplyr::summarize(totimpact=wtd.mean(totimpact, weights=POP, normwt=T), slrloss=wtd.mean(slrloss, weights=POP, normwt=T), tradeloss=wtd.mean(tradeloss, weights=POP, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.mean(product.chg - totimpact - -tradeloss - -slrloss, weights=POP, normwt=T)), total=ifelse(all(is.na(product.chg)), wtd.mean(totimpact - tradeloss - slrloss, weights=POP, normwt=T), wtd.mean(product.chg, weights=POP, normwt=T)), weight2=wtd.mean(weight.norm, weights=POP)) %>%
    group_by(Year) %>%
    dplyr::summarize(solow=ifelse(all(is.na(total)), NA, wtd.median(total - totimpact - -tradeloss - -slrloss, weights=weight2, normwt=T)), prod25=ifelse(all(is.na(total)), wtd.quantile(totimpact - tradeloss - slrloss, .25, weights=weight2, normwt=T), wtd.quantile(total, .25, weights=weight2, normwt=T)), prod75=ifelse(all(is.na(total)), wtd.quantile(totimpact - tradeloss - slrloss, .75, weights=weight2, normwt=T), wtd.quantile(total, .75, weights=weight2, normwt=T)), total=ifelse(all(is.na(total)), wtd.median(totimpact - tradeloss - slrloss, weights=weight2, normwt=T), wtd.median(total, weights=weight2, normwt=T)), totimpact=wtd.median(totimpact, weights=weight2, normwt=T), slrloss=wtd.median(slrloss, weights=weight2, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight2, normwt=T))
allyr3.pop$totalloess <- tail(predict(loess(total ~ Year, allyr3.pop, span=.25)), nrow(allyr3.pop))
allyr3.pop$prod25loess <- tail(predict(loess(prod25 ~ Year, allyr3.pop, span=.25)), nrow(allyr3.pop))
allyr3.pop$prod75loess <- tail(predict(loess(prod75 ~ Year, allyr3.pop, span=.25)), nrow(allyr3.pop))

allyr3.gdp <- allyr2 %>% group_by(mc, Year) %>%
    dplyr::summarize(totimpact=wtd.mean(totimpact, weights=GDP.2015, normwt=T), slrloss=wtd.mean(slrloss, weights=GDP.2015, normwt=T), tradeloss=wtd.mean(tradeloss, weights=GDP.2015, normwt=T), solow=ifelse(all(is.na(product.chg)), NA, wtd.mean(product.chg - totimpact - -tradeloss - -slrloss, weights=GDP.2015, normwt=T)), total=ifelse(all(is.na(product.chg)), wtd.mean(totimpact - tradeloss - slrloss, weights=GDP.2015, normwt=T), wtd.mean(product.chg, weights=GDP.2015, normwt=T)), weight2=wtd.mean(weight.norm, weights=GDP.2015)) %>%
    group_by(Year) %>%
    dplyr::summarize(solow=ifelse(all(is.na(total)), NA, wtd.median(total - totimpact - -tradeloss - -slrloss, weights=weight2, normwt=T)), prod25=ifelse(all(is.na(total)), wtd.quantile(totimpact - tradeloss - slrloss, .25, weights=weight2, normwt=T), wtd.quantile(total, .25, weights=weight2, normwt=T)), prod75=ifelse(all(is.na(total)), wtd.quantile(totimpact - tradeloss - slrloss, .75, weights=weight2, normwt=T), wtd.quantile(total, .75, weights=weight2, normwt=T)), total=ifelse(all(is.na(total)), wtd.median(totimpact - tradeloss - slrloss, weights=weight2, normwt=T), wtd.median(total, weights=weight2, normwt=T)), totimpact=wtd.median(totimpact, weights=weight2, normwt=T), slrloss=wtd.median(slrloss, weights=weight2, normwt=T), tradeloss=wtd.median(tradeloss, weights=weight2, normwt=T))
allyr3.gdp$totalloess <- tail(predict(loess(total ~ Year, allyr3.gdp, span=.25)), nrow(allyr3.gdp))
allyr3.gdp$prod25loess <- tail(predict(loess(prod25 ~ Year, allyr3.gdp, span=.25)), nrow(allyr3.gdp))
allyr3.gdp$prod75loess <- tail(predict(loess(prod75 ~ Year, allyr3.gdp, span=.25)), nrow(allyr3.gdp))

allyr4 <- rbind(cbind(allyr3.pop, weights="Population"), cbind(allyr3.gdp, weights="Output"))

## ggplot(allyr4, aes(Year, totalloess)) +
##     geom_line(aes(colour=weights)) + geom_ribbon(aes(ymin=prod25loess, ymax=prod75loess, group=weights), alpha=.5) +
##     theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
##     scale_x_continuous(NULL, expand=c(0, 0)) + scale_colour_discrete("Weighting:")
## ggsave("figures/globaltime.pdf", width=6.5, height=4)

ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
    geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total")) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=.5) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))
ggsave("figures/globaltime-noloess.pdf", width=6.5, height=4)

## Pres fig 1: direct-only
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR"), alpha=0) +
    geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade"), alpha=0) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1, alpha=0) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))

## Pres fig 2: direct + slr
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
    geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade"), alpha=0) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1, alpha=0) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))

## Pres fig 2.5: direct + slr + trade
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
    geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1, alpha=0) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))

## Pres fig 3: total
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
    geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))

## Pres fig 4: + output-weighted
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact - slrloss, colour="Direct + SLR")) +
    geom_line(aes(y=totimpact - tradeloss - slrloss, colour="Direct + SLR + Trade")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total")) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + SLR", "Direct + SLR + Trade", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#7570b3", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))


## Numbers for report
subset(allyr3.pop, Year == 2023) # -0.0633
subset(allyr3.gdp, Year == 2023) # -0.0176
sum((subset(allyr2, Year == 2023) %>% group_by(ISO) %>% dplyr::summarize(GDP.2015=GDP.2015[1]))$GDP.2015, na.rm=T)

## Determine ranges

allyr2 <- allyr.ww %>% group_by(ISO, mc, Year) %>%
    mutate(total=ifelse(all(is.na(product.chg)), totimpact - tradeloss - slrloss, product.chg)) %>%
    group_by(ISO, mc) %>% mutate(smooth=stats::filter(total, rep(1/19, 19), sides=1))

allyr3 <- allyr2 %>% filter(Year >= 2000) %>% group_by(ISO, mc) %>% dplyr::summarize(stddev=sd(smooth - total)) %>% group_by(ISO) %>% dplyr::summarize(sd.mu=median(stddev, na.rm=T), sd.ci25=quantile(stddev, .25, na.rm=T), sd.ci75=quantile(stddev, .75, na.rm=T))

## Total losses per year for L+MIC

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

df.pro2b <- read.iw("data/capital/tabula-C-produced.csv", 'Produced Capital') %>%
    filter(!is.na(ISO)) %>% group_by(ISO) %>%
    reframe(`Produced Capital Est`=approx(Year, `Produced Capital`, 1960:2023, rule=2)$y, Year=1960:2023)
df.ren2b <- read.iw("data/capital/tabula-A2-renewable.csv", 'Renewable Capital') %>%
    filter(!is.na(ISO)) %>% group_by(ISO) %>%
    reframe(`Renewable Capital Est`=approx(Year, `Renewable Capital`, 1960:2023, rule=2)$y, Year=1960:2023)

log2lev <- function(xx) {
    exp(xx) - 1
}

df.gdp3 <- load.gdp3()

pdf <- allyr.ww %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
    left_join(df.gdp3, by=c('Year', 'ISO'='Country Code')) %>%
    left_join(df.pro2b, by=c('Year', 'ISO')) %>% left_join(df.ren2b, by=c('Year', 'ISO')) %>%
    filter(INCOME_GRP %in% c("5. Low income", "4. Lower middle income", "3. Upper middle income")) %>%
    mutate(totimpact.usd=(log2lev(totimpact) / (1 + log2lev(totimpact))) * GDP.2015.est / 1e9,
           tradeloss.usd=(log2lev(tradeloss) / (1 + log2lev(tradeloss))) * GDP.2015.est / 1e9,
           slrloss.usd=-(log2lev(slrloss) / (1 + log2lev(slrloss))) * GDP.2015.est / 1e9,
           solow=product.chg - totimpact - -tradeloss - -slrloss,
           solow.usd=(log2lev(solow) / (1 + log2lev(solow))) * GDP.2015.est / 1e9) %>%
           #cumul.allcap.usd=pmax(allcap.true - allcap.nocc, -(`Produced Capital Est` + `Renewable Capital Est`)) * 100 / 83.6,
           #allcap.usd=c(NA, diff(cumul.allcap.usd))) %>%
    group_by(Year, mc) %>%
    dplyr::summarize(totimpact.usd=sum(totimpact.usd, na.rm=T),
                     tradeloss.usd=sum(tradeloss.usd, na.rm=T),
                     slrloss.usd=sum(slrloss.usd, na.rm=T),
                     solow.usd=sum(solow.usd, na.rm=T),
                     #allcap.usd=sum(allcap.usd, na.rm=T),
                     total.usd=totimpact.usd - tradeloss.usd - slrloss.usd + solow.usd, # + allcap.usd,
                     weight2=sum(weight.norm)) %>%
    group_by(Year) %>%
    dplyr::summarize(totimpact.usd=wtd.median(totimpact.usd, weights=weight2, normwt=T),
                     tradeloss.usd=wtd.median(tradeloss.usd, weights=weight2, normwt=T),
                     slrloss.usd=wtd.median(slrloss.usd, weights=weight2, normwt=T),
                     solow.usd=wtd.median(solow.usd, weights=weight2, normwt=T),
                     #allcap.usd=wtd.median(allcap.usd, weights=weight2, normwt=T),
                     total.usd.25=wtd.quantile(total.usd, .25, weights=weight2, normwt=T),
                     total.usd.75=wtd.quantile(total.usd, .75, weights=weight2, normwt=T),
                     total.usd=wtd.median(total.usd, weights=weight2, normwt=T))

pdf2 <- melt(pdf[, 1:5], 'Year')

ggplot(pdf2, aes(Year)) +
    coord_cartesian(ylim=c(-10000, 0)) +
    geom_col(aes(y=value, fill=variable)) +
    geom_errorbar(data=pdf, aes(ymin=total.usd.25, ymax=total.usd.75)) +
    theme_bw() + scale_y_continuous("Total global loss in lower-income countries ($billion)") +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2023.7)) +
    scale_fill_manual(NULL, breaks=rev(c('totimpact.usd', 'tradeloss.usd', 'solow.usd', 'slrloss.usd')), labels=rev(c("Direct Impact", "International Impact", "Capital Impact", "Coastal Impact")), values=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")) + theme(legend.position=c(.5, .25))
ggsave("figures/totalbyyear.pdf", width=8, height=4)
