## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(PBSmapping)

persist <- "0.08"
source("src/lib/utils2.R")

load.solowdata()

allyr <- results2 %>%
    left_join(tradeloss, by=c('mc', 'Year'='year', 'ISO')) %>%
    left_join(tradeloss.global, by=c('Year'='year'), suffix=c('', '.global'))
allyr$fracloss[is.na(allyr$fracloss)] <- allyr$fracloss.global[is.na(allyr$fracloss)]

solowdone <- read.csv("data/solow-v4.csv")

allyr$product.true <- NA
allyr$product.nocc <- NA
allyr$allcap.true <- NA
allyr$allcap.nocc <- NA

for (mcii in 1:30) {
    load.solowdata.mc(mcii)

    for (iso in unique(allyr$ISO)) {
        if (!any(solowdone$ISO == iso & solowdone$mc == mcii))
            next
        print(c(mcii, iso))

        stan.data <- make.stan.data(iso)

        load(paste0("data/solow/v4-", iso, "-", mcii, ".RData"))

        solowout <- model.solow(la, stan.data, F, rencaptrue=la$rencap_model)

        product.true <- colMeans(la$product)
        product.nocc <- colMeans(solowout$product)

        allcap.true <- colMeans(la$rencap_model) + colMeans(la$procap_model)
        allcap.nocc <- colMeans(solowout$rencap_model) + colMeans(solowout$procap_model)

        allyr$product.true[allyr$mc == mcii & allyr$Year %in% 1961:2022 & allyr$ISO == iso] <- product.true
        allyr$product.nocc[allyr$mc == mcii & allyr$Year %in% 1961:2022 & allyr$ISO == iso] <- product.nocc
        allyr$allcap.true[allyr$mc == mcii & allyr$Year %in% 1961:2022 & allyr$ISO == iso] <- allcap.true[2:length(allcap.true)]
        allyr$allcap.nocc[allyr$mc == mcii & allyr$Year %in% 1961:2022 & allyr$ISO == iso] <- allcap.nocc[2:length(allcap.true)]
    }
}

allyr$product.chg <- 1 - allyr$product.nocc / allyr$product.true
allyr$partprod.chg <- (allyr$product.chg - (allyr$totimpact - allyr$fracloss)) * runif(nrow(allyr)) + allyr$totimpact - allyr$fracloss
allyr$itlimpact <- -allyr$fracloss

solowdone <- read.csv("data/solow-v4.csv")
solowdone2 <- solowdone %>% group_by(ISO) %>% mutate(ess.adj=ifelse(is.na(ess), min(solowdone$ess, na.rm=T), ess), weight.ess=ess.adj / sum(ess.adj), lp.adj=ifelse(lp > 1, lp, exp(lp - 1)), weight.lp=lp.adj / sum(lp.adj), weight=weight.ess * weight.lp) # (sign(totimpact.end + itlimpact.end) == sign(product.chg)) * # drops entries where sign(Solow) <> sign(impact)
solowdone3 <- solowdone2[!duplicated(paste(solowdone2$ISO, solowdone2$mc)),]

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

allyr.ww <- allyr %>% left_join(solowdone3, by=c('ISO', 'mc'), suffix=c('', '.solowdone')) %>%
    filter(is.na(weight) | weight > 1e-9) %>% group_by(ISO, Year) %>%
    mutate(weight.norm=case_when(
               all(is.na(weight)) ~ 1/length(weight),
               is.na(weight) ~ 0,
               TRUE ~ weight / sum(weight, na.rm=T)))

save(allyr.ww, file="data/allyr-ww.RData")
## load("data/allyr-ww.RData")

allyr2 <- allyr.ww %>% group_by(ISO, Year) %>%
    filter(weight.norm > 1e-9) %>%
    dplyr::summarize(totimpact=wtd.median(totimpact, weights=weight.norm, normwt=T), itlimpact=wtd.median(itlimpact, weights=weight.norm, normwt=T), solow=ifelse(rep(all(is.na(partprod.chg)), length(partprod.chg)), NA, wtd.median(partprod.chg - totimpact - itlimpact, weights=weight.norm, normwt=T)), total=ifelse(rep(all(is.na(partprod.chg)), length(partprod.chg)), wtd.median(totimpact + itlimpact, weights=weight.norm, normwt=T), wtd.median(partprod.chg, weights=weight.norm, normwt=T)), prod25=ifelse(rep(all(is.na(partprod.chg)), length(partprod.chg)), wtd.quantile(totimpact + itlimpact, .25, weights=weight.norm, normwt=T), wtd.quantile(partprod.chg, .25, weights=weight.norm, normwt=T)), prod75=ifelse(rep(all(is.na(partprod.chg)), length(partprod.chg)), wtd.quantile(totimpact + itlimpact, .75, weights=weight.norm, normwt=T), wtd.quantile(partprod.chg, .75, weights=weight.norm, normwt=T)))

tohighlight <- c('USA', 'CHN', 'IND', 'BEL', 'RUS', 'BRA', 'AUS', 'MDV', 'NGA', 'SAU')
allyr2$label <- ifelse(allyr2$ISO %in% tohighlight, allyr2$ISO, 'XXX')

ggplot(allyr2, aes(Year, total, group=ISO, colour=label)) +
    coord_cartesian(ylim=c(-.2, .15)) +
    geom_hline(yintercept=0) +
    geom_line(data=subset(allyr2, label == 'XXX' & total != 0), linewidth=.1) +
    geom_line(data=subset(allyr2, label != 'XXX' & total != 0), linewidth=1) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2022)) + scale_y_continuous("Change in GDP due to climate change (%)", labels=scales::percent) +
    scale_colour_manual(NULL, breaks=c(tohighlight, 'XXX'), values=c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#b15928', '#6a3d9a', '#00000080'), labels=c(countrycode(tohighlight, 'iso3c', 'country.name'), 'Others')) +
    theme_bw()
ggsave("figures/timeseries.pdf", width=8, height=4)

## Create population and GDP-weighted means

df.gdp2 <- read.wb("data/capital/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_5871893.xls", 'GDP.2015')
df.gdp3 <- df.gdp2 %>% group_by(`Country Code`) %>%
    dplyr::summarize(GDP.Year=ifelse(any(!is.na(GDP.2015)), Year[tail(which(!is.na(GDP.2015)), 1)], NA),
                     GDP.2015=ifelse(any(!is.na(GDP.2015)), GDP.2015[tail(which(!is.na(GDP.2015)), 1)], NA))

df.pop2 <- read.wb("data/capital/API_SP.POP.TOTL_DS2_en_excel_v2_5871620.xls", 'POP')
df.pop3 <- df.pop2 %>% group_by(`Country Code`) %>%
    dplyr::summarize(POP.Year=ifelse(any(!is.na(POP)), Year[tail(which(!is.na(POP)), 1)], NA),
                     POP=ifelse(any(!is.na(POP)), POP[tail(which(!is.na(POP)), 1)], NA))

allyr2 <- allyr.ww %>% left_join(df.gdp3, by=c('ISO'='Country Code')) %>% left_join(df.pop3, by=c('ISO'='Country Code'))

allyr3.pop <- allyr2 %>% group_by(mc, Year) %>%
    dplyr::summarize(totimpact=wtd.mean(totimpact, weights=POP, normwt=T), itlimpact=wtd.mean(itlimpact, weights=POP, normwt=T), solow=ifelse(all(is.na(partprod.chg)), NA, wtd.mean(partprod.chg - totimpact - itlimpact, weights=POP, normwt=T)), total=ifelse(all(is.na(partprod.chg)), wtd.mean(totimpact + itlimpact, weights=POP, normwt=T), wtd.mean(partprod.chg, weights=POP, normwt=T)), weight2=wtd.mean(weight.norm, weights=POP)) %>%
    group_by(Year) %>%
    dplyr::summarize(solow=ifelse(all(is.na(total)), NA, wtd.median(total - totimpact - itlimpact, weights=weight2, normwt=T)), prod25=ifelse(all(is.na(total)), wtd.quantile(totimpact + itlimpact, .25, weights=weight2, normwt=T), wtd.quantile(total, .25, weights=weight2, normwt=T)), prod75=ifelse(all(is.na(total)), wtd.quantile(totimpact + itlimpact, .75, weights=weight2, normwt=T), wtd.quantile(total, .75, weights=weight2, normwt=T)), total=ifelse(all(is.na(total)), wtd.median(totimpact + itlimpact, weights=weight2, normwt=T), wtd.median(total, weights=weight2, normwt=T)), totimpact=wtd.median(totimpact, weights=weight2, normwt=T), itlimpact=wtd.median(itlimpact, weights=weight2, normwt=T))
allyr3.pop$totalloess <- tail(predict(loess(total ~ Year, allyr3.pop, span=.25)), nrow(allyr3.pop))
allyr3.pop$prod25loess <- tail(predict(loess(prod25 ~ Year, allyr3.pop, span=.25)), nrow(allyr3.pop))
allyr3.pop$prod75loess <- tail(predict(loess(prod75 ~ Year, allyr3.pop, span=.25)), nrow(allyr3.pop))

allyr3.gdp <- allyr2 %>% group_by(mc, Year) %>%
    dplyr::summarize(totimpact=wtd.mean(totimpact, weights=GDP.2015, normwt=T), itlimpact=wtd.mean(itlimpact, weights=GDP.2015, normwt=T), solow=ifelse(all(is.na(partprod.chg)), NA, wtd.mean(partprod.chg - totimpact - itlimpact, weights=GDP.2015, normwt=T)), total=ifelse(all(is.na(partprod.chg)), wtd.mean(totimpact + itlimpact, weights=GDP.2015, normwt=T), wtd.mean(partprod.chg, weights=GDP.2015, normwt=T)), weight2=wtd.mean(weight.norm, weights=GDP.2015)) %>%
    group_by(Year) %>%
    dplyr::summarize(prod25=ifelse(all(is.na(total)), wtd.quantile(totimpact + itlimpact, .25, weights=weight2, normwt=T), wtd.quantile(total, .25, weights=weight2, normwt=T)), prod75=ifelse(all(is.na(total)), wtd.quantile(totimpact + itlimpact, .75, weights=weight2, normwt=T), wtd.quantile(total, .75, weights=weight2, normwt=T)), total=ifelse(all(is.na(total)), wtd.median(totimpact + itlimpact, weights=weight2, normwt=T), wtd.median(total, weights=weight2, normwt=T)), totimpact=wtd.median(totimpact, weights=weight2, normwt=T), itlimpact=wtd.median(itlimpact, weights=weight2, normwt=T), solow=ifelse(all(is.na(total)), NA, wtd.median(total - totimpact - itlimpact, weights=weight2, normwt=T)))
allyr3.gdp$totalloess <- tail(predict(loess(total ~ Year, allyr3.gdp, span=.25)), nrow(allyr3.gdp))
allyr3.gdp$prod25loess <- tail(predict(loess(prod25 ~ Year, allyr3.gdp, span=.25)), nrow(allyr3.gdp))
allyr3.gdp$prod75loess <- tail(predict(loess(prod75 ~ Year, allyr3.gdp, span=.25)), nrow(allyr3.gdp))

allyr4 <- rbind(cbind(allyr3.pop, weights="Population"), cbind(allyr3.gdp, weights="Output"))

ggplot(allyr4, aes(Year, totalloess)) +
    geom_line(aes(colour=weights)) + geom_ribbon(aes(ymin=prod25loess, ymax=prod75loess, group=weights), alpha=.5) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0)) + scale_colour_discrete("Weighting:")
ggsave("figures/globaltime.pdf", width=6.5, height=4)

ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact + itlimpact, colour="Direct + International")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total")) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=.5) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2022)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + International", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))
ggsave("figures/globaltime-noloess.pdf", width=6.5, height=4)

## Pres fig 1: direct-only
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact + itlimpact, colour="Direct + International"), alpha=0) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1, alpha=0) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2022)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + International", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))

## Pres fig 2: direct + international
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact + itlimpact, colour="Direct + International")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1, alpha=0) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2022)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + International", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))

## Pres fig 3: total
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact + itlimpact, colour="Direct + International")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total"), alpha=0) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2022)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + International", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))

## Pres fig 4: + output-weighted
ggplot(allyr3.pop, aes(Year)) +
    geom_line(aes(y=totimpact, colour="Direct Impact")) +
    geom_line(aes(y=totimpact + itlimpact, colour="Direct + International")) +
    geom_line(aes(y=total, colour="Total Impact"), linewidth=1) +
    geom_line(data=allyr3.gdp, aes(y=total, colour="Output-weighted Total")) +
    geom_ribbon(data=subset(allyr4, weights == "Population"), aes(ymin=prod25, ymax=prod75, group=weights), alpha=0) +
    theme_bw() + scale_y_continuous("Global weighted changing in GDP (%)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2022)) +
    scale_colour_manual(NULL, breaks=c("Direct Impact", "Direct + International", "Total Impact", "Output-weighted Total"), values=c("#1b9e77", "#d95f02", "#000000", "#808080")) + theme(legend.position=c(.5, .25))


## Numbers for report
subset(allyr3.pop, Year == 2022) # -0.0633
subset(allyr3.gdp, Year == 2022) # -0.0176
sum((subset(allyr2, Year == 2022) %>% group_by(ISO) %>% summarize(GDP.2015=GDP.2015[1]))$GDP.2015, na.rm=T)

## Determine ranges

allyr2 <- allyr.ww %>% group_by(ISO, mc, Year) %>%
    mutate(total=ifelse(all(is.na(partprod.chg)), totimpact + itlimpact, partprod.chg)) %>%
    group_by(ISO, mc) %>% mutate(smooth=stats::filter(total, rep(1/19, 19), sides=1))

allyr3 <- allyr2 %>% filter(Year >= 2000) %>% group_by(ISO, mc) %>% summarize(stddev=sd(smooth - total)) %>% group_by(ISO) %>% summarize(sd.mu=median(stddev, na.rm=T), sd.ci25=quantile(stddev, .25, na.rm=T), sd.ci75=quantile(stddev, .75, na.rm=T))

## Total losses per year for L+MIC

polydata <- attr(importShapefile("regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

df.gdp3b <- subset(df.gdp2, `Country Code` %in% unique(df.gdp2$`Country Code`[!is.na(df.gdp2$GDP.2015)]) & !(`Country Code` %in% c("LIE", 'NCL'))) %>% group_by(`Country Code`) %>%
    reframe(Year=Year, GDP.2015.est=approx(Year, GDP.2015, Year, rule=2)$y)

df.pro2b <- read.iw("capital/tabula-C-produced.csv", 'Produced Capital') %>%
    filter(!is.na(ISO)) %>% group_by(ISO) %>%
    reframe(`Produced Capital Est`=approx(Year, `Produced Capital`, 1960:2022, rule=2)$y, Year=1960:2022)
df.ren2b <- read.iw("capital/tabula-A2-renewable.csv", 'Renewable Capital') %>%
    filter(!is.na(ISO)) %>% group_by(ISO) %>%
    reframe(`Renewable Capital Est`=approx(Year, `Renewable Capital`, 1960:2022, rule=2)$y, Year=1960:2022)

log2lev <- function(xx) {
    exp(xx) - 1
}

pdf <- allyr.ww %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>%
    left_join(df.gdp3b, by=c('Year', 'ISO'='Country Code')) %>%
    left_join(df.pro2b, by=c('Year', 'ISO')) %>% left_join(df.ren2b, by=c('Year', 'ISO')) %>%
    filter(INCOME_GRP %in% c("5. Low income", "4. Lower middle income", "3. Upper middle income")) %>%
    mutate(totimpact.usd=(log2lev(totimpact) / (1 + log2lev(totimpact))) * GDP.2015.est / 1e9,
           itlimpact.usd=(log2lev(itlimpact) / (1 + log2lev(itlimpact))) * GDP.2015.est / 1e9,
           solow=partprod.chg - totimpact - itlimpact,
           solow.usd=(log2lev(solow) / (1 + log2lev(solow))) * GDP.2015.est / 1e9) %>%
           #cumul.allcap.usd=pmax(allcap.true - allcap.nocc, -(`Produced Capital Est` + `Renewable Capital Est`)) * 100 / 83.6,
           #allcap.usd=c(NA, diff(cumul.allcap.usd))) %>%
    group_by(Year, mc) %>%
    dplyr::summarize(totimpact.usd=sum(totimpact.usd, na.rm=T),
                     itlimpact.usd=sum(itlimpact.usd, na.rm=T),
                     solow.usd=sum(solow.usd, na.rm=T),
                     #allcap.usd=sum(allcap.usd, na.rm=T),
                     total.usd=totimpact.usd + itlimpact.usd + solow.usd, # + allcap.usd,
                     weight2=sum(weight.norm)) %>%
    group_by(Year) %>%
    dplyr::summarize(totimpact.usd=wtd.median(totimpact.usd, weights=weight2, normwt=T),
                     itlimpact.usd=wtd.median(itlimpact.usd, weights=weight2, normwt=T),
                     solow.usd=wtd.median(solow.usd, weights=weight2, normwt=T),
                     #allcap.usd=wtd.median(allcap.usd, weights=weight2, normwt=T),
                     total.usd.25=wtd.quantile(total.usd, .25, weights=weight2, normwt=T),
                     total.usd.75=wtd.quantile(total.usd, .75, weights=weight2, normwt=T),
                     total.usd=wtd.median(total.usd, weights=weight2, normwt=T))

pdf2 <- melt(pdf[, 1:4], 'Year')

ggplot(pdf2, aes(Year)) +
    geom_col(aes(y=value, fill=variable)) +
    geom_errorbar(data=pdf, aes(ymin=total.usd.25, ymax=total.usd.75)) +
    theme_bw() + scale_y_continuous("Total global loss in lower-income countries ($billion)") +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1950, 2022.7)) +
    scale_fill_manual(NULL, breaks=rev(c('totimpact.usd', 'itlimpact.usd', 'solow.usd')), labels=rev(c("Direct Impact", "International Impact", "Capital Impact")), values=c("#1b9e77", "#d95f02", "#7570b3")) + theme(legend.position=c(.5, .25))
ggsave("figures/totalbyyear.pdf", width=8, height=4)
