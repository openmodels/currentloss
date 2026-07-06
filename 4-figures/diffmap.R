## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
source("src/lib/myPBSmapping.R")
library(countrycode)

persist = 0.6
trade.methods <- c('dd', 'dd-mcr2all')
source("src/lib/utils2.R")
source("src/lib/synth.R")

allyr.ww1 <- get.allyr.ww(persist, trade.methods[1])
allyr.ww0 <- get.allyr.ww(persist, trade.methods[2])

sumbymc20 <- allyr.ww0 %>% group_by(ISO, mc) %>%
    dplyr::summarize(across(dimpact:weight.norm, ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
    group_by(ISO, mc) %>%
    mutate(slrimpact=-slrloss, tradeimpact=-tradeloss,
           rencap.chg=1 - rencap.nocc / rencap.true,
           allcap.chg=1 - allcap.nocc / allcap.true, procap.chg=allcap.chg - rencap.chg,
           weight=weight.norm)
sumbymc21 <- allyr.ww1 %>% group_by(ISO, mc) %>%
    dplyr::summarize(across(dimpact:weight.norm, ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
    group_by(ISO, mc) %>%
    mutate(slrimpact=-slrloss, tradeimpact=-tradeloss,
           rencap.chg=1 - rencap.nocc / rencap.true,
           allcap.chg=1 - allcap.nocc / allcap.true, procap.chg=allcap.chg - rencap.chg,
           weight=weight.norm)

sumbymc2.both <- sumbymc20 %>% left_join(sumbymc21, suffix=c('.0', '.1'), by=c('ISO', 'mc')) %>%
    mutate(weight=weight.0 * weight.1,
           product.chg=product.chg.1 - product.chg.0,
           totimpact=totimpact.1 - totimpact.0,
           tradeimpact=tradeimpact.1 - tradeimpact.0,
           slrimpact=slrimpact.1 - slrimpact.0)

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

sumbyiso <- sumbymc2.both %>% filter(is.na(weight) | weight > 1e-10) %>% group_by(ISO) %>%
    dplyr::summarize(total.0=ifelse(all(is.na(product.chg.0)), wtd.median(totimpact.0 + tradeimpact.0 + slrimpact.0, weights=weight.0, normwt=T), wtd.median(product.chg.0, weights=weight.0, normwt=T)),
                     total.1=ifelse(all(is.na(product.chg.1)), wtd.median(totimpact.1 + tradeimpact.1 + slrimpact.1, weights=weight.1, normwt=T), wtd.median(product.chg.1, weights=weight.1, normwt=T)),
                     signchange=paste(sign(total.0), "->", sign(total.1)),
                     total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + tradeimpact + slrimpact, weights=weight, normwt=T), wtd.median(product.chg, weights=weight, normwt=T)), prod25=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .25, weights=weight, normwt=T), wtd.quantile(product.chg, .25, weights=weight, normwt=T)), prod75=ifelse(all(is.na(product.chg)), wtd.quantile(totimpact + tradeimpact + slrimpact, .75, weights=weight, normwt=T), wtd.quantile(product.chg, .75, weights=weight, normwt=T)))

source("src/lib/loadmaps.R")

shp2 <- shp %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(sumbyiso, by=c('ADM0_A3'='ISO'))
centroids2 <- centroids %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(sumbyiso, by=c('ADM0_A3'='ISO'))

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=pmin(quantile(sumbyiso$total, .99), pmax(quantile(sumbyiso$total, .01), log2lev(total))), group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=subset(centroids2, show), aes(label=paste0(ifelse(total >= .005, '+', ''), round(log2lev(total) * 100))), size=3, label.padding=unit(0.1, "lines")) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_gradient2("Difference in Change in GDP (pp):", low = scales::muted("red"), high = scales::muted("blue"), labels=scales::percent, guide=guide_colorbar(barwidth=10, barheight=1)) +
    theme_bw() + theme(legend.position="bottom", legend.key.width=unit(10,"in"),
                       legend.key.height=unit(1,"in"))
ggsave(paste0("figures/finalprod-map-diff-", persist, "-", trade.methods[1], '-', trade.methods[2], ".pdf"), width=10, height=5.5)

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=signchange, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_manual("Change in Sign of Change of GDP (%):", breaks=c("-1 -> -1", "-1 -> 1", "1 -> -1", "1 -> 1"),
                      labels=c("Unchanged Neg.", "Neg. to Pos.", "Pos. to Neg.", "Unchanged Pos."),
                      values=c('#e41a1c', '#4daf4a', '#984ea3', '#377eb8')) +
    theme_bw() + theme(legend.position="bottom")
ggsave(paste0("figures/finalprod-map-diffsign-", persist, "-", trade.methods[1], '-', trade.methods[2], ".pdf"), width=10, height=5.5)
