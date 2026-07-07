## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(ggplot2)
library(Hmisc)

persist <- "0.46"
trade.method <- "dd-mcr2all"
source("src/lib/utils2.R")
source("src/lib/synth.R")

allyr.ww <- get.allyr.ww(persist, trade.method)

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}
polydata <- st_read("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

allyr3.pop <- get.weighted.ts(allyr.ww, 'pop', "global")
rm(allyr.ww)

load("data/mcres.RData")
load("data/mcres-decumul.RData")

allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[as.character(persist)]])
rm(mcres, decumul.bypersist)

allres2 <- allres %>% filter(!is.na(dimpact)) %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year, mc) %>% dplyr::summarize(gloimpact=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST)) %>%
    group_by(paper, name, Year) %>% summarize(mu=mean(gloimpact))

allres2.smooth <- allres2 %>% group_by(paper, name) %>% mutate(mu=stats::filter(c(rep(0, 9), mu), rep(1/10, 10), method='conv')[5:(length(mu)+4)])
allres2.smooth2 <- allres2.smooth %>% group_by(paper, name) %>%
    mutate(mu=stats::filter(c(rep(0, 30), mu), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30])

trade.method <- "dd-mcr2all"
source("src/lib/utils2.R")
source("src/lib/synth.R")
allyr.ww <- get.allyr.ww(persist, trade.method)
allyr3.pop <- get.weighted.ts(allyr.ww, 'pop', "global")

labels <- data.frame(Year=2010, xend=2010,
                     y=allyr3.pop$total[allyr3.pop$Year == 2010],
                     yend=.008, label="Combined Best Estimate")

ggplot(allres2.smooth2, aes(Year)) +
    coord_cartesian(ylim=c(-.05, .01)) +
    geom_line(aes(y=mu, colour=paper, linetype=paper, group=paste(paper, name)), linewidth=.3) +
    geom_line(data=allyr3.pop, aes(y=total), size=2, colour='black', alpha=.75) +
    geom_ribbon(data=allyr3.pop, aes(ymin=prod25, ymax=prod75), alpha=.5) +
    geom_segment(data=labels, aes(xend=xend, y=y, yend=yend)) +
    geom_label(data=labels, aes(x=xend, y=yend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (change in growth rate)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1959, 2023)) +
    scale_colour_manual("Reference:", values=rep(RColorBrewer::brewer.pal(8, "Dark2"), 2)) +
    scale_linetype_manual("Reference:", values=rep(c('solid', 'twodash'), each=8)) +
    theme(legend.justification=c(0,0), legend.position=c(.01,.01), legend.key.size=unit(0.5, 'lines'), legend.text=element_text(size=7)) +
    guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))
