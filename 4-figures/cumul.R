## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
source("~/projects/research-common/R/myPBSmapping.R")
library(ggplot2)

source("src/lib/distance.R")

load("data/mcres.RData")
load("data/mcres-decumul.RData")

mcres.final <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[["0.46"]])

df.imp2 <-
    mcres.final %>% group_by(paper, name, ISO, mc) %>%
    mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - 0.46)^(0:30), sides=1)[-1:-30])

if (F) {
    mcres.kotz <- subset(mcres, paper == "Kotz et al. 2022")
    df.imp2.kotz <- subset(df.imp2, paper == "Kotz et al. 2022")
    stopifnot(all(df.imp2.kotz$ISO == mcres.kotz$ISO & df.imp2.kotz$Year == mcres.kotz$Year & df.imp2.kotz$name == mcres.kotz$name))
    print(quantile(df.imp2.kotz$totimpact - mcres.kotz$dimpact))
}

shp <- importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
polydata <- attr(shp, 'PolyData')

df.imp2pop <- df.imp2 %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name, Year) %>%
    summarize(totimpact.pop=sum(totimpact * POP_EST, na.rm=T) / sum(POP_EST[!is.na(totimpact)]))
df.imp2popmed <- df.imp2pop %>% group_by(Year) %>% summarize(totimpact.pop=median(totimpact.pop, na.rm=T))

ggplot(df.imp2pop, aes(Year, totimpact.pop)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    geom_line(data=df.imp2popmed, size=2, colour='black') +
    theme_bw() + scale_y_continuous("Direct Impact (% change in GDP)", labels=scales::percent) + xlab(NULL) +
    scale_colour_discrete("Reference:") + scale_x_continuous(limits=c(1950, 2022), expand=c(0, 0))
ggsave("figures/totimpacts-0.46.pdf", width=8, height=4)

load("data/mcrfres-0.46.RData")

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

results2 <- results %>% group_by(ISO, mc) %>%
    mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - 0.46)^(0:30), sides=1)[-1:-30]) %>%
    left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(Year, mc) %>% summarize(gloimpact=sum(totimpact * POP_EST) / sum(POP_EST)) %>%
    group_by(Year) %>% summarize(mu=mean(gloimpact),
                                 ci25=quantile(gloimpact, .25),
                                 ci75=quantile(gloimpact, .75))

labels <- data.frame(Year=c(1988, 1997), xend=c(1987, 2003),
                     y=c(results2$mu[results2$Year == 1988], df.imp2popmed$totimpact.pop[df.imp2popmed$Year == 1997]),
                     yend=c(-.04, .015), labelyend=c(-.045, .02), label=c("Random Forest", "Median Model"))

ggplot(df.imp2pop, aes(Year, totimpact.pop)) +
    coord_cartesian(ylim=c(-.15, .05)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    geom_line(data=df.imp2popmed, size=2, colour='black', alpha=.75) +
    geom_line(data=results2, aes(y=mu), size=2, colour='#b15928', alpha=.75) +
    geom_segment(data=labels, aes(xend=xend, y=y, yend=yend)) +
    geom_text(data=labels, aes(x=xend, y=labelyend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (% change in GDP)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1940, 2022)) +
    scale_colour_discrete("Reference:")
ggsave("figures/totimpacts-withrf-0.46.pdf", width=8, height=4)

## Presentation Fig 1: Models only
ggplot(df.imp2pop, aes(Year, totimpact.pop)) +
    coord_cartesian(ylim=c(-.1, .02)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    theme_bw() + scale_y_continuous("Direct Impact (% change in GDP)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1940, 2022)) +
    scale_colour_discrete("Reference:")

## Presentation Fig 2: Models + median
ggplot(df.imp2pop, aes(Year, totimpact.pop)) +
    coord_cartesian(ylim=c(-.1, .02)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    geom_line(data=df.imp2popmed, size=2, colour='black', alpha=.75) +
    geom_segment(data=subset(labels, label == "Median Model"), aes(xend=xend, y=y, yend=yend)) +
    geom_text(data=subset(labels, label == "Median Model"), aes(x=xend, y=labelyend, label=label), vjust="center", hjust="center") +
    theme_bw() + scale_y_continuous("Direct Impact (% change in GDP)", labels=scales::percent) +
    scale_x_continuous(NULL, expand=c(0, 0), limits=c(1940, 2022)) +
    scale_colour_discrete("Reference:")

df.imp3 <- df.imp2 %>% filter(Year == 2022) %>% group_by(ISO) %>% summarize(mu=median(totimpact, na.rm=T))

centroids <- calcCentroid(shp, rollup=1)

centroids$show <- F
for (PID in order(polydata$POP_EST, decreasing=T)) {
    dists <- gcd.slc(centroids$X[PID], centroids$Y[PID], centroids$X[centroids$show], centroids$Y[centroids$show])
    if (all(dists > 600))
        centroids$show[PID] <- T
}

shp2 <- shp %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(df.imp3, by=c('ADM0_A3'='ISO'))
centroids2 <- centroids %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(df.imp3, by=c('ADM0_A3'='ISO'))

shpl <- importShapefile("data/regions/ne_10m_land/ne_10m_land.shp")

ggplot(shp2, aes(X, Y)) +
    geom_polygon(aes(fill=mu, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=subset(centroids2, show), aes(label=round(mu * 100)), size=2, label.padding=unit(0.1, "lines")) +
    coord_map(ylim=c(-50, 65)) + xlab(NULL) + ylab(NULL) +
    scale_fill_gradient2("GDP per capita\nchange (%)", labels=scales::percent) +
    theme_bw()
ggsave("figures/cumul-product.pdf", width=8, height=4)

## write.csv(df.imp2, "totres.csv", row.names=F)
#save(df.imp2, file="mctotres.Rdata")
