## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/jrising@udel.edu - Google Drive/My Drive/Research/Current Losses")

library(dplyr)
library(reshape2)

MCNUM <- 3000


era5 <- read.csv("data/era5-t2m-combo-adm0.csv")
era5 <- era5 %>% arrange(ISO)

era5.adm1 <- read.csv("data/era5-t2m-combo-adm1.csv")
era5.adm1 <- era5.adm1 %>% arrange(ADM1_Code)

years <- unique(era5$Year)
years.base <- years[1:20]

## Add on GDPpcs
gdppcs <- read.csv("data/socioeconomics/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_3630804/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_3630804.csv", skip=3)
gdppcs2 <- melt(gdppcs, names(gdppcs)[1:4])
gdppcs2$year <- sapply(gdppcs2$variable, function(ss) as.numeric(substring(ss, 2, 5)))
gdppcs3 <- gdppcs2 %>% group_by(Country.Code) %>% reframe(year=year, gdppc=value, Lgdppc=lag(value, 1), growth=log(gdppc) - log(Lgdppc))

era5b <- era5 %>% left_join(gdppcs3, by=c('ISO'='Country.Code', 'Year'='year'))
era5.adm1b <- era5.adm1 %>% left_join(gdppcs3, by=c('ISO'='Country.Code', 'Year'='year'))

project.single <- function(setup, simulate, mcii=NULL, contemp.only=F, adm.level=0) {
    if (adm.level == 0) {
        era5.source <- era5
        era5.sourceb <- era5b
    } else if (adm.level == 1) {
        era5.source <- era5.adm1
        era5.sourceb <- era5.adm1b
    }

    ## Setup for projection
    setupinfo <- setup(mcii)
    results.proj <- data.frame()
    for (year in years) {
        subera5 <- subset(era5.sourceb, Year == year)
        projs <- simulate(setupinfo, year, subera5, contemp.only=contemp.only)
        results.proj <- rbind(results.proj, data.frame(Year=year, ISO=subera5$ISO, projs=projs))
    }

    ## Setup for counterfactual
    setupinfo <- setup(mcii)
    results.base <- data.frame()
    for (yy in years) {
        if (yy %in% years.base)
            subera5.base <- subset(era5.source, Year == yy) %>% left_join(subset(gdppcs3, year == yy), by=c('ISO'='Country.Code'))
        else {
            subera5.base <- subset(era5.source, Year == sample(years.base, 1)) %>% left_join(subset(gdppcs3, year == yy), by=c('ISO'='Country.Code'))
            subera5.base$Year <- yy
        }
        bases <- simulate(setupinfo, yy, subera5.base, contemp.only=contemp.only)
        results.base <- rbind(results.base, data.frame(Year=yy, ISO=subera5.base$ISO, bases=bases))
    }

    results <- results.proj %>% left_join(results.base, by=c('Year', 'ISO'))
    results$dimpact <- results$projs - results$bases
    results
}

project.mc <- function(setup, simulate, contemp.only=F) {
    results <- data.frame()
    for (mcii in 1:MCNUM) {
        results <- rbind(results, cbind(project.single(setup, simulate, mcii, contemp.only=contemp.only), mc=mcii))
    }
    results
}



library(ggplot2)
library(cowplot)
library(grid)
library(scales)
library(PBSmapping)
global.adm0 <- importShapefile("data/regions/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
global.adm0.polydata <- attr(global.adm0, 'PolyData')

make.map.proj <- function(outdir, prefix, results, scale.title="change in\nGDP p.c. (%)", loval=NULL, hival=NULL) {
    for (scn in c('ssp126', 'ssp370')) {
        for (per in c(2020, 2050, 2090)) {
            for (measure in c('mean', 'ci05', 'ci95')) {
                subres <- subset(results[[scn]], year > per - 10 & year <= per + 10)

                if (measure == 'mean') {
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(mean(impact, na.rm=T)))
                    scale.title.full <- paste0("Expected\n", scale.title)
                } else if (measure == 'ci05') {
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(quantile(impact, .05, na.rm=T)))
                    scale.title.full <- paste0("5% q.\n", scale.title)
                } else if (measure == 'ci95') {
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(quantile(impact, .95, na.rm=T)))
                    scale.title.full <- paste0("1-in-20\n", scale.title)
                } else
                    print("Error")

                subres3 <- subres2 %>% group_by(ADM0) %>% summarize(fracloss=mean(fracloss, na.rm=T))

                make.map.inner(outdir, paste(prefix, scn, per, measure, sep='-'), subres3, scale.title.full, loval, hival)
            }
        }
    }
}

make.map.post <- function(outdir, prefix, results, scale.title="change in\nGDP p.c. (%)  ", loval=NULL, hival=NULL) {
    results$fracloss <- 1 - exp(results$impact)
    for (scn in unique(results$scenario)) {
        for (per in unique(results$period)) {
            for (measure in c('mean', 'ci05', 'ci95')) {
                subres <- subset(results, scenario == scn & period == per)

                if (measure == 'mean') {
                    subres2 <- subres %>% group_by(ADM0) %>% summarize(fracloss=mean(fracloss))
                    scale.title.full <- paste0("Expected\n", scale.title)
                } else if (measure == 'ci05') {
                    subres2 <- subres %>% group_by(ADM0) %>% summarize(fracloss=quantile(fracloss, .05, na.rm=T))
                    scale.title.full <- paste0("5% q.\n", scale.title)
                } else if (measure == 'ci95') {
                    subres2 <- subres %>% group_by(ADM0) %>% summarize(fracloss=quantile(fracloss, .95, na.rm=T))
                    scale.title.full <- paste0("1-in-20\n", scale.title)
                } else
                    print("Error")

                subres3 <- subres2 %>% group_by(ADM0) %>% summarize(fracloss=mean(fracloss, na.rm=T))

                make.map.inner(outdir, paste(prefix, scn, per, measure, sep='-'), subres3, scale.title.full, loval, hival)

            }
        }
    }
}

make.map.inner <- function(outdir, prefix, subres3, scale.title.full, loval, hival) {
    subres4 <- subres3 %>% left_join(global.adm0.polydata[, c('PID', 'ADM0_A3')], by=c('ADM0'='ADM0_A3'))
    shp2 <- global.adm0 %>% left_join(subres4[, c('PID', 'fracloss')])

    if (is.null(loval)) {
        gp <- ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
            coord_map(ylim=c(-55, 78), projection="mollweide") + scale_x_continuous(expand=c(0, 0)) + geom_polygon(aes(fill=fracloss), colour='#808080', size=.1) +
            scale_fill_gradient2(scale.title.full, limits=c(loval, hival), low=muted("blue"),
                                 high=muted("red"), oob=squish, labels=scales::percent) +
            theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="bottom",
                               axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.border=element_blank()) + xlab(NULL) + ylab(NULL)

        ggsave(file.path(outdir, paste0(prefix, '.pdf')), gp, width=7, height=3.7)
    } else {
        gp <- ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
            coord_map(ylim=c(-55, 78), projection="mollweide") + scale_x_continuous(expand=c(0, 0)) + geom_polygon(aes(fill=fracloss), colour='#808080', size=.1) +
            scale_fill_gradient2(scale.title.full, limits=c(loval, hival), low=muted("blue"),
                                 high=muted("red"), oob=squish, labels=scales::percent) +
            theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="bottom",
                               axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.border=element_blank()) + xlab(NULL) + ylab(NULL)

        ggsave(file.path(outdir, paste0(prefix, '.pdf')), gp + theme(legend.position="none"), width=7, height=3.2)

        legend <- get_legend(gp)
        pdf(file.path(outdir, paste0(prefix, '-legend.pdf')), width=3, height=1)
        grid.newpage()
        grid.draw(legend)
        dev.off()
    }
}
