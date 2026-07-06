## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(ggplot2)
source("src/lib/synth.R")

persists = c(0.36, 0.6, 0.78)
trade.methods <- c('dd-mcr2all', 'dd', 'dd-mcpaperall')
trade.method.labels <- c("R² Filled", "RF with all criteria", "Monte Carlo by spec.")

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}
polydata <- st_read("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

pdf <- data.frame()

for (persist in persists) {
    for (tt in 1:length(trade.methods)) {
        trade.method <- trade.methods[tt]
        print(c(persist, trade.method))

        if (!file.exists(paste0("data/allyr-ww-", persist, "-", trade.method, ".RData")))
            next

        source("src/lib/utils2.R")
        allyr.ww <- get.allyr.ww(persist, trade.method)

        allyr3.pop <- get.weighted.ts(allyr.ww, 'pop', 'global')
        allyr3.pop90 <- get.weighted.ts(allyr.ww, 'pop', 'global', .45)

        pdf <- rbind(pdf, cbind(persist=persist, metaanal=trade.method.labels[tt], subset(allyr3.pop, Year == 2023),
                                prod5=allyr3.pop90$prod25[allyr3.pop90$Year == 2023],
                                prod95=allyr3.pop90$prod75[allyr3.pop90$Year == 2023]))
    }
}

pdf2 <- pdf %>% mutate(group=ifelse(persist == 0.6, ifelse(metaanal == "R² Filled", "Main", "Meta-Anal."), "ω"),
                       label=ifelse(persist == 0.6, ifelse(metaanal == "R² Filled", "0.6 x R²",
                                                    ifelse(metaanal == "RF with all criteria", "RF", "MM")), as.character(persist)))
pdf2$group <- factor(pdf2$group, levels=c("Main", "ω", "Meta-Anal."))
pdf2$label <- factor(pdf2$label, levels=c("0.6 x R²", "0.36", "0.78", "MM", "RF"))

pdf3 <- subset(pdf2, metaanal != "RF with all criteria" | persist == 0.6)

ggplot(pdf3, aes(x=label, y=total, ymin=prod25, ymax=prod75)) +
    facet_wrap(~ group, scales='free_x', space='free_x') +
    geom_linerange(aes(ymin=prod75, ymax=prod95)) +
    geom_linerange(aes(ymin=prod5, ymax=prod25)) + geom_crossbar() +
    theme_bw() +
    scale_y_continuous("Global weighted change in GDP in 2014-2023 (%)", limits=c(min(pdf3$prod5), max(pdf3$prod95)), labels=scales::percent) +
    xlab(NULL)
ggsave("figures/globaltime-finalbars.pdf", width=2.5, height=3.7, device=cairo_pdf)
