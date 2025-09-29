## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    ## Testing the method
    dimpact <- rnorm(40)
    accum <- stats::filter(c(rep(0, 30), dimpact), (1 - .21)^(0:30), sides=1)[-1:-30]

    decay <- (1 - 0.31)^(0:30)

    ## Initialize vector to store reconstructed dimpact
    reconstructed_dimpact <- accum

    ## Iterate to reconstruct dimpact values
    for (tt in 2:length(accum)) {
        reconstructed_dimpact[tt] <- accum[tt] - sum(c(rep(0, 30), reconstructed_dimpact)[tt:(tt+29)] * rev(decay[-1]))
    }

    dimpact - reconstructed_dimpact
}

load("data/mcres.RData")

## Construct decumulated value for Kotz et al.

decumul.bypersist <- list()
for (persist in c(0, 0.31, 0.46, 0.78)) {
    mcres.kotz <- subset(mcres, paper == "Kotz et al. 2022")
    decay <- (1 - persist)^(0:30)
    revdeca <- rev(decay[-1])

    for (nam in unique(mcres.kotz$name)) {
        for (mm in unique(mcres.kotz$mc)) {
            print(c(persist, nam, mm))
            for (iso in unique(mcres.kotz$ISO)) {
                rrs <- which(mcres.kotz$name == nam & mcres.kotz$mc == mm & mcres.kotz$ISO == iso)
                decumul <- c(rep(0, 30), mcres.kotz$dimpact[rrs])

                ## Iterate to reconstruct dimpact values
                for (tt in 2:length(rrs)) {
                    decumul[tt+30] <- mcres.kotz$dimpact[rrs[tt]] - sum(decumul[tt:(tt+29)] * revdeca)
                }

                mcres.kotz$dimpact[rrs] <- decumul[31:length(decumul)]
            }
        }
    }

    decumul.bypersist[[as.character(persist)]] <- mcres.kotz
}

save(decumul.bypersist, file="data/mcres-decumul.RData")
## load("data/mcres-decumul.RData")

library(dplyr)

if (F) {
    for (persist in c(0.08, 0.31)) {
        mcres.kotz <- subset(mcres, paper == "Kotz et al. 2022")
        df.test <- decumul.bypersist[[as.character(persist)]] %>% group_by(paper, name, ISO, mc) %>%
            mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - persist)^(0:30), sides=1)[-1:-30])
        stopifnot(all(df.test$ISO == mcres.kotz$ISO & df.test$Year == mcres.kotz$Year & df.test$name == mcres.kotz$name))
        print(quantile(df.test$totimpact - mcres.kotz$dimpact))
    }
}

mcres.final <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[["0.46"]])

results <- mcres.final %>% group_by(Year, ISO, name, paper) %>% summarize(dimpact=mean(dimpact))

source("~/projects/research-common/R/myPBSmapping.R")
polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')
results2 <- results %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(Year, name, paper) %>% summarize(dimpact.pop=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST[!is.na(dimpact)]))
results2$paper[results2$paper == "Kotz et al. 2022"] <- "Kotz et al. 2022 (*)"

results3 <- results2 %>% group_by(Year) %>% summarize(dimpact.pop=median(dimpact.pop, na.rm=T))

library(ggplot2)
gp <- ggplot(results2, aes(Year, dimpact.pop)) +
    coord_cartesian(ylim=c(-0.05, 0.02)) +
    geom_line(aes(colour=paper, group=paste(paper, name)), linewidth=.3) +
    geom_line(data=results3, size=2, colour='black') +
    theme_bw() + ylab("Impact (change in growth rate)") + scale_colour_discrete(NULL)
ggsave("figures/allimpacts.pdf", width=8, height=4)
