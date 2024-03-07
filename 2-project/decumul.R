## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    ## Testing the method
    dimpact <- rnorm(40)
    accum <- stats::filter(c(rep(0, 30), dimpact), (1 - .08)^(0:30), sides=1)[-1:-30]

    decay <- (1 - 0.08)^(0:30)

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

mcres.kotz <- subset(mcres, paper == "Kotz et al. 2022")

decumul.bypersist <- list()
for (persist in c(0.08, 0.21)) {
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

    decumul.bypersist[[persist]] <- mcres.kotz
}

save(decumul.bypersist, file="data/mcres-decumul.RData")

mcres.final <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[0.08]])

library(dplyr)
results <- mcres.final %>% group_by(Year, ISO, name, paper) %>% summarize(dimpact=mean(dimpact))

library(PBSmapping)
polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')
results2 <- results %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(Year, name, paper) %>% summarize(dimpact.pop=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST[!is.na(dimpact)]))
results2$paper[results2$paper == "Kotz et al. 2022"] <- "Kotz et al. 2022 (*)"

results3 <- results2 %>% group_by(Year) %>% summarize(dimpact.pop=median(dimpact.pop, na.rm=T))

library(ggplot2)
ggplot(results2, aes(Year, dimpact.pop)) +
    coord_cartesian(ylim=c(-0.04, 0.01)) +
    geom_line(aes(colour=paper, group=paste(paper, name))) +
    geom_line(data=results3, size=2, colour='black') +
    theme_bw() + ylab("Impact (change in growth rate)")
ggsave("figures/allimpacts.pdf", width=8, height=4)
