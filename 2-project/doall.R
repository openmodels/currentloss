setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses/data")

library(readxl)
library(PBSmapping)
library(MASS)

source("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses/src/2-project/driver.R")
source("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses/src/lib/utils.R")

metadata <- read_xlsx("Current Losses Estimate Metadata.xlsx")
polydata <- attr(importShapefile("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses/data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

papers <- list("Dell et al. 2012" = "../src/models/djo.R", "Callahan & Mankin 2022" = "../src/models/callahanmankin.R",
                "Burke et al. 2015" = "../src/models/burkeetal.R","Pretis et al. 2018" = "../src/models/pretis.R",
                "Baarsch et al. 2020" = "../src/models/baarsch.R", "Acevedo et al. 2020" = "../src/models/acevedo.R",
               "Kahn et al. 2021" = "../src/models/kahnetal.R", "Kotz et al. 2022" = "../src/models/kotzetal.R"
                )


results <- data.frame()
allres <- data.frame()
for (paper in names(papers)) {
    source(papers[[paper]])

    for (name in metadata$Name[metadata$Paper == paper]) {
        print(name)
        oneres.not.contemp.only <- NULL
        for (contemp.only in c(F, T)) {
            funcs <- get.funcs(name)
            if (is.null(funcs))
                next
            oneres <- project.single(funcs$setup, funcs$simulate, contemp.only=contemp.only, adm.level=ifelse(paper == "Kotz et al. 2022", 1, 0))
            if (contemp.only == F)
                oneres.not.contemp.only <- oneres
            else {
                if (all(!is.na(oneres$dimpact)) && (all(oneres$projs == oneres.not.contemp.only$projs) || all(oneres$dimpact[!is.na(oneres$dimpact)] == 0)))
                    next
            }

            byyear <- oneres %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
                group_by(Year) %>% summarize(dimpact.pop=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST[!is.na(dimpact)]))
            results <- rbind(results, cbind(byyear, name=name, paper=paper, contemp.only))
            allres <- rbind(allres, cbind(oneres, name=name, paper=paper, contemp.only))
        }
    }
}

results$preferred <- NA
allres$preferred <- NA
for (paper in names(papers)) {
    for (name in unique(results$name[results$paper == paper])) {
        if (sum(!is.na(allres$dimpact[allres$paper == paper & allres$name == name & allres$contemp.only == F])) > 1000) {
            results$preferred[results$paper == paper & results$name == name & results$contemp.only == F] <- T
            allres$preferred[allres$paper == paper & allres$name == name & allres$contemp.only == F] <- T
        } else if (sum(!is.na(allres$dimpact[allres$paper == paper & allres$name == name & allres$contemp.only == T])) > 1000) {
            results$preferred[results$paper == paper & results$name == name & results$contemp.only == T] <- T
            allres$preferred[allres$paper == paper & allres$name == name & allres$contemp.only == T] <- T
        } else {
            print(paste("None for", paper, name))
        }
    }
}

write.csv(allres, "allres.csv", row.names=F)

library(ggplot2)

results2 <- results %>% filter(preferred) %>% group_by(Year) %>% summarize(dimpact.pop=median(dimpact.pop, na.rm=T))

ggplot(subset(results, preferred), aes(Year, dimpact.pop, colour=paper, group=paste(paper, name))) +
    coord_cartesian(ylim=c(-0.04, 0.01)) +
    geom_line() +
    geom_line(data=results2, size=2, colour='black') +
    theme_bw() + ylab("Impact (change in growth rate)")
ggsave("../figures/allimpacts.pdf", width=8, height=4)

allres2 <- allres %>% group_by(Year, ISO, name, paper) %>%
    summarize(projs.lags=ifelse(any(contemp.only), projs[!contemp.only] - projs[contemp.only], NA),
              bases.lags=ifelse(any(contemp.only), bases[!contemp.only] - bases[contemp.only], NA),
              dimpact.lags=ifelse(any(contemp.only), dimpact[!contemp.only] - dimpact[contemp.only], NA),
              projs=projs[!contemp.only], bases=bases[!contemp.only], dimpact=dimpact[!contemp.only])
results2 <- results %>% group_by(Year, name, paper) %>%
    summarize(dimpact.pop.lags=ifelse(any(contemp.only), dimpact.pop[!contemp.only] - dimpact.pop[contemp.only], NA),
              dimpact.pop=dimpact.pop[!contemp.only])
results3 <- results2 %>% group_by(Year) %>% summarize(dimpact.pop.lags=median(dimpact.pop.lags, na.rm=T))

ggplot(results2, aes(Year, dimpact.pop.lags, colour=paper, group=paste(paper, name))) +
    geom_line() +
    geom_line(data=results3, size=2, colour='black') +
    theme_bw()
ggsave("../figures/lagimpacts.pdf", width=8, height=4)

## Now make the Monte Carlo
MCNUM <- 30 # Have lots of models
mcres <- data.frame()
for (paper in names(papers)) {
    source(papers[[paper]])
    for (name in unique(results$name[results$paper == paper])) {
        print(c(paper, name))
        funcs <- get.funcs(name)
        contemp.only <- results$contemp.only[results$paper == paper & results$name == name & results$preferred][1]
        onemcres <- project.mc(funcs$setup, funcs$simulate, contemp.only=contemp.only, adm.level=ifelse(paper == "Kotz et al. 2022", 1, 0))
        mcres <- rbind(mcres, cbind(onemcres, name=name, paper=paper, contemp.only))
    }
}

save(mcres, file="mcres.RData")
