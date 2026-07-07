## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(PBSmapping)
library(MASS)

source("src/lib/myPBSmapping.R")
source("src/2-project/driver.R")
source("src/lib/utils.R")

metadata <- read_xlsx("data/Current Losses Estimate Metadata.xlsx")
polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

papers <- list("Dell et al. 2012" = "src/models/djo.R",
               "Callahan & Mankin 2022" = "src/models/callahanmankin.R",
               "Burke et al. 2015" = "src/models/burkeetal.R",
               "Pretis et al. 2018" = "src/models/pretis.R",
               ##"Baarsch et al. 2020" = "src/models/baarsch.R",
               "Acevedo et al. 2020" = "src/models/acevedo.R",
               "Kahn et al. 2021" = "src/models/kahnetal.R",
               "Kotz et al. 2022" = "src/models/kotzetal.R",
               "Kalkuhl & Wenz 2020" = "src/models/kalkuhlwenz.R",
               "Sequeira et al. 2018" = "src/models/sequeira.R",
               "Zhao et al. 2018" = "src/models/zhaoetal.R",
               "Damania et al. 2020" = "src/models/ddz.R",
               "Henseler & Schumacher 2019" = "src/models/henselerschumacher.R",
               "Burke et al. 2018" = "src/models/burkeetal2018.R",
               "De Vos & Everaert 2021" = "src/models/devoseveraert.R",
               "Yang et al. 2023" = "src/models/yangetal.R",
               "Bareille et al. 2024" = "src/models/bareilleetal.R",
               "Zhang et al. 2024" = "src/models/zhangetal.R",
               "Meierrieks & Stadelmann 2024" = "src/models/meierrieksstadelmann.R",
               "Apergis & Rehman 2024" = "src/models/apergisrehman.R",
               "Brown et al. 2013" = "src/models/brownetal.R",
               "Kahn et al. 2017" = "src/models/kahnetal2017.R",
               "Liu et al. 2023" = "src/models/liuetal.R",
               "Yang et al. 2025" = "src/models/yangetal2025.R",
               "Gupta et al. 2024" = "src/models/guptaetal.R",
               "Jiao et al. 2024" = "src/models/jiaoetal.R",
               "Benhamed et al. 2023" = "src/models/benhamedetal.R",
               "Desbordes & Eberhardt 2024" = "src/models/desbordeseberhardt.R")

results <- data.frame()
allres <- data.frame()
for (paper in names(papers)) {
    source(papers[[paper]])

    for (name in metadata$Name[!is.na(metadata$Paper) & metadata$Paper == paper & metadata$Include == 'Included']) {
        print(name)
        oneres.not.contemp.only <- NULL
        for (contemp.only in c(F, T)) {
            funcs <- get.funcs(name)
            if (is.null(funcs))
                next
            oneres <- project.single(funcs$setup, funcs$simulate, contemp.only=contemp.only, adm.level=ifelse(paper %in% c("Kotz et al. 2022", "Kalkuhl & Wenz 2020", "Damania et al. 2020", "Zhao et al. 2018", "Bareille et al. 2024", "Kahn et al. 2017", "Meierrieks & Stadelmann 2024", "Zhang et al. 2024"), 1, 0))
            if (contemp.only == F)
                oneres.not.contemp.only <- oneres
            else {
                if (all(!is.na(oneres$dimpact)) && (all(oneres$projs == oneres.not.contemp.only$projs) || all(oneres$dimpact[!is.na(oneres$dimpact)] == 0)))
                    next
            }

            byyear <- oneres %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
                group_by(Year) %>% summarize(dimpact.pop=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST[!is.na(dimpact)]))
            results <- rbind(results, cbind(byyear, name=name, paper=paper, contemp.only=contemp.only))
            allres <- rbind(allres, cbind(oneres, name=name, paper=paper, contemp.only=contemp.only))
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

write.csv(allres, "data/allres.csv", row.names=F)
## allres = read.csv("data/allres.csv")

library(ggplot2)

results2 <- results %>% filter(preferred) %>% group_by(Year) %>% summarize(dimpact.pop=median(dimpact.pop, na.rm=T))

ggplot(subset(results, preferred), aes(Year, dimpact.pop, colour=paper, group=paste(paper, name))) +
    coord_cartesian(ylim=c(-0.04, 0.01)) +
    geom_line(alpha=.5) +
    geom_line(data=results2, size=1.5, colour='black') +
    theme_bw() + ylab("Impact (change in growth rate)")
ggsave("figures/allimpacts.pdf", width=8, height=4)

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
    geom_line(alpha=.5) +
    geom_line(data=results3, size=1.5, colour='black') +
    theme_bw()
ggsave("figures/lagimpacts.pdf", width=8, height=4)

## Now make the Monte Carlo
MCNUM <- 30 # Have lots of models
rm(allres, allres2)

## ## Drop specific models for regeneration
## for (impose.contemp.only in c(F, T)) {
##     load(paste0("data/mcres", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))
##     mcres <- subset(mcres, !(paper %in% c("Brown et al. 2013", "Yang et al. 2025")))
##     save(mcres, file=paste0("data/mcres", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))
## }

for (impose.contemp.only in c(F, T)) {
    if (file.exists(paste0("data/mcres", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))) {
        load(paste0("data/mcres", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))
    } else {
        mcres <- data.frame()
    }
    if (file.exists(paste0("data/mcres2", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))) {
        load(paste0("data/mcres2", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))
    } else {
        mcres2 <- data.frame()
    }
    for (paper in names(papers)) {
        source(papers[[paper]])
        for (name in unique(results$name[results$paper == paper])) {
            if (any(mcres$paper == paper & mcres$name == name) || any(mcres2$paper == paper & mcres2$name == name))
                next
            print(c(paper, name))
            funcs <- get.funcs(name)
            if (impose.contemp.only) {
                contemp.only <- T
            } else {
                contemp.only <- results$contemp.only[results$paper == paper & results$name == name & results$preferred][1]
            }
            onemcres <- project.mc(funcs$setup, funcs$simulate, contemp.only=contemp.only, adm.level=ifelse(paper %in% c("Kotz et al. 2022", "Kalkuhl & Wenz 2020", "Damania et al. 2020", "Zhao et al. 2018"), 1, 0))
            if (which(names(papers) == paper) < 10) {
                mcres <- rbind(mcres, cbind(onemcres, name=name, paper=paper, contemp.only=contemp.only))
                save(mcres, file=paste0("data/mcres", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))
            } else {
                mcres2 <- rbind(mcres2, cbind(onemcres, name=name, paper=paper, contemp.only=contemp.only))
                save(mcres2, file=paste0("data/mcres2", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))
            }
        }
    }

    mcres <- rbind(mcres, mcres2)
    save(mcres, file=paste0("data/mcres", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))
    file.remove(paste0("data/mcres2", ifelse(impose.contemp.only, "-contemp", ""), ".RData"))
}
