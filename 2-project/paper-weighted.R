## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(dplyr)
library(PBSmapping)

do.skip.existing <- F
sample.approaches <- c("mainmed", "main", "all")

mem.maxVSize(Inf)

load("data/mcres.RData")
load("data/mcres-decumul.RData")

main.models <- list("Dell et al. 2012"="Main 2.3", "Burke et al. 2015"="Main", "Callahan & Mankin 2022"="Main",
                    "Pretis et al. 2018"="M2", #"Baarsch et al. 2020"="Current",
                    "Acevedo et al. 2020"="column_5",
                    "Kahn et al. 2021"="Table 2, Spec. 1, m = 30, HPJ-FE", "Kotz et al. 2022"="Main",
                    "Kalkuhl & Wenz 2020"="Table 4, Spec. 5",
                    "Sequeira et al. 2018"="Table 5, Spec. 1 & 2, 4 & 5",
		    "Zhao et al. 2018"="Table 3, Col. 3",
		    "Damania et al. 2020"="Table 1, Col 1",
		    "Henseler & Schumacher 2019"="Main spec.",
		    "Burke et al. 2018"="Main spec.",
		    "De Vos & Everaert 2021"="Table 5, CCEPbc",
		    "Yang et al. 2023"="Table 6, FE-NLS, 6",
                    "Bareille et al. 2024" = "Table 3, Model 4",
                    "Zhang et al. 2024" = "Table A3",
                    "Meierrieks & Stadelmann 2024" = "Table 2, Column 6",
                    "Apergis & Rehman 2024" = "Table 2",
                    "Brown et al. 2013" = "Table 2, T2W",
                    #"Kahn et al. 2017" = NULL, # Preferred in model 3, with no temperature
                    "Liu et al. 2023" = "Table S1, Lag 1",
                    "Yang et al. 2025" = "Panel B, Covariate-dependent threshold",
                    "Gupta et al. 2024" = "Table 1, Split",
                    "Jiao et al. 2024" = "Adaptation IIS",
                    "Benhamed et al. 2023" = "Table 4, LMI/HI, Contiguity",
                    "Desbordes & Eberhardt 2024" = "Table 3, CCE3, Col 6")
model.order <- rev(names(main.models))

for (sample.approach in sample.approaches) {
    for (persist in c("0.6", "0", "0.36", "0.78")) {
        allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[persist]])

        ## Find rows for valid models that are NA (before some point in that model)
        allresfix <- allres %>% group_by(ISO, paper, name) %>% filter(!all(is.na(dimpact))) %>%
            mutate(dimpact=ifelse(is.na(dimpact), 0, dimpact))
        allres2 <- allres %>% left_join(allresfix, by=c('ISO', 'Year', 'paper', 'name', 'mc'), suffix=c('.ori', '.fix'))
        allres2$dimpact <- ifelse(is.na(allres2$dimpact.ori), allres2$dimpact.fix, allres2$dimpact.ori)

        ## For the mainmed method
        allres2$papername <- paste(allres2$paper, allres2$name)
        main.papernames <- sapply(names(main.models), function(paper) paste(paper, main.models[[paper]]))

        for (mcii in 1:max(allres$mc)) {
            print(c(persist, mcii))

            outpath <- paste0("data/metaanal/mcpaperres-", persist, "-", sample.approach, "-", mcii, ".RData")
            if (file.exists(outpath) && do.skip.existing)
                next

            results <- data.frame()
            if (sample.approach == 'mainmed') {
                allres3 <- subset(allres2, papername %in% main.papernames & mc == mcii) %>% group_by(ISO, Year) %>% summarize(mc=mc[1], dimpact=median(dimpact, na.rm=T))
            } else {
                paperii <- model.order[((mcii - 1) %% length(model.order)) + 1]
                if (sample.approach == 'main') {
                    allres3 <- subset(allres2, paper == paperii & mc == mcii & name == main.models[[paperii]])
                } else if (sample.approach == 'all') {
                    nameii <- sample(unique(allres2$name[allres2$paper == paperii]), 1)
                    allres3 <- subset(allres2, paper == paperii & mc == mcii & name == nameii)
                }
            }

            results <- rbind(results, allres3[, c('mc', 'Year', 'ISO', 'dimpact')])

            save(results, file=outpath)
        }

    }
}
