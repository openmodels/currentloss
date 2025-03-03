## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(dplyr)
library(mice)

load("data/mcres.RData")
load("data/mcres-decumul.RData")

source("src/lib/loadmetadata.R")

micemodel <- mice(metadata[, c(grep("Q.", names(metadata)), grep("R2", names(metadata)))])
metadata2 <- complete(micemodel)
metadata2$papername <- paste(metadata$Paper, metadata$Name)
metadata2$`Adjusted R2`[metadata2$`Adjusted R2` < 0] <- 0
metadata2$`Raw R2` <- ifelse(is.na(metadata$`Total R2`), metadata$`Adjusted R2`, metadata$`Total R2`)
metadata2$`Raw R2`[is.na(metadata2$`Raw R2`)] <- 0

r2cols <- names(metadata)[grep("R2", names(metadata))]

for (r2col in r2cols) {
    for (persist in c("0", "0.21", "0.36", "0.47")) {
        allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[persist]])

        ## Find rows for valid models that are NA (before some point in that model)
        allresfix <- allres %>% group_by(ISO, paper, name) %>% filter(!all(is.na(dimpact))) %>%
            mutate(dimpact=ifelse(is.na(dimpact), 0, dimpact))
        allres2 <- allres %>% left_join(allresfix, by=c('ISO', 'Year', 'paper', 'name', 'mc'), suffix=c('.ori', '.fix'))
        allres2$dimpact <- ifelse(is.na(allres2$dimpact.ori), allres2$dimpact.fix, allres2$dimpact.ori)

        ## For the mainmed method
        allres2$papername <- paste(allres2$paper, allres2$name)

        for (mcii in 1:max(allres$mc)) {
            print(c(persist, mcii))

            outpath <- paste0("data/metaanal/mcr2res-", persist, "-", r2col, "-", mcii, ".RData")
            if (file.exists(outpath))
                next

	    chosen <- sample(metadata2$papername, 1, prob=metadata2[, r2col])
            allres3 <- subset(allres2, papername == chosen & mc == mcii)
            results <- allres3[, c('mc', 'Year', 'ISO', 'dimpact')]

            save(results, file=outpath)
        }

    }
}
