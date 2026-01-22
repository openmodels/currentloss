## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(dplyr)
library(mice)

load("data/mcres.RData")
load("data/mcres-decumul.RData")

source("src/lib/loadmetadata.R")

metadata.plot <- metadata
names(metadata.plot)[grep("R2", names(metadata))] <- gsub(" ", ".", names(metadata.plot)[grep("R2", names(metadata))])
micemodel <- mice(metadata.plot[, c(grep("Q.", names(metadata.plot)), grep("R2", names(metadata.plot)))])
stripplot(micemodel)

micemodel <- mice(metadata[, c(grep("Q.", names(metadata)), grep("R2", names(metadata)))])
metadata2 <- complete(micemodel)
metadata2$papername <- paste(metadata$Paper, metadata$Name)
metadata2$`Adjusted R2`[metadata2$`Adjusted R2` < 0] <- 0
metadata2$`Raw R2` <- ifelse(is.na(metadata$`Total R2`), metadata$`Adjusted R2`, metadata$`Total R2`)
metadata2$`Raw R2`[is.na(metadata2$`Raw R2`)] <- 0

r2cols <- names(metadata2)[grep("R2", names(metadata2))]
isos <- unique(mcres$ISO)
years <- unique(mcres$Year)
nummc <- max(mcres$mc)

## Exclude columns from Zhao et al. because of extreme (> 100%) single-year impacts
mcres <- subset(mcres, paper != "Zhao et al. 2018" | !(name %in% c("Table 3, Col. 2", "Table 3, Col. 3", "Table 3, Col. 5", "Table 3, Col. 6", "Table 3, Col. 7")))
for (persist in c("0.6", "0.36", "0.78", "0"))
    decumul.bypersist[[persist]] <- subset(decumul.bypersist[[persist]], name != "With Linear Trends")

for (persist in c("0.6", "0.36", "0.78", "0")) {
    allres <- rbind(subset(mcres, paper != "Kotz et al. 2022"), decumul.bypersist[[persist]])

    ## Find rows for valid models that are NA (before some point in that model)
    allresfix <- allres %>% group_by(ISO, paper, name) %>% filter(!all(is.na(dimpact))) %>%
        mutate(dimpact=ifelse(is.na(dimpact), 0, dimpact))
    allres2 <- allres %>% left_join(allresfix, by=c('ISO', 'Year', 'paper', 'name', 'mc'), suffix=c('.ori', '.fix'))
    allres2$dimpact <- ifelse(is.na(allres2$dimpact.ori), allres2$dimpact.fix, allres2$dimpact.ori)

    rm(allres, allresfix)

    ## For the mainmed method
    allres2$papername <- paste(allres2$paper, allres2$name)

    for (r2col in c("Total R2", "Raw R2", r2cols[!(r2cols %in% c("Total R2", "Raw R2"))])) {
        print(c(r2col, persist))

        for (mcii in 1:nummc) {
            print(c(persist, mcii))

            outpath <- paste0("data/metaanal/mcr2res-", persist, "-", r2col, "-", mcii, ".RData")
            if (file.exists(outpath))
                next

            allres3 <- subset(allres2, mc == mcii) %>% left_join(metadata2, by='papername')
            allres3$weights <- allres3[, r2col]

            results <- allres3 %>%
                group_by(mc, ISO, Year) %>% summarize(dimpact=sample(dimpact, 1, prob=weights))

            save(results, file=outpath)
        }
    }
}
