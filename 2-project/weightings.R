## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(mice)
library(ggplot2)

source("~/projects/research-common/R/myPBSmapping.R")
source("src/lib/loadmetadata.R")

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

allres <- data.frame()
for (mc in 1:30) {
    filepath <- paste0("data/metaanal/mcrfres-0.6-", mc, "-obs.RData")
    if (!file.exists(filepath)) {
        print(paste("Missing after", mc))
        break
    }
    load(filepath)
    allres <- rbind(allres, results)
}

allres2 <- allres %>% filter(Year >= 1960) %>%
    left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(paper, name) %>%
    summarize(usage=sum(usage * POP_EST) / sum(POP_EST))

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
		    "Yang et al. 2023"="Table 6, FE-NLS, 6")
allres2$main <- sapply(1:nrow(allres2), function(ii) main.models[[allres2$paper[ii]]] == allres2$name[ii])

allres3 <- allres2 %>% group_by(paper) %>% summarize(low=min(usage), high=max(usage), count=length(usage), main=usage[main])

micemodel <- mice(metadata[, c(grep("Q.", names(metadata)), grep("R2", names(metadata)))])
metadata2 <- complete(micemodel)
metadata2$paper <- metadata$Paper
metadata2$name <- metadata$Name
metadata2$main <- sapply(1:nrow(metadata2), function(ii) main.models[[metadata$Paper[ii]]] == metadata$Name[ii])

## Drop Total R2 values for entries not used in R2 approach
metadata2$`Total R2`[(metadata2$paper == "Zhao et al. 2018" & metadata2$name %in% c("Table 3, Col. 2", "Table 3, Col. 3", "Table 3, Col. 5", "Table 3, Col. 6", "Table 3, Col. 7")) | (metadata2$paper == "Kotz et al. 2022" & metadata2$name == "With Linear Trends")] <- 0

metadata3 <- metadata2 %>% group_by(paper) %>% summarize(low=min(`Total R2`), high=max(`Total R2`), main=`Total R2`[main])

allres4 <- allres3 %>% left_join(metadata3[, c('paper', 'low', 'high', 'main')], suffix=c('.rf', '.r2'), by='paper')

allres5 <- allres2 %>% left_join(metadata2[, c('paper', 'name', 'Total R2', 'main')], suffix=c('.rf', '.r2'), by=c('paper', 'name'))
allres5$usage <- allres5$usage / sum(allres5$usage)
allres5$`Total R2` <- allres5$`Total R2` / sum(allres5$`Total R2`)
allres5$main <- allres5$main.r2 / sum(allres5$main.r2)

## mean(allres5$usage[allres5$paper == "Kotz et al. 2022"])
## sum(allres5$usage[allres5$paper == "Kotz et al. 2022"])
## allres5 %>% group_by(paper) %>% summarize(usage=sum(usage))

newnames <- c(rbind(paste(allres5$paper, 1:nrow(allres5)), paste("white", 1:nrow(allres5))))
newnames <- factor(newnames, levels=newnames)

pdf <- data.frame(paper=newnames, usage=c(rbind(allres5$usage, rep(0.001, nrow(allres5)))), x=0, width=1)
for (ii in 1:99) {
    values <- (allres5$usage * (cos(pi * ii / 100) + 1) / 2 + allres5$`Total R2` *  (1 - cos(pi * ii / 100)) / 2)
    pdf <- rbind(pdf, data.frame(paper=newnames, usage=c(rbind(values, rep(0.001, length(values)))), x=0.5 + ii / 100, width=.05))
}
pdf <- rbind(pdf, data.frame(paper=newnames, usage=c(rbind(allres5$`Total R2`, rep(0.001, nrow(allres5)))), x=2, width=1))
for (ii in 1:99) {
    values <- (allres5$`Total R2` * (cos(pi * ii / 100) + 1) / 2 + allres5$main *  (1 - cos(pi * ii / 100)) / 2)
    pdf <- rbind(pdf, data.frame(paper=newnames, usage=c(rbind(values, rep(0.001, length(values)))), x=2.5 + ii / 100, width=.05))
}
pdf <- rbind(pdf, data.frame(paper=newnames, usage=c(rbind(allres5$main, rep(0.001, nrow(allres5)))), x=4, width=1))

colors <- c('#FF0000', '#00FF00', '#0000FF', '#FFFF00', '#FF00FF', '#00FFFF', '#800000', '#808000', '#008080', '#000080', '#FFA500', '#800080', '#A52A2A', '#008000', '#FF6347')
allres5$color <- colors[as.numeric(factor(allres5$paper))]

ggplot(pdf) +
    geom_col(aes(x=4 - x, y=usage, fill=paper, width=width)) +
    geom_label(data=data.frame(label=levels(factor(allres5$paper)), y=1.08 - (1:length(unique(allres5$paper)) - 0.5) / (length(unique(allres5$paper)) - 1.1)),
               aes(x=0, y=y, label=label), size=2) +
    theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position="none") +
    scale_fill_manual(breaks=newnames, values=c(rbind(allres5$color, rep("#FFFFFF", nrow(allres5))))) +
    scale_x_continuous(NULL, breaks=c(4, 2, 0), labels=c("Random Forest", "Total R2", "Main Only"), expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0))
ggsave("figures/weightings.pdf", width=6.5, height=4)
