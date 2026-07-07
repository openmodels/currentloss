## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(mice)
library(xtable)

source("src/lib/loadmetadata.R")

micemodel <- mice(metadata[, c(grep("Q.", names(metadata)), grep("R2", names(metadata)))])
metadata2 <- complete(micemodel)

tdf <- metadata[, c('Paper', 'Name')]

tdf$`Total R2` <- paste0("{\\bf ", round(metadata$`Total R2`, 2), "}")
tdf$`Total R2`[is.na(metadata$`Total R2`)] <- as.character(round(metadata2$`Total R2`[is.na(metadata$`Total R2`)], 2))

tdf$`Adjusted R2` <- paste0("{\\bf ", round(metadata$`Adjusted R2`, 2), "}")
tdf$`Adjusted R2`[is.na(metadata$`Adjusted R2`)] <- as.character(round(metadata2$`Adjusted R2`[is.na(metadata$`Adjusted R2`)], 2))

tdf$`Within R2` <- paste0("{\\bf ", round(metadata$`Within R2`, 3), "}")
tdf$`Within R2`[is.na(metadata$`Within R2`)] <- as.character(round(metadata2$`Within R2`[is.na(metadata$`Within R2`)], 3))

tdf2 <- tdf %>% arrange(Paper, Name)
names(tdf2)[1:2] <- c("Paper Panel", "Estimate Name")
print(xtable(tdf2), include.rownames=F, sanitize.text.function=function(x) gsub("_", "\\_", gsub("&", "\\&", x, fixed=T), fixed=T))

mean(!is.na(metadata$`Total R2`))
mean(!is.na(metadata$`Adjusted R2`))
mean(!is.na(metadata$`Within R2`))

mean(!is.na(metadata$`Total R2`) | !is.na(metadata$`Adjusted R2`) | !is.na(metadata$`Within R2`))
