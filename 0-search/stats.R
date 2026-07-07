setwd("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses/")

library(dplyr)

alldf <- data.frame()
for (source in c('scopus', 'websci')) {
    source("src/0-search/standardize.R")

    if (source == 'websci') {
        df3 <- df2[, c('DOI', 'Authors', 'Article Title', 'Source Title', 'Publication Year', 'included', 'XG.gemini', 'XE.gemini', 'XW.gemini', 'XN.gemini', 'PP.gemini', 'XG.openai', 'XE.openai', 'XW.openai', 'XN.openai', 'PP.openai')]
        names(df3)[3:5] <- c('Title', 'Journal', 'Year')
    } else {
        df3 <- df2[, c('DOI', 'Authors', 'Title', 'Source.title', 'Year', 'included', 'XG.gemini', 'XE.gemini', 'XW.gemini', 'XN.gemini', 'PP.gemini', 'XG.openai', 'XE.openai', 'XW.openai', 'XN.openai', 'PP.openai')]
        names(df3)[4] <- 'Journal'
    }
    df3$source <- source

    df3$XG.common <- ifelse(!is.na(df3$XG.gemini) & !is.na(df3$XG.openai), df3$XG.gemini & df3$XG.openai, NA)
    df3$XE.common <- ifelse(!is.na(df3$XE.gemini) & !is.na(df3$XE.openai), df3$XE.gemini & df3$XE.openai, NA)
    df3$XW.common <- ifelse(!is.na(df3$XW.gemini) & !is.na(df3$XW.openai), df3$XW.gemini & df3$XW.openai, NA)
    df3$XN.common <- ifelse(!is.na(df3$XN.gemini) & !is.na(df3$XN.openai), df3$XN.gemini & df3$XN.openai, NA)

    alldf <- rbind(alldf, df3)
}

table(alldf$source)
length(unique(alldf$DOI))

sum(alldf$XG.common, na.rm=T)
sum(alldf$XW.common, na.rm=T)
sum(alldf$XE.common, na.rm=T)
sum(alldf$XN.common, na.rm=T)

sum(!alldf$XG.common & !alldf$XE.common & !alldf$XW.common & !alldf$XN.common, na.rm=T)

allfurther <- data.frame()
for (source in c('scopus', 'websci')) {
    df <- read.csv(paste0("data/search/further-consideration-", source, ".csv"))
    allfurther <- rbind(allfurther, df[, c('DOI', 'included', 'Final.Verdict', 'Final.include')])
}

allfurther$Final.Verdict[allfurther$Final.include] <- "include"
table(allfurther$Final.Verdict)

subset(allfurther, !included & Final.include)

## Combine with new systematic review

library(readxl)
sysrev <- read_excel("data/search/Current Losses Review Process.xlsx")

## Total DOIs

alldoi <- unique(c(alldf$DOI, sysrev$DOI))
