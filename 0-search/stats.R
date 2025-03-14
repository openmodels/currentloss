setwd("~/research/currentloss/search2")

library(dplyr)

alldf <- data.frame()
for (source in c('scopus', 'websci')) {
    source("src/0-search/standardize.R")

    if (source == 'websci') {
        df3 <- df2[, c('DOI', 'Authors', 'Article Title', 'Source Title', 'Publication Year', 'included', 'XG.gemini', 'XE.gemini', 'XW.gemini', 'XN.gemini', 'PP.gemini', 'XG.openai', 'XE.openai', 'XW.openai', 'XN.openai', 'PP.openai', 'XX.common', 'Verdict.common', 'AI.include')]
        names(df3)[3:5] <- c('Title', 'Journal', 'Year')
    } else {
        df3 <- df2[, c('DOI', 'Authors', 'Title', 'Source.title', 'Year', 'included', 'XG.gemini', 'XE.gemini', 'XW.gemini', 'XN.gemini', 'PP.gemini', 'XG.openai', 'XE.openai', 'XW.openai', 'XN.openai', 'PP.openai', 'XX.common', 'Verdict.common', 'AI.include')]
        names(df3)[4] <- 'Journal'
    }
    df3$source <- source

    df2$XG.common <- ifelse(!is.na(df2$XG.gemini) & !is.na(df2$XG.openai), df2$XG.gemini & df2$XG.openai, NA)
    df2$XE.common <- ifelse(!is.na(df2$XE.gemini) & !is.na(df2$XE.openai), df2$XE.gemini & df2$XE.openai, NA)
    df2$XW.common <- ifelse(!is.na(df2$XW.gemini) & !is.na(df2$XW.openai), df2$XW.gemini & df2$XW.openai, NA)
    df2$XN.common <- ifelse(!is.na(df2$XN.gemini) & !is.na(df2$XN.openai), df2$XN.gemini & df2$XN.openai, NA)

    alldf <- rbind(alldf, df3)
}

table(alldf$source)
length(unique(alldf$DOI))
