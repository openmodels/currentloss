g
setwd("~/research/currentloss/search2")

library(dplyr)

source <- "websci"

df.gemini <- read.csv(paste0(source, "-gemini.csv"))
df.openai <- read.csv(paste0(source, "-openai.csv"))
unique(df.gemini$Verdict)[!(unique(df.gemini$Verdict) %in% unique(df.openai$Verdict))]
unique(df.openai$Verdict)[!(unique(df.openai$Verdict) %in% unique(df.gemini$Verdict))]

df.gemini <- read.csv(paste0(source, "-gemini.csv")) %>% select('EID', 'Verdict', 'Comments') %>% mutate(XG=grepl("Not GDP growth", Verdict), XE=grepl("Not econometric", Verdict), XW=grepl("Not global", Verdict), XN=grepl("No new empirics", Verdict), PP=ifelse(Verdict %in% c("Unlikely", "Somewhat", "Likely"), Verdict, NA))
df.openai <- read.csv(paste0(source, "-openai.csv")) %>% select('EID', 'Verdict', 'Comments') %>% mutate(XG=grepl("Not GDP growth", Verdict), XE=grepl("Not econometric", Verdict), XW=grepl("Not global", Verdict), XN=grepl("No new empirics", Verdict), PP=ifelse(Verdict %in% c("Unlikely", "Somewhat", "Likely"), Verdict, NA))
if (source == 'scopus')
    df.gemini$EID <- 0:(nrow(df.gemini)-1)

## df.gemini2 <- df.gemini %>% filter(Verdict %in% c('Somewhat', 'Likely', 'Unlikely'))
## df.openai2 <- df.openai %>% filter(Verdict %in% c('Somewhat', 'Likely', 'Unlikely')) %>% select('EID', 'Verdict', 'Comments')

if (source == 'scopus') {
    df.source <- read.csv("scopus.csv")
} else {
    library(readxl)
    df.source <- data.frame()
    for (ii in 1:36) {
        df.source.sub <- read_excel(paste0("savedrecs", ii, ".xls"))
        df.source <- rbind(df.source, df.source.sub)
    }
    df.source$Year <- df.source$`Publication Year`
}
df.source$EID <- 0:(nrow(df.source)-1)

papers <- data.frame(leadauth=c("Dell", "Burke", "Pretis", "Burke", "Sequeira", "Henseler",
                                "Kalkuhl", "Acevedo", "Damania", "Kahn", "Kotz", "Callahan",
                                "Zhao"),
                     year=c(2012, 2015, 2018, 2018, 2018, 2019, 2020, 2020, 2020, 2021, 2022, 2022, 2018),
                     choose=c(NA, NA, 1, 1, 1, NA, 1, NA, NA, 1, 1, 1, 1))

df.source$included <- F
for (ii in 1:nrow(papers)) {
    if (!is.na(papers$choose[ii])) {
        matchauth <- substring(df.source$Authors, 1, nchar(papers$leadauth[ii])) == papers$leadauth[ii]
        matchyear <- df.source$Year == papers$year[ii]
        df.source$included[which(matchauth & matchyear)[papers$choose[ii]]] <- T
    }
}

df2 <- df.source %>% left_join(df.gemini, by='EID') %>% left_join(df.openai, by='EID', suffix=c('.gemini', '.openai')) %>% filter(!is.na(Verdict.gemini) | !is.na(Verdict.openai))
df2$XX.common <- paste0(ifelse(df2$XG.gemini & df2$XG.openai, "XG", ""),
                        ifelse(df2$XE.gemini & df2$XE.openai, "XE", ""),
                        ifelse(df2$XW.gemini & df2$XW.openai, "XW", ""),
                        ifelse(df2$XN.gemini & df2$XN.openai, "XN", ""))
df2$Verdict.common <- ifelse(!is.na(df2$PP.gemini) | !is.na(df2$PP.openai), paste(df2$PP.gemini, df2$PP.openai),
                      ifelse(df2$XX.common == "", "Disagree", df2$XX.common))
df2$AI.include <- ifelse(!is.na(df2$PP.gemini) | !is.na(df2$PP.openai), T,
                  ifelse(df2$XX.common == "", T, F))
df3 <- subset(df2, included | AI.include)
write.csv(df3, paste0("further-consideration-", source, ".csv"), row.names=F)



