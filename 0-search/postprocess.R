## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)

source <- "websci"

source("src/0-search/standardize.R")

df2 <- df2 %>% filter(!is.na(Verdict.gemini) | !is.na(Verdict.openai))
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



