## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

persist <- "0.08"
year <- 2015
mcii <- 1

library(dplyr)
source("src/lib/loadutils.R")
source("src/3-othereffects/trade-io.R")

## Get all GDPs (for SLR fraction calc)
df.gdp3 <- load.gdp3()
slr2 <- load.slr2(df.gdp3)

load(paste0("data/mcrfres-", persist, ".RData"))

results2 <- results %>% group_by(ISO, mc) %>%
    mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30]) %>%
    left_join(slr2, by=c('ISO', 'Year'='year', 'mc'))
results2$slrloss[is.na(results2$slrloss)] <- 0

results2.year <- subset(results2, Year == year & mc == mcii)

domar.loss <- calc.domar.loss(year, results2.year$ISO, results2.year$totimpact - results2.year$slrloss)
fd.method <- calc.final.demand.method(year, results2.year$ISO, results2.year$totimpact - results2.year$slrloss)
ll.method <- calc.leontief.method(year, results2.year$ISO, results2.year$totimpact - results2.year$slrloss)

toplot <- data.frame(ISO=results2.year$ISO, domar.loss, fd.method, ll.method)
