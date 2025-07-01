persist <- "0.08"
year <- 1991
mcii <- 21

library(dplyr)
source("lib/loadutils.R")
source("3-othereffects/trade-io.R")

## Get all GDPs (for SLR fraction calc)
df.gdp3 <- load.gdp3()
slr2 <- load.slr2(df.gdp3)

load(paste0("../data/mcrfres-", persist, ".RData"))

results2 <- results %>% group_by(ISO, mc) %>%
    mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30]) %>%
    left_join(slr2, by=c('ISO', 'Year'='year', 'mc'))
results2$slrloss[is.na(results2$slrloss)] <- 0

results2.year <- subset(results2, Year == year & mc == mcii)

isos <- results2.year$ISO
dimpact <- results2.year$totimpact - results2.year$slrloss

domar.loss <- calc.domar.distribute.method(year, isos, dimpact)
fd.method <- calc.final.demand.method(year, isos, dimpact)
ll.method <- calc.leontief.method(year, isos, dimpact)

toplot <- data.frame(ISO=isos, domar.loss, fd.method, ll.method)

library(GGally)

ggpairs(toplot, columns = 2:4, axisLabels = "show", columnLabels=c('Distributed Domar', 'Final Demand', 'Leontief Inverse'))
ggsave("../results/trade-compare.pdf", width=8.5, height=5)
