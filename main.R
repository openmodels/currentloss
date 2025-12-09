install.packages("sf")
library(sf)

print("Generating figure 1 content")
source("2-project/figure1.R")

print("Generating timeseries plots")
source("4-figures/timeseries.R")

print("Generating maps and bar plots")
source("4-figures/figures.R")

