library(readxl)

metadata <- read_xlsx("data/Current Losses Estimate Metadata.xlsx")
metadata <- subset(metadata, !is.na(Paper) & Include == "Included")

metadata$Name[is.na(metadata$Name)] <- "NA"
metadata$Dependent[is.na(metadata$Dependent)] <- "NA"
metadata$`Weather weight`[is.na(metadata$`Weather weight`)] <- "NA"
metadata$`Weather weight`[grep("Pop.", metadata$`Weather weight`)] <- "Pop. weight"
metadata$`Rich/Poor`[is.na(metadata$`Rich/Poor`)] <- "NA"
metadata$`Rich/Poor`[metadata$`Rich/Poor` == "Project poor only"] <- "Subsetted"
metadata$Temp[is.na(metadata$Temp)] <- "NA" # None, so doesn't matter
metadata$Prec.[is.na(metadata$Prec.)] <- "No"
metadata$`Year FE`[is.na(metadata$`Year FE`)] <- "NA"
metadata$`Trends`[is.na(metadata$`Trends`)] <- "NA"
metadata$`Trends`[metadata$`Trends` %in% c("Implicit linear by region", "Linear by Unit", "By Country", "Linear, By Country")] <- "Linear, by Unit"
metadata$`Trends`[metadata$`Trends` %in% c("Quad, By Country", "Quad by Unit")] <- "Quad, by Unit"
metadata$`Other FE`[is.na(metadata$`Other FE`)] <- "NA"
metadata$`Other Controls`[is.na(metadata$`Other Controls`)] <- "NA"
metadata$`Growth Lags`[is.na(metadata$`Growth Lags`)] <- "NA"
metadata$`Dataset`[is.na(metadata$`Dataset`)] <- "NA"
metadata$`Year Coverage`[is.na(metadata$`Year Coverage`)] <- "NA"
metadata$last.year <- sapply(metadata$`Year Coverage`, function(yys) as.numeric(strsplit(yys, " ?- ?")[[1]][2]))
metadata$first.year <- sapply(metadata$`Year Coverage`, function(yys) as.numeric(strsplit(yys, " ?- ?")[[1]][1]))
metadata$first.year[is.na(metadata$first.year)] <- 1950 # Varying 1901
metadata$first.year[metadata$`Year Coverage` == "1990, 1995, 2000, 2005"] <- 1990
metadata$last.year[metadata$`Year Coverage` == "1990, 1995, 2000, 2005"] <- 2005
metadata$`Climate`[is.na(metadata$`Climate`)] <- "NA"

metadata$Q.Weather <- 1 * (metadata$`Weather weight` == "Pop. weight")
metadata$Q.Poverty <- ifelse(metadata$`Rich/Poor` == "Interact", 0.5,
                      ifelse(metadata$`Rich/Poor` == "Subsetted", 1.0, 0.))

metadata$Q.Temp <- ifelse(metadata$Temp == "VarT, DT, LDT, DT:T, LDT:LT", 1 - ((1 - .5)*(1 - .25)*(1 - .5)*(1 - .25)),
                   ifelse(metadata$Temp %in% c("DT, LDT, DT:T, LDT:T, T", "DT, LDT, DT:T, LDT:T, LT"), 1 - ((1 - .25)*(1 - .5)*(1 - .25)*(1 - .5)),
                   ifelse(metadata$Temp %in% c("DT, LDT, DT:T, LDT:T, T, T2", "DT, LDT, DT:T, LDT:T, LT, LT2"), 1 - ((1 - .25)*(1 - .5)*(1 - .25)*(1 - .5)*(1 - .5)),
                   ifelse(metadata$Temp == "DT, LDT", 1 - (1 - .25),
                   ifelse(metadata$Temp == "1 Lag", 1 - (1 - 0.25),
                   ifelse(metadata$Temp == "5 Lags", 1 - (1 - 0.25)^4,
                   ifelse(metadata$Temp == "Tx, Tx T, T, Tvar", 1 - (1 - 0.5)^3,
                   ifelse(metadata$Temp == "Quad, 1 Lag", 1 - (1 - .5)*(1 - .25),
                   ifelse(metadata$Temp == "Linear by country, Lag by country", 1 - (1 - .5)^2 * (1 - .25)^2,
                   ifelse(metadata$Temp %in% c("Quad", "Interacted with average", "Linear Spline", "LT, DT"), .5,
		   ifelse(metadata$Temp == "Cubic", 1 - .5^2,
                   ifelse(metadata$Temp == "10 Lags", 1 - (1 - 0.25)^9,
		   ifelse(metadata$Temp == "T1, LT1, T2, LT2", 1 - .5*(.75^2),
                   ifelse(metadata$Temp %in% c("Linear", "Z-score", "FD", "Average 1986-2000 -  Average 1970-1985", "Symmetric Spline", "Deviations"), 0., NA)))))))))))
metadata$Q.Prec <- ifelse(metadata$Prec. %in% c("DP, LDP, DP:P, LDP:P, P", "DP, LDP, DP:P, LDP:P, LP"), 1 - ((1 - .25)*(1 - .5)*(1 - .25)*(1 - .5)),
                   ifelse(metadata$Prec. %in% c("DP, LDP, DP:P, LDP:P, P, P2", "DP, LDP, DP:P, LDP:P, LP, LP2"), 1 - ((1 - .25)*(1 - .5)*(1 - .25)*(1 - .5)*(1 - .5)),
                   ifelse(metadata$Prec. == "Indicatators", 1 - (1 - 0.5)*(1 - 0.5)^2*(1 - 0.5)^2*(1 - 0.5)^2,
                   ifelse(metadata$Prec. == "5 Lags", 1 - (1 - 0.25)^4,
                   ifelse(metadata$Prec. == "Quad, 1 Lag", 1 - (1 - .5)*(1 - .25),
                   ifelse(metadata$Prec. == "Linear by country, Lag by country", 1 - (1 - .5)^2 * (1 - .25)^2,
                   ifelse(metadata$Prec. %in% c("Quad x 2", "Quad + Access x 2"), 1 - (1 - 0.5)^3,
                   ifelse(metadata$Prec. == "Quad x 2 + Access x 4", 1 - (1 - 0.5)^5,
                   ifelse(metadata$Prec. %in% c("Quad", "Interacted with average", "Linear Spline", "LP, DP"), .5,
                   ifelse(metadata$Prec. %in% c("1 Lag", "DT, LDT"), 1 - (1 - 0.25),
                   ifelse(metadata$Prec. == "10 Lags", 1 - (1 - 0.25)^9,
                   ifelse(metadata$Prec. %in% c("Linear", "Z-score", "FD", "Average 1986-2000 -  Average 1970-1985", "Symmetric Spline"), 0,
                   ifelse(metadata$Prec. %in% c("NA", "No"), -1, NA)))))))))
metadata$Q.YearFE <- ifelse(metadata$`Year FE` == "By Region", 1,
                     ifelse(metadata$`Year FE` == "By Continent", 0.75,
                     ifelse(metadata$`Year FE` == "Yes", 0.5, 0)))
metadata$Q.Trends <- ifelse(metadata$`Trends` == "Quad, by Unit", 1,
                     ifelse(metadata$`Trends` == "Linear, by Unit", 0.5,
                     ifelse(metadata$`Trends` == "Global", 0.25, 0)))
metadata$Q.OtherFE <- ifelse(metadata$`Other FE` == "IIS", 1,
                      ifelse(metadata$`Other FE` == "Poor x Year", .75,
                      ifelse(metadata$`Other FE` == "HPJ-FE", 0.5,
                      ifelse(metadata$`Other FE` == "Poor", 0.25, 0))))
metadata$Q.Control <- ifelse(metadata$`Other Controls` == "Lag GDP, Lag capital, Pesaran controls", 1,
                      ifelse(metadata$`Other Controls` == "Lag Weather, Disaster", 0.75,
                      ifelse(metadata$`Other Controls` == "Lag Weather", 0.5, 0)))
metadata$Q.GLags <- as.numeric(metadata$`Growth Lags`) / 4
metadata$Q.YearLate <- 5 / (2015 - metadata$last.year + 5)
metadata$Q.YearSpan <- (metadata$last.year - metadata$first.year) / 65

## Calculate difference in R2 due to averages for Kalkuhl & Wenz 2020 and Zhao et al. 2018
df.annual <- read.csv("data/burkeetal/GrowthClimateDataset.csv")
## df.longdiff <- subset(df.annual, year >= 1995 & year <= 2014) %>% group_by(iso, year < 2005) %>% summarize(year=mean(year), growthWDI=mean(growthWDI))
df.5year <- subset(df.annual, year > 1987 & year <= 2007) %>% group_by(iso, round(year / 5)) %>% summarize(year=mean(year), growthWDI=mean(growthWDI))
var.annual <- var(df.annual$growthWDI, na.rm=T)
## var.longdiff <- var(df.longdiff$growthWDI, na.rm=T)
var.5year <- var(df.5year$growthWDI, na.rm=T)
var.annual.feres <- var(resid(lm(growthWDI ~ iso + factor(year), data=df.annual)))
## var.longdiff.feres <- var(resid(lm(growthWDI ~ iso + factor(year), data=df.longdiff)))
var.5year.feres <- var(resid(lm(growthWDI ~ iso + factor(year), data=df.5year)))
1 - var.annual.feres / var.annual
## 1 - var.longdiff.feres / var.longdiff
1 - var.5year.feres / var.5year

## Known: 1 - (sum (y - yhatx - FE)^2 / (sum (y - ybar)^2) = 1 - Var(y - yhatx - FE) / Var(y)
## Known: Var(y), Var(y - FE), Var(y + tilde), Var(y + tilde - FE)

## var.longdiff.tilde1 <- var.annual - var.longdiff
## var.longdiff.tilde2 <- var.annual.feres - var.longdiff.feres
## var.longdiff.tilde <- (var.longdiff.tilde1 + var.longdiff.tilde2) / 2

var.5year.tilde1 <- var.annual - var.5year
var.5year.tilde2 <- var.annual.feres - var.5year.feres
var.5year.tilde <- (var.5year.tilde1 + var.5year.tilde2) / 2

## Want: 1 - (sum (y + ytilde - yhatx - FE)^2 / (sum (y + ytilde - ybar)^2) =
##   1 - Var(y + ytilde - yhatx - FE) / Var(y + ytilde) = 1 - (Var(y - yhatx - FE) + Var(ytidle)) / (Var(y) + Var(ytilde))

for (ii in which(metadata$Paper == 'Zhao et al. 2018')) {
    ## R2 = 1 - Var(y - yhatx - FE) / Var(y); Get Var(y - yhatx - FE)
    var.5year.res <- var.5year * (1 - metadata$`Total R2`[ii])
    r2.annual <- 1 - (var.5year.res + var.5year.tilde) / (var.5year + var.5year.tilde)
    metadata$`Total R2`[ii] <- r2.annual
}

## for (ii in which(metadata$Paper == 'Kalkuhl & Wenz 2020')) {
##     ## R2 = 1 - Var(y - yhatx - FE) / Var(y); Get Var(y - yhatx - FE)
##     var.longdiff.res <- var.longdiff * (1 - metadata$`Total R2`[ii])
##     r2.annual <- 1 - (var.longdiff.res + var.longdiff.tilde) / (var.longdiff + var.longdiff.tilde)
##     metadata$`Total R2`[ii] <- r2.annual
## }
