## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
source("src/lib/myPBSmapping.R")
library(ggplot2)

allres <- read.csv("data/allres.csv")
polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

byyear <- rbind(allres %>% filter(Year >= 2014 & paper != "Kotz et al. 2022") %>%
                group_by(name, paper, contemp.only, ISO) %>% summarize(dimpact=mean(dimpact, na.rm=T)),
                allres %>% filter(Year >= 2013 & paper == "Kotz et al. 2022") %>%
                group_by(name, paper, contemp.only, ISO) %>% mutate(dimpact=mean(diff(dimpact), na.rm=T))) %>%
    left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(name, paper, contemp.only) %>%
    summarize(dimpact.pop=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST[!is.na(dimpact)]))

byyear2 <- byyear %>%
    group_by(name, paper) %>% summarize(contcoef=c(dimpact.pop[contemp.only], NA)[1],
                                        allcoef=c(dimpact.pop[!contemp.only], NA)[1]) %>%
    filter(!is.na(contcoef) & !is.na(allcoef))

ggplot(byyear2, aes(contcoef, allcoef)) +
    geom_point()

ggplot(byyear2) +
    geom_segment(aes(x="Contemporaneous\nOnly", xend="All Coefficients", y=contcoef, yend=allcoef)) +
    scale_x_discrete(NULL, expand=c(.3, 0)) + ylab("Instantaneous Growth Effect in 2014-2023") +
    theme_bw()
ggsave("figures/persistence-check-A.pdf", width=3.25, height=4)

summary(lm(diffcoef ~ 1, byyear2 %>% mutate(diffcoef=contcoef - allcoef)))
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.0012055  0.0008433  -1.429     0.16

table((byyear2 %>% mutate(diffcoef=contcoef - allcoef, signdiff=sign(diffcoef)))$signdiff)

## Also need to check: which use FDs
metadata <- read_xlsx("data/Current Losses Estimate Metadata.xlsx")

metadata$group <- ifelse(metadata$Temp %in% c("FD", "VarT, DT, LDT, DT:T, LDT:LT", "DT, LDT", "DT"), "FD",
                  ifelse(metadata$Temp %in% c("DT, LDT, DT:T, LDT:T, T", "DT, LDT, DT:T, LDT:T, T, T2",
                                              "DT, LDT, DT:T, LDT:T, LT", "DT, LDT, DT:T, LDT:T, LT, LT2",
                                              "DT, DT:T, T, T2", "LT, DT"), "FD & Levels",
                  ifelse(metadata$Temp %in% c("10 Lags", "1 Lag", "5 Lags", "Quad, 1 Lag", "Linear by country, Lag by country"), "Lags", "Levels")))

byyear <- rbind(allres %>% filter(Year >= 2014 & preferred & paper != "Kotz et al. 2022") %>%
                group_by(name, paper, ISO) %>% summarize(dimpact=mean(dimpact, na.rm=T)),
                allres %>% filter(Year >= 2013 & preferred & paper == "Kotz et al. 2022") %>%
                group_by(name, paper, ISO) %>% summarize(dimpact=mean(diff(dimpact), na.rm=T))) %>%
    left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
    group_by(name, paper) %>%
    summarize(dimpact.pop=sum(dimpact * POP_EST, na.rm=T) / sum(POP_EST[!is.na(dimpact)])) %>%
    left_join(metadata, by=c('paper'='Paper', 'name'='Name'))

byyear$group <- factor(byyear$group, c("Levels", "FD", "FD & Levels", "Lags"))

ggplot(byyear, aes(group, dimpact.pop)) +
    geom_violin() +
    geom_jitter(height=0, width=.25) +
    xlab(NULL) + ylab("Instantaneous Growth Effect in 2014-2023") +
    theme_bw()
ggsave("figures/persistence-check-B.pdf", width=3.25, height=4)

summary(lm(dimpact.pop ~ group, byyear))
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)
## (Intercept)      -7.095e-03  1.709e-03  -4.152 6.29e-05 ***
## groupFD           4.223e-03  6.835e-03   0.618    0.538
## groupFD & Levels -6.521e-05  6.835e-03  -0.010    0.992
## groupLags         2.559e-03  4.093e-03   0.625    0.533

byyear$group <- factor(byyear$group, levels=c("FD", "Levels", "FD & Levels", "Lags"))
summary(lm(dimpact.pop ~ group, byyear))

## Simulations
## Is persistence or not persistence
## DGP is temp or FD
## Is regressed with temp or FD

results <- data.frame()
for (bs in 1:1000) {
    print(bs)
    df <- tibble(TT=rnorm(100, 10, 2), dTT=c(NA, diff(TT)),
                 dyy.TT=TT + rnorm(100, 0, 1),
                 dyy.dTT=dTT + rnorm(100, 0, 1),
                 dyy.TTdTT=TT + dTT + rnorm(100, 0, 1),
                 eyy.TT=c(NA, diff(dyy.TT)), # equal to dyy.TT[-1] - dyy.TT[-n()]
                 eyy.dTT=c(NA, diff(dyy.dTT)),
                 eyy.TTdTT=c(NA, diff(dyy.TTdTT)),
                 fyy.TT=dyy.TT + eyy.TT,
                 gyy.TT=c(NA, NA, dyy.TT[-1:-2] - head(dyy.TT, length(dyy.TT) - 2)),
                 hyy.TT=stats::filter(c(rep(0, 30), eyy.TT), (1 - 0.6)^(0:30), sides=1)[-1:-30])

    result <- data.frame(dyy.TT.TT=coef(lm(dyy.TT ~ TT, data=df))[2], dyy.TT.dTT=coef(lm(dyy.TT ~ dTT, data=df))[2],
                         dyy.dTT.TT=coef(lm(dyy.dTT ~ TT, data=df))[2], dyy.dTT.dTT=coef(lm(dyy.dTT ~ dTT, data=df))[2],
                         dyy.TTdTT.TT=coef(lm(dyy.TTdTT ~ TT, data=df))[2], dyy.TTdTT.dTT=coef(lm(dyy.TTdTT ~ dTT, data=df))[2],
                         dyy.TTdTT.TTdTT1=coef(lm(dyy.TTdTT ~ TT + dTT, data=df))[2], dyy.TTdTT.TTdTT2=coef(lm(dyy.TTdTT ~ TT + dTT, data=df))[3],
                         dyy.TT.TTdTT1=coef(lm(dyy.TT ~ TT + dTT, data=df))[2], dyy.TT.TTdTT2=coef(lm(dyy.TT ~ TT + dTT, data=df))[3],
                         dyy.dTT.TTdTT1=coef(lm(dyy.dTT ~ TT + dTT, data=df))[2], dyy.dTT.TTdTT2=coef(lm(dyy.dTT ~ TT + dTT, data=df))[3],

                         eyy.TT.TT=coef(lm(eyy.TT ~ TT, data=df))[2], eyy.TT.dTT=coef(lm(eyy.TT ~ dTT, data=df))[2],
                         eyy.TT.TTdTT1=coef(lm(eyy.TT ~ TT + dTT, data=df))[2], eyy.TT.TTdTT2=coef(lm(eyy.TT ~ TT + dTT, data=df))[3],

                         fyy.TT.TTdTT1=coef(lm(fyy.TT ~ TT + dTT, data=df))[2], fyy.TT.TTdTT2=coef(lm(fyy.TT ~ TT + dTT, data=df))[3],
                         gyy.TT.TTdTT1=coef(lm(gyy.TT ~ TT + dTT, data=df))[2], gyy.TT.TTdTT2=coef(lm(gyy.TT ~ TT + dTT, data=df))[3],
                         hyy.TT.TTdTT1=coef(lm(hyy.TT ~ TT + dTT, data=df))[2], hyy.TT.TTdTT2=coef(lm(hyy.TT ~ TT + dTT, data=df))[3])

    results <- rbind(results, cbind(bs=bs, result))

    ## library(modelsummary)
    ## modelsummary(list("dyy.TT ~ TT"=lm(dyy.TT ~ TT, data=df), "dyy.TT ~ dTT"=lm(dyy.TT ~ dTT, data=df),
    ##                   "dyy.dTT ~ TT"=lm(dyy.dTT ~ TT, data=df), "dyy.dTT ~ dTT"=lm(dyy.dTT ~ dTT, data=df),
    ##                   "eyy.TT ~ TT"=lm(eyy.TT ~ TT, data=df), "eyy.TT ~ dTT"=lm(eyy.TT ~ dTT, data=df),
    ##                   "eyy.dTT ~ TT"=lm(eyy.dTT ~ TT, data=df), "eyy.dTT ~ dTT"=lm(eyy.dTT ~ dTT, data=df)))

}

results2 <- cbind(persist=c(rep(T, 12), rep(F, 4), rep(NA, 2), rep(F, 2), rep(NA, 2)), # dyy=T or eyy=F
                  dgpmatch=c(T, F, F, T, F, F, T, T, T, T, T, T, # For A.B.C, is B in C?
                             F, T, T, T, T, T, F, F, F, F),
                  expected=c(1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1,
                             0, 1, 0, 1, 1, 1, 1, 0, NA, NA),
                  as.data.frame(t(apply(results[, -1], 2, function(xx) quantile(xx, c(.001, .01, .05, .5, .95, .99, .999))))),
                  mu=apply(results[, -1], 2, function(xx) mean(xx)), se=apply(results[, -1], 2, function(xx) sd(xx)))
## names(results2)[4:8] <- c('q0', 'q25', 'q50', 'q75', 'q100')
results2$pe.match <- abs(results2$`50%` - results2$expected) < .25
results2$stars <- ifelse(sign(results2$`0.1%`) == sign(results2$`99.9%`), "***",
                  ifelse(sign(results2$`1%`) == sign(results2$`99%`), "**",
                  ifelse(sign(results2$`5%`) == sign(results2$`95%`), "*", "")))

## Key question: Can growth regression with both pick up persistence vs. transience?
## dyy.TT.TTdTT and eyy.TT.TTdTT say it can.
## Notes:
## eyy.TT.TT says that transient impacts would be picked up by a dy ~ T regression.
## dyy.TT.dTT says that persistent impacts are partly picked up by a dy ~ dT regression.
## dyy.TTdTT.TT and dyy.TTdTT.dTT show that's held through if true has both transient and persistent, but only check for one.
## Including both TT and dTT is always fine in my experiments...

## ! I bet if recovers in 2 years, will be completely mis-interpretted.
## But is that the question...

## 1. Does Burke et al. imply persistence? NO! It picks up an effect when it's not there.
results2[rownames(results2) == "eyy.TT.TT",]
## 2. Conversely, does a regression against dT find the transient effect? NO! Again, finds a transient effect where there is none.
results2[rownames(results2) == "dyy.TT.dTT",]
results2[rownames(results2) == "dyy.TTdTT.dTT",]
## 3. That said, including both effectively distinguishes between these effects.
results2[rownames(results2) %in% c('fyy.TT.TTdTT1', 'fyy.TT.TTdTT2'),]
## 4. Can the regressinos that include both properly identify persistence that lasts 2 years? NO! Picks up a T effect, but it is not persistent.
results2[rownames(results2) %in% c('gyy.TT.TTdTT1', 'gyy.TT.TTdTT2'),]

## 5. OKAY, but regression with both perfectly picks up the level of my style of persistence.
## 6. But does that mean you project it as the sum and get the same thing? NO! That would include part of the persistence lasting forever, and that's not what I describe.
