## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(PBSmapping)

persist <- "0.08"
source("src/lib/utils2.R")

load.solowdata()
solowsum <- load.solowsum(persist)

df.gdp2.last <- df.gdp2 %>% group_by(`Country Code`) %>%
    dplyr::summarize(GDP.Year=ifelse(any(!is.na(GDP.2015)), Year[tail(which(!is.na(GDP.2015)), 1)], NA),
                     GDP.2015=ifelse(any(!is.na(GDP.2015)), GDP.2015[tail(which(!is.na(GDP.2015)), 1)], NA))

allyr <- results2 %>%
    left_join(slr2, by=c('mc', 'Year'='year', 'ISO')) %>%
    left_join(tradeloss, by=c('mc', 'Year'='year', 'ISO')) %>%
    left_join(tradeloss.global, by=c('Year'='year'), suffix=c('', '.global'))
allyr$tradeloss[is.na(allyr$tradeloss)] <- allyr$tradeloss.global[is.na(allyr$tradeloss)]
allyr$tradeloss[is.na(allyr$tradeloss)] <- 0
allyr$slrloss[is.na(allyr$slrloss)] <- 0

allyr$product.true <- NA
allyr$product.nocc <- NA
allyr$allcap.true <- NA
allyr$allcap.nocc <- NA
allyr$rencap.true <- NA
allyr$rencap.nocc <- NA
allyr$rencap.ccpc <- NA

for (mcii in 1:30) {
    load.solowdata.mc(mcii)

    for (iso in unique(allyr$ISO)) {
        if (!any(solowsum$ISO == iso & solowsum$mc == mcii))
            next
        print(c(mcii, iso))

        stan.data <- make.stan.data(iso)

        load(paste0("data/solow-", persist, "/v4-", iso, "-", mcii, ".RData"))

        solowout <- model.solow(la, stan.data, F, rencaptrue=la$rencap_model)

        denom <- df2$denom[df2$ISO == iso][1]
        product.true <- colMeans(la$product) * denom
        product.nocc <- colMeans(solowout$product) * denom

        allcap.true <- (colMeans(la$rencap_model) + colMeans(la$procap_model)) * denom
        allcap.nocc <- (colMeans(solowout$rencap_model) + colMeans(solowout$procap_model)) * denom
        rencap.true <- colMeans(la$rencap_model) * denom
        rencap.nocc <- colMeans(solowout$rencap_model) * denom

        allyr$product.true[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- product.true
        allyr$product.nocc[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- product.nocc
        allyr$allcap.true[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- allcap.true[2:length(allcap.true)]
        allyr$allcap.nocc[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- allcap.nocc[2:length(allcap.true)]
        allyr$rencap.true[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- rencap.true[2:length(rencap.true)]
        allyr$rencap.nocc[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- rencap.nocc[2:length(rencap.true)]

        solowout2 <- model.solow(la, stan.data, "prodonly")
        rencap.ccpc <- colMeans(solowout2$rencap_model) * denom
        allyr$rencap.ccpc[allyr$mc == mcii & allyr$Year %in% 1961:2023 & allyr$ISO == iso] <- rencap.ccpc[2:length(rencap.true)]
    }
}

allyr$product.chg <- 1 - allyr$product.nocc / allyr$product.true
allyr$rencap.chg.ccpc <- 1 - allyr$rencap.ccpc / allyr$rencap.true

solowsum2 <- solowsum %>% group_by(ISO) %>% mutate(weight.ess=ess / sum(ess), lp.adj=ifelse(lp > 1, lp, exp(lp - 1)), weight.lp=lp.adj / sum(lp.adj), weight=weight.ess * weight.lp)
solowsum3 <- solowsum2[!duplicated(paste(solowsum2$ISO, solowsum2$mc)),]

wtd.median <- function(xx, weights=NULL, normwt=F) {
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

allyr.ww <- allyr %>% left_join(solowsum3, by=c('ISO', 'mc'), suffix=c('', '.solowsum')) %>%
    filter(is.na(weight) | weight > 1e-9) %>% group_by(ISO, Year) %>%
    mutate(weight.norm=case_when(
               all(is.na(weight)) ~ 1/length(weight),
               is.na(weight) ~ 0,
               TRUE ~ weight / sum(weight, na.rm=T)))

save(allyr.ww, file="data/allyr-ww.RData")
