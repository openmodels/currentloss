## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(Hmisc)
library(PBSmapping)
library(parallel)

do.parallel <- T
do.redo <- T
persist <- "0.46"
trade.method <- 'dd-mcr2all'
solow.config <- '' #'' #'-prodonly' #'-additive'
solow.data.dir <- "/mnt/LabShare/Current Losses v2"

if (do.parallel) {
    cl <- makeCluster(detectCores())
    clusterEvalQ(cl, {
        library(Hmisc)
        library(PBSmapping)
    })
    clusterExport(cl, "solow.data.dir")
    mylapply <- function(xx, func) {
        parLapply(cl, xx, func)
    }
} else {
    mylapply <- lapply
}

configs <- list()
for (persist in c("0.46", "0", "0.31", "0.78")) {
    for (trade.method.prefix in c('dd', 'fd', 'li')) {
        for (trade.method in paste0(trade.method.prefix, "-mcr2all")) { #c("", "-mcpaperall", "-mcr2all"))) {
            for (solow.config in c('', '-prodonly', '-noadd', '-additive')) {
                if (!file.exists(paste0(solow.data.dir, "/solow-", persist, "-", trade.method, solow.config)))
                    next
                if (!do.redo && file.exists(paste0("data/allyr-ww-", persist, "-", trade.method, solow.config, ".RData")))
                    next
                configs[[length(configs)+1]] <- c(persist, trade.method, solow.config)
            }
        }
    }
}

print(configs)

mylapply(configs, function(config) {
    persist <<- config[1]
    trade.method <<- config[2]
    solow.config <<- config[3]

    print(c(persist, trade.method, solow.config))

source("src/lib/utils2.R")

load.solowdata()
solowsum <- load.solowsum(persist, trade.method, solow.config, solow.data.dir=solow.data.dir)

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

failures <- c()

for (mcii in 1:30) {
    load.solowdata.mc(mcii)

    for (iso in unique(allyr$ISO)) {
        if (!any(solowsum$ISO == iso & solowsum$mc == mcii))
            next
        print(c(mcii, iso))

        stan.data <- make.stan.data(iso)

        success <- tryCatch({
            load(paste0(solow.data.dir, "/solow-", persist, "-", trade.method, solow.config, "/v4-", iso, "-", mcii, ".RData"))
            T
        }, error=function(e) {
            F
        })
        if (!success) {
            failures <- c(failures, paste(mcii, iso))
            next
        }

        if (is.null(la$cumulpart) && solow.config == '-noadd')
            la$cumulpart <- rep(1, stan.data$T)

        denom <- df2$denom[df2$ISO == iso][1]
        if (solow.config != '-prodonly') {
            solowout <- model.solow(la, stan.data, F, rencaptrue=la$rencap_model)

            product.true <- colMeans(la$product) * denom
            product.nocc <- colMeans(solowout$product) * denom

            allcap.true <- (colMeans(la$rencap_model) + colMeans(la$procap_model)) * denom
            allcap.nocc <- (colMeans(solowout$rencap_model) + colMeans(solowout$procap_model)) * denom
            rencap.true <- colMeans(la$rencap_model) * denom
            rencap.nocc <- colMeans(solowout$rencap_model) * denom

            allyr$product.true[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- product.true
            allyr$product.nocc[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- product.nocc
            allyr$allcap.true[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- allcap.true[2:length(allcap.true)]
            allyr$allcap.nocc[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- allcap.nocc[2:length(allcap.true)]
            allyr$rencap.true[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- rencap.true[2:length(rencap.true)]
            allyr$rencap.nocc[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- rencap.nocc[2:length(rencap.true)]

            solowout2 <- model.solow(la, stan.data, "prodonly")
            rencap.ccpc <- colMeans(solowout2$rencap_model) * denom
            allyr$rencap.ccpc[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- rencap.ccpc[2:length(rencap.true)]
        } else {
            solowout <- model.solow.prodonly(la, stan.data, F)

            product.true <- colMeans(la$product) * denom
            product.nocc <- colMeans(solowout$product) * denom

            allcap.true <- colMeans(la$procap_model) * denom
            allcap.nocc <- colMeans(solowout$procap_model) * denom

            allyr$product.true[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- product.true
            allyr$product.nocc[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- product.nocc
            allyr$allcap.true[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- allcap.true[2:length(allcap.true)]
            allyr$allcap.nocc[allyr$mc == mcii & allyr$Year %in% 1960:2022 & allyr$ISO == iso] <- allcap.nocc[2:length(allcap.true)]
        }
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

save(allyr.ww, file=paste0("data/allyr-ww-", persist, "-", trade.method, solow.config, ".RData"))

})
