if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

    source("src/2-project/driver.R")
    source("src/2-project/utils.R")
}

zhaodata <- read.csv("papers/models/tabula-Zhao et al. 2018.csv", nrows=14,
                     colClasses=c("character", rep("numeric", 7)))
names(zhaodata)[1] <- "pred"
excludecold <- read.csv("papers/models/tabula-Zhao et al. 2018.csv", skip=20,
                        col.names=c('X', paste0('X', 1:7)))

get.funcs <- function(name) {
    poors <- get.poors(1970)

    col <- paste0("X", substring(name, nchar(name)))

    prednames <- zhaodata$pred[!is.na(zhaodata[, col]) & zhaodata$pred != ""]
    values <- zhaodata[!is.na(zhaodata[, col]), col]
    beta <- values[seq(1, length(values), by=2)] / 100 # in %
    se <- values[seq(2, length(values), by=2)] / 100

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    subera5.lags <- list()
    setup <- function(mcii) {
        subera5.lags <<- list()
        if (is.null(mcii))
            return(beta)
        coeffs[mcii, ]
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        subera5.lags[[as.character(year)]] <<- subera5

        totals <- 0 * subera5$t2m
        for (kk in 1:length(prednames)) {
            if (prednames[kk] == "Temperature×Poor")
                totals <- totals + (subera5$t2m - 273.15) * coeffs[kk] * (subera5$ISO %in% poors)
            else if (prednames[kk] == "Temperature2×Poor")
                totals <- totals + (subera5$t2m - 273.15)^2 * coeffs[kk] * (subera5$ISO %in% poors)
            else if (prednames[kk] == "Temperature3×Poor")
                totals <- totals + (subera5$t2m - 273.15)^3 * coeffs[kk] * (subera5$ISO %in% poors)
            else if (prednames[kk] == "Temperature×Rich")
                totals <- totals + (subera5$t2m - 273.15) * coeffs[kk] * !(subera5$ISO %in% poors)
            else if (prednames[kk] == "Temperature2×Rich")
                totals <- totals + (subera5$t2m - 273.15)^2 * coeffs[kk] * !(subera5$ISO %in% poors)
            else if (prednames[kk] == "Temperature3×Rich")
                totals <- totals + (subera5$t2m - 273.15)^3 * coeffs[kk] * !(subera5$ISO %in% poors)
            else if (prednames[kk] == "gt-1") {
                if (length(subera5.lags) == 0)
                    totals <- totals * NA
                else
                    subera5.lags[[as.character(year - 1)]]$growth * coeffs[kk]
            }
        }

        totals
    }

    list(setup=setup, simulate=simulate)
}

if (F) {
    funcs <- get.funcs('col1')
    oneres <- project.single(funcs$setup, funcs$simulate, adm.level=1)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
