if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

    library(MASS)

    source("driver.R")
    source("utils.R")
}

get.funcs <- function(name) {
    ## L.growth temp temp2 vartemp maxtemp mintemp
    if (name == "M1") {
        beta <- c(0.1243, 0.0163, -0.0005, 0.0002, -0.0007, -0.0008)
        se <- c(0.0121, 0.0039, 0.0001, 0.0002, 0.0013, 0.0009)
    } else if (name == "M2") {
        beta <- c(0.1604, 0.0115, -0.0004, 0.0002, -0.0001, -0.00005)
        se <- c(0.0099, 0.0029, 0.0001, 0.0001, 0.001, 0.0007)
    } else if (name == "M3") {
        beta <- c(0.1598, 0.0083, -0.0004)
        se <- c(0.0099, 0.0023, 0.0001)
    }

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    subera5.lags <- list()

    setup <- function(mcii) {
        subera5.lags <<- list()
        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        subera5.lags[[as.character(year)]] <<- subera5

        if (length(subera5.lags) < 2)
            subera5$t2m * NA
        else {
            tas <- subera5$t2m - 273.15
            tas2 <- tas^2

            totals <- tas * coeffs[2] + tas2 * coeffs[3]
            if (length(coeffs) > 3) {
                vartemp <- subera5$t2mavgvar
                maxtemp <- subera5$t2mavgmax - 273.15
                mintemp <- subera5$t2mavgmin - 273.15

                totals <- totals + vartemp * coeffs[4] + maxtemp * coeffs[5] + mintemp * coeffs[6]
            }
            if (!contemp.only) {
                totals <- totals + subera5.lags[[as.character(year - 1)]]$growth * coeffs[1]
            }
            totals
        }
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("M1")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
