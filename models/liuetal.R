## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    if (name == "Table S1, Lag 0") {
        beta <- c(0.0073, -2.31e-4)
        stars <- c(3, 3)
    } else if (name == "Table S1, Lag 1") {
        beta <- c(0.0123, -0.0093, -3.79e-4, 2.78e-4) # T, LT, T2, LT2
        stars <- rep(3, 4)
    } else {
        ERROR
    }
    se <- abs(beta) / qnorm(.99) # always 3 stars, so 99% confidence level

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    subera5.lag <- NULL

    setup <- function(mcii) {
        subera5.lag <<- NULL

        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        if (length(coeffs) == 2)
            dimpact <- (subera5$t2m - 273.15) * coeffs[1] + (subera5$t2m - 273.15)^2 * coeffs[2]
        else if (length(coeffs) == 4) {
            dimpact <- (subera5$t2m - 273.15) * coeffs[1] + (subera5$t2m - 273.15)^2 * coeffs[3]
            if (!contemp.only) {
                if (is.null(subera5.lag)) {
                    dimpact <- NA
                } else {
                    dimpact <- dimpact + (subera5.lag$t2m - 273.15) * coeffs[2] + (subera5.lag$t2m - 273.15)^2 * coeffs[4]
                }
            }
        }

        subera5.lag <<- subera5

        dimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Table S1, Lag 1")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
}
