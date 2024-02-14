## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    ## VarT, DT, LDT, DT:T, LDT:LT
    if (name == "Main") {
        beta <- c(-5.8e-2, 9.6e-4, -2.3e-3, -1.1e-3, -6.5e-4)
        se <- c(4.5e-3, 2e-3, 2.4e-3, 2e-4, 2.1e-4)
    } else if (name == "Pop-weighted") {
        beta <- c(-5.8e-2, 3.6e-4, -2e-3, -1e-3, -7.1e-4)
        se <- c(4.5e-3, 2e-3, 2.4e-3, 2e-4, 2.1e-4)
    } else if (name == "With Linear Trends") {
        beta <- c(-6e-2, 1.7e-3, -9.4e-4, -1e-3, -7.7e-4)
        se <- c(4.5e-3, 2.3e-3, 2.3e-3, 1.9e-4, 1.9e-4)
    } else if (name == "With Quad Trends") {
        beta <- c(-4.9e-2, 4.5e-3, 2.2e-3, -1.1e-3, -9.1e-4)
        se <- c(4.6e-3, 2.4e-3, 2.4e-3, 1.9e-4, 1.9e-4)
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
        if (contemp.only || length(subera5.lags) < 3) {
            vartimpact <- 0
            dimpact <- subera5$t2m * NA
            cumimpact <- subera5$t2m * 0
        } else {
            vartimpact <- subera5$t2mvaravg * coeffs[1]
            dimpact <-
                (subera5$t2m - subera5.lags[[as.character(year - 1)]]$t2m) * coeffs[2] +
                (subera5.lags[[as.character(year - 1)]]$t2m - subera5.lags[[as.character(year - 2)]]$t2m) * coeffs[3] +
                (subera5$t2m - subera5.lags[[as.character(year - 1)]]$t2m) * (subera5$t2m - 273.15) * coeffs[4] +
                (subera5.lags[[as.character(year - 1)]]$t2m - subera5.lags[[as.character(year - 2)]]$t2m) * (subera5.lags[[as.character(year - 1)]]$t2m - 273.15) * coeffs[5]
            cumimpact <- subera5.lags[[as.character(year - 1)]]$cumimpact + dimpact
        }

        subera5$cumimpact <- cumimpact
        subera5.lags[[as.character(year)]] <<- subera5

        cumimpact + vartimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs('Main')
    oneres <- project.single(funcs$setup, funcs$simulate, adm.level=1)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
}
