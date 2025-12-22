## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    if (name == "Table 2, T2W") {
        beta <- 0.08
        sigma <- 0.32
    } else if (name == "Table 5, T2W") {
        beta <- -0.06
        sigma <- 0.1
    }

    coeffs <- rnorm(MCNUM, beta, sigma)
    subera5.lags <- list()

    setup <- function(mcii) {
        subera5.lags <<- list()

        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        dimpact <- 0
        if (name == "Table 5, T2W" && (contemp.only || length(subera5.lags) == 0)) {
            dimpact <- subera5$t2m * NA
        } else if (name == "Table 2, T2W") {
            dimpact <- dimpact + (subera5$t2m - 273.15) * coeffs
        } else if (name == "Table 5, T2W") {
            dimpact <- dimpact + (subera5.lags[[as.character(year - 1)]]$t2m - 273.15) * coeffs
        }

        subera5.lags[[as.character(year)]] <<- subera5

        dimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs('Table 2, T2W')
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
}
