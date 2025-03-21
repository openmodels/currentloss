if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    if (name == "Main spec.") {
        beta <- c(0.00928, -0.000453)
        se <- c(0.00418, 0.000155)
    } else if (name == "Poor vs. Rich") {
        ## T, Txpoor, T2, T2xpoor
        beta <- c(0.00828, -0.0209, -0.000340, -7.41e-5)
        se <- c(0.00387, 0.0351, 0.000144, 0.000796)
    }

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    poors <- get.poors(1980, cutoff=0.2)

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        coeffs[mcii, ]
    }

    if (name == "Main spec.") {
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            (subera5$t2m - 273.15) * coeffs[1] + (subera5$t2m - 273.15)^2 * coeffs[2]
        }
    } else if (name == "Poor vs. Rich") {
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            (subera5$t2m - 273.15) *
                (coeffs[1] + coeffs[2] * (subera5$ISO %in% poors)) +
                (subera5$t2m - 273.15)^2 *
                (coeffs[3] + coeffs[4] * (subera5$ISO %in% poors))
        }
    }

    list(setup=setup, simulate=simulate)
}

if (F) {
    funcs <- get.funcs('Main spec.')
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)

    funcs <- get.funcs('Poor vs. Rich')
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
