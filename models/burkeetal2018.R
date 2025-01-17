if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    beta <- c(0.0127, -0.0005)
    se <- c(0.0032, 0.0001)

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        coeffs[mcii, ]
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        (subera5$t2m - 273.15) * coeffs[1] + (subera5$t2m - 273.15)^2 * coeffs[2]
    }

    list(setup=setup, simulate=simulate)
}

if (F) {
    funcs <- get.funcs('Main spec.')
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
