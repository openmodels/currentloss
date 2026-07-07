## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    stopifnot(name %in% c("Table 1, All", "Table 1, Split")) # Identical coefficientsr

    beta <- -0.004
    se <- 0.001

    coeffs <- matrix(NA, MCNUM, 1)
    coeffs[, 1] <- rnorm(MCNUM, beta, se)

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        (subera5$t2m - 273.15) * coeffs[1]
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Table 1, All")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres %>% filter(ISO == 'NOR'))$dimpact)
}
