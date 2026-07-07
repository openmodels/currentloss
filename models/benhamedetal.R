if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    ## temp, Lgrowth
    if (name == 'Table 4, Full Sample, Contiguity') {
        beta <- c(-0.083, 0.097)
        se <- c(0.284, 0.)
    } else if (name == 'Table 4, Full Sample, Inv. Dist.') {
        beta <- c(-0.081, 0.089)
        se <- c(0.292, 0.0)
    } else if (name == 'Table 4, LMI/HI, Contiguity') {
        beta <- c(-0.152, 0.041, 0.047, 0.267)
        se <- c(0.000, 0.063, 0.728, 0.0)
    } else if (name == 'Table 4, LMI/HI, Inv. Dist.') {
        beta <- c(-0.144, 0.032, 0.048, 0.268)
        se <- c(0.0, 0.141, 0.716, 0.0)
    } else {
        return(NULL)
    }

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    poors <- get.poors(1970)

    setup <- function(mcii) {
        subera5.lag <<- NULL

        if (is.null(mcii))
            return(beta / 100)
        coeffs[mcii, ] / 100
    }

    subera5.lag <- NULL
    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        if (is.null(subera5.lag)) {
            dimpact <- subera5$t2m * NA
        } else {
            if (length(coeffs) == 2) {
                if (contemp.only) {
                    dimpact <- subera5$t2m * coeffs[1]
                } else {
                    dimpact <- subera5$t2m * coeffs[1] + subera5.lag$growth * coeffs[2]
                }
            } else {
                if (contemp.only) {
                    dimpact <- (subera5$ISO %in% poors) * subera5$t2m * coeffs[1] +
                        (!(subera5$ISO %in% poors)) * subera5$t2m * coeffs[3]
                } else {
                     dimpact <- (subera5$ISO %in% poors) * (subera5$t2m * coeffs[1] + subera5.lag$growth * coeffs[2]) +
                        (!(subera5$ISO %in% poors)) * (subera5$t2m * coeffs[3] + subera5.lag$growth * coeffs[4])
                }
            }
        }

        subera5.lag <<- subera5

        dimpact
    }

    list(setup=setup, simulate=simulate)
}

if (F) {
    funcs <- get.funcs('Table 4, LMI/HI, Contiguity')
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
