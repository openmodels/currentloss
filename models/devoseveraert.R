## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    ## y_t-1, rich temp, rich temp_t-1, poor temp, poor temp_t-1
    if (name == "Table 5, FE") {
        beta1 <- c(0.17, 0.17, -0.30, -2.08, 0.69)
        se1 <- c(0.07, 0.45, 0.33, 0.57, 0.69)
        beta2 <- c(0.18, 0.13, 0.44, -1.26, 0.83)
        se2 <- c(0.08, 0.20, 0.22, 0.45, 0.33)
    } else if (name == "Table 5, CCEPbc") {
        beta1 <- c(0.24, 0.48, -0.39, -1.93, 1.84)
        se1 <- c(0.08, 0.52, 0.54, 0.80, 0.92)
        beta2 <- c(0.22, 0.44, 0.08, -1.24, 0.57)
        se2 <- c(0.07, 0.37, 0.32, 0.68, 0.68)
    }

    beta <- c(beta1, beta2) / 100
    se <- c(se1, se2) / 100
    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    poors <- get.poors(1970)
    subera5.lag <- NULL

    setup <- function(mcii) {
        subera5.lag <<- NULL

        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        ## Currently ignore y_t-1
        if (is.null(subera5.lag)) {
            dimpact <- NA
        } else {
            if (contemp.only) {
                if (year <= 1982) {
                    dimpact <- (((subera5$t2m - 273.15) * coeffs[2]) * !(subera5$ISO %in% poors)) +
                        (((subera5$t2m - 273.15) * coeffs[4]) * (subera5$ISO %in% poors))
                } else {
                    dimpact <- (((subera5$t2m - 273.15) * coeffs[7]) * !(subera5$ISO %in% poors)) +
                        (((subera5$t2m - 273.15) * coeffs[9]) * (subera5$ISO %in% poors))
                }
            } else {
                if (year <= 1982) {
                    dimpact <- (((subera5$t2m - 273.15) * coeffs[2] + (subera5.lag$t2m - 273.15) * coeffs[3]) * !(subera5$ISO %in% poors)) +
                        (((subera5$t2m - 273.15) * coeffs[4] + (subera5.lag$t2m - 273.15) * coeffs[5]) * (subera5$ISO %in% poors))
                } else {
                    dimpact <- (((subera5$t2m - 273.15) * coeffs[7] + (subera5.lag$t2m - 273.15) * coeffs[8]) * !(subera5$ISO %in% poors)) +
                        (((subera5$t2m - 273.15) * coeffs[9] + (subera5.lag$t2m - 273.15) * coeffs[10]) * (subera5$ISO %in% poors))
                }
            }
        }

        subera5.lag <<- subera5

        dimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Table 5, CCEPbc")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres %>% filter(ISO == 'NOR'))$dimpact)
}
