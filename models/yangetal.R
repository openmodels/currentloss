## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    if (name == "Table 4, FE-NLS, 1") {
        ## T
        beta <- c(-0.493)
        se <- c(0.333)
    } else if (name == "Table 4, FE-NLS, 2") {
        ## T, Txpoor
        beta <- c(0.310, -2.045)
        se <- c(0.328, 0.583)
    } else if (name == "Table 4, FE-NLS, 3") {
        ## T, Txpoor
        beta <- c(0.293, -1.982)
        se <- c(0.332, 0.583)
    } else if (name == "Table 6, FE-NLS, 2") {
        ## Txpoor, LTxpoor, Txrich, LTxrich
        beta <- c(-1.773, 0.900, 0.250, 0.265)
        se <- c(0.580, 0.432, 0.335, 0.214)
    } else if (name == "Table 6, FE-NLS, 6") {
        ## Txpoor, LTxpoor, Txrich, LTxrich
        beta <- c(-1.735, 0.883, 0.259, 0.279)
        se <- c(0.577, 0.428, 0.344, 0.216)
    }

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc] / 100, se[cc] / 100)

    poors <- get.poors(1970)
    subera5.lag <- NULL

    setup <- function(mcii) {
        subera5.lag <<- NULL

        if (is.null(mcii))
            return(beta / 100)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        ## Currently ignore y_t-1
        if (length(coeffs) == 1)
            dimpact <- (subera5$t2m - 273.15) * coeffs[1]
        else if (length(coeffs) == 2)
            dimpact <- (subera5$t2m - 273.15) * (coeffs[1] + coeffs[2] * (subera5$ISO %in% poors))
        else if (length(coeffs) == 4) {
            if (contemp.only) {
                dimpact <- (((subera5$t2m - 273.15) * coeffs[1]) * (subera5$ISO %in% poors)) +
                    (((subera5$t2m - 273.15) * coeffs[3]) * !(subera5$ISO %in% poors))
            } else {
                if (is.null(subera5.lag)) {
                    dimpact <- NA
                } else {
                    dimpact <- (((subera5$t2m - 273.15) * coeffs[1] + (subera5.lag$t2m - 273.15) * coeffs[2]) * (subera5$ISO %in% poors)) +
                        (((subera5$t2m - 273.15) * coeffs[3] + (subera5.lag$t2m - 273.15) * coeffs[4]) * !(subera5$ISO %in% poors))
                }
            }
        }

        subera5.lag <<- subera5

        dimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Table 6, FE-NLS, 6")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres %>% filter(ISO == 'NOR'))$dimpact)
}
