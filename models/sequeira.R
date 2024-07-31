## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    if (name == "Table 3, Spec. 1 & 2") {
        ## y_t-1, temp_t-1, Delta temp
        beta1 <- c(-0.192, -0.005, -0.004)
        se1 <- c(0.016, 0.006, 0.004)
        beta2 <- c(-0.203, -0.002, -0.003)
        se2 <- c(0.020, 0.004, 0.004)
    } else if (name == "Table 5, Spec. 1 & 2, 4 & 5") {
        ## Rich y_t-1, temp_t-1, Delta temp; Poor ...
        beta1 <- c(-0.140, 0.003, 0.002, -0.243, -0.023, -0.022)
        se1 <- c(0.018, 0.005, 0.003, 0.026, 0.014, 0.010)
        beta2 <- c(-0.127, 0.005, 0.003, -0.293, -0.021, -0.014)
        se2 <- c(0.020, 0.003, 0.003, 0.032, 0.010, 0.008)
    }

    coeffs <- matrix(NA, MCNUM, length(beta1))
    for (cc in 1:length(beta1)) {
        coeffs[seq(1, MCNUM, by=2), cc] <- rnorm(length(seq(1, MCNUM, by=2)), beta1[cc], se1[cc])
        coeffs[seq(2, MCNUM, by=2), cc] <- rnorm(length(seq(2, MCNUM, by=2)), beta2[cc], se2[cc])
    }

    poors <- get.poors(1970)
    subera5.lag <- NULL

    setup <- function(mcii) {
        subera5.lag <<- NULL

        if (is.null(mcii))
            return(beta1)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        ## Currently ignore y_t-1
        if (is.null(subera5.lag)) {
            dimpact <- NA
        } else {
            if (length(coeffs) == 3) {
                dimpact <- (subera5.lag$t2m - 273.15) * coeffs[2] +
                    (subera5$t2m - subera5.lag$t2m) * coeffs[3]
            } else {
                dimpact <- ((subera5.lag$t2m - 273.15) * coeffs[2] + (subera5$t2m - subera5.lag$t2m) * coeffs[3]) * !(subera5$ISO %in% poors) +
                    ((subera5.lag$t2m - 273.15) * coeffs[5] + (subera5$t2m - subera5.lag$t2m) * coeffs[6]) * (subera5$ISO %in% poors)
            }
        }

        subera5.lag <<- subera5

        dimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Table 3, Spec. 1 & 2")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres %>% filter(ISO == 'NOR'))$dimpact)
}
