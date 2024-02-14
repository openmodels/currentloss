if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

    library(MASS)

    source("driver.R")
    source("utils.R")
}

library(readxl)

get.funcs <- function(name) {
    output <- read_excel("../papers/models/Acevedo et al. 2020 SI/output/Table_1.xlsx", sheet=name)

    beta <- c(output$t_coef[output$period == 0], output$t2_coef[output$period == 0],
              output$t_coef[output$period == 7], output$t2_coef[output$period == 7])
    se <- c(output$t_se[output$period == 0], output$t2_se[output$period == 0],
            output$t_se[output$period == 7], output$t2_se[output$period == 7])

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
        subera5.lags[[as.character(year)]] <<- subera5

        if (length(subera5.lags) < 2)
            subera5$t2m * NA
        else {
            tas <- subera5$t2m - 273.15
            tas2 <- tas^2

            if (contemp.only) {
                tas * coeffs[1] + tas2 * coeffs[2]
            } else {
                tas * coeffs[3] + tas2 * coeffs[4]
            }
        }
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("column_5")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
