if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

    source("driver.R")
    source("utils.R")
}

get.funcs <- function(name) {
    if (name == 'Table 1, Col 1') {
        ## T, T2 Cell level
        beta <- c(0.693, -0.021)
        se <- c(0.225, 0.006)
    } else if (name == 'Table 1, Col 2') {
        ## T, T2 Adm 1 level
        beta <- c(0.744, -0.025)
        se <- c(0.221, 0.005)
    } else if (name == 'Table 2, Col 1-2') {
        ## Rich T, T2, Poor T, T2
        beta <- c(0.242, -0.013, 0.768, -0.024)
        se <- c(0.107, 0.004, 0.338, 0.008)
    }

    ## In % growth terms
    beta <- beta / 100
    se <- se / 100

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    poors <- get.poors(1970)

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        coeffs[mcii, ]
    }

    if (name %in% c('Table 1, Col 1', 'Table 1, Col 2')) {
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            (subera5$t2m - 273.15) * coeffs[1] + (subera5$t2m - 273.15)^2 * coeffs[2]
        }
    } else if (name == 'Table 2, Col 1-2') {
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            (subera5$t2m - 273.15) * (coeffs[1] * !(subera5$ISO %in% poors) +
                                      coeffs[3] * (subera5$ISO %in% poors)) +
            (subera5$t2m - 273.15)^2 * (coeffs[2] * !(subera5$ISO %in% poors) +
                                        coeffs[4] * (subera5$ISO %in% poors))
        }
    }

    list(setup=setup, simulate=simulate)
}

if (F) {
    funcs <- get.funcs('Table 1, Col 1')
    oneres <- project.single(funcs$setup, funcs$simulate, adm.level=1)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
