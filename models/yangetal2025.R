## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    if (name == "Panel A, Linear MIDAS") {
        beta <- rep(c(-0.429, 0.407), 2)
        cilo <- rep(c(-0.703, 0.119), 2)
        cihi <- rep(c(-0.155, 0.695), 2)
        gamma <- rep(0, 2)
        galo <- rep(0, 2)
        gahi <- rep(0, 2)
    } else if (name == "Panel B, Constant Threshold") {
        beta <- c(0.448, -0.398, -1.053, 1.110)
        cilo <- c(0.105, -0.721, -1.483, 0.660)
        cihi <- c(0.790, -0.074, -0.622,  1.559)
        gamma <- c(15.992, 0)
        galo <- c(11.229, 0)
        gahi <- c(20.736, 0)
    } else if (name == "Panel B, Covariate-dependent threshold") {
        beta <- c(0.147, -0.141, -1.393, 1.470)
        cilo <- c(-0.059, -0.337, -1.849, 0.972)
        cihi <- c(0.353, 0.055, -0.936, 1.967)
        gamma <- c(25.365, -9.055)
        galo <- c(20.836, -19.094)
        gahi <- c(30.307, -0.069)
    } else if (name == "Panel C, Constant Threshold") {
        beta <- c(0.469, -0.418, -1.039, 1.095)
        cilo <- c(0.102, -0.770, -1.465, 0.650)
        cihi <- c(0.836, -0.067, -0.612, 1.539)
        gamma <- c(15.996, 0)
        galo <- c(11.250, 0)
        gahi <- c(20.748, 0)
    } else if (name == "Panel C, Covariate-dependent threshold") {
        beta <- c(0.033, -0.046, -1.413, 1.489)
        cilo <- c(-0.086, -0.121, -1.869, 0.997)
        cihi <- c(0.152, 0.029, -0.957, 1.980)
        gamma <- c(25.582, -9.586)
        galo <- c(20.872, -19.031)
        gahi <- c(30.304, -0.128)
    } else {
        ERROR
    }

    coeffs <- matrix(NA, MCNUM, length(beta) + length(gamma))
    for (cc in 1:length(beta)) {
        side <- runif(MCNUM) < .5
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], ifelse(side, (cihi[cc] - beta[cc]) / qnorm(.975), (beta[cc] - cilo[cc]) / qnorm(.975)))
    }
    for (cc in 1:length(gamma)) {
        side <- runif(MCNUM) < .5
        coeffs[, length(beta) + cc] <- rnorm(MCNUM, gamma[cc], ifelse(side, (gahi[cc] - gamma[cc]) / qnorm(.975), (gamma[cc] - galo[cc]) / qnorm(.975)))
    }

    poors <- get.poors(1970)
    subera5.lag <- NULL

    setup <- function(mcii) {
        subera5.lag <<- NULL

        if (is.null(mcii))
            return(c(beta, gamma))
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        if (is.null(subera5.lag)) {
            subera5.lag <<- subera5
            return(NA)
        }

        threshold <- coeffs[5] + coeffs[6] * (subera5$ISO %in% poors)
        dimpact <- (subera5$t2m - 273.15) * ifelse(subera5$t2m - 273.15 < threshold, coeffs[1], coeffs[3])


        if (!contemp.only) {
            dimpact <- dimpact + (subera5.lag$t2m - 273.15) * ifelse(subera5$t2m - 273.15 < threshold, coeffs[2], coeffs[4])
        }

        subera5.lag <<- subera5

        dimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Panel C, Covariate-dependent threshold")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres %>% filter(ISO == 'NOR'))$dimpact)
}
