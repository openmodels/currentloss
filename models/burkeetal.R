if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

    library(MASS)

    source("driver.R")
    source("utils.R")
}

get.funcs <- function(name) {
    if (name %in% c("Main", ">= 20 years", "Continent x year", "Continent x year, no trends", "No Year FE", "Linear trends", "PWT", "DJO+Quad+yearFE+trend")) {
        if (name == "Main") {
            gamma <- c(0.0127183, -0.0004871)
            gammavcv <- matrix(c(0.00001435,-0.0000003758,-3.758E-07,1.402E-08), 2, 2, byrow=T)
        } else if (name == ">= 20 years") {
            ## Drop countries with < 20 years data
            gamma <- c(.013456, -.0005026)
            gammavcv <- matrix(c(.00001445, -3.778e-07, -3.778e-07, 1.418e-08), 2, 2)
        } else if (name == "Continent x year") {
            gamma <- c(.0142308, -.0004768)
            gammavcv <- matrix(c(.00001401, -3.601e-07, -3.601e-07, 1.526e-08), 2, 2)
        } else if (name == "Continent x year, no trends") {
            ## Continent x year FE, no time trends
            gamma <- c(.013279, -.0003815)
            gammavcv <- matrix(c(.00001126, -2.527e-07, -2.527e-07, 1.041e-08), 2, 2)
        } else if (name == "No Year FE") {
            ## reg growthWDI temp temp2 precip precip2 _yi_* _y2_* i.iso_id, cluster(iso_id)
            gamma <- c(.0102998, -.0004042)
            gammavcv <- matrix(c(.00001529, -3.738e-07, -3.738e-07, 1.321e-08), 2, 2)
        } else if (name == "Linear trends") {
            ## reg growthWDI temp temp2 precip precip2 i.year _yi_* i.iso_id, cluster(iso_id)
            gamma <- c(.0127707, -.0004766)
            gammavcv <- matrix(c(.00001836, -5.102e-07, -5.102e-07, 1.987e-08), 2, 2)
        } else if (name == "PWT") {
            ## reg rgdpCAPgr temp temp2 precip precip2 _yi_* _y2_* i.iso_id , cluster(iso_id)
            gamma <- c(.0072095, -.000365)
            gammavcv <- matrix(c(.00001486, -4.132e-07, -4.132e-07, 1.605e-08), 2, 2)
        } else if (name == "DJO+Quad+yearFE+trend") {
            ## (1) original DJO sample and specification, except quadratic (not linear) in temperature, as in 1 but adding country trends and precipitation, (3) as in 2 but replacing continent-year FE with year FE
            ## S3 (3): T, T2
            gamma <- c(0.0100, -0.0004)
            gammavcv <- matrix(c(0.0059^2, 0, 0, 0.0002^2), 2, 2)
        }


        coeffs <- mvrnorm(MCNUM, mu=gamma, Sigma=gammavcv, empirical=T)

        setup <- function(mcii) {
            if (is.null(mcii))
                return(gamma)
            as.numeric(coeffs[mcii,])
        }

        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            tas <- subera5$t2m - 273.15
            tas2 <- tas^2
            cbind(tas, tas2) %*% coeffs
        }
    } else if (name == "Poor interact") {
        ##         Rich T    Poor T    Rich T^2   Poor T2
        gamma <- c(.0088951, .0254342, -.0003155, -.0007719)
        gammavcv.vals <- c(.00001945,
                           7.555e-07, .00031159,
                           -6.849e-07, -2.073e-08, 3.403e-08,
                           -7.440e-08, -6.401e-06, 1.661e-09, 1.396e-07)
        gammavcv <- matrix(NA, nrow=4, ncol=4)
        gammavcv[upper.tri(gammavcv, diag=T)] <- gammavcv.vals
        gammavcv[lower.tri(gammavcv)] <- t(gammavcv)[lower.tri(gammavcv)]
        coeffs <- mvrnorm(MCNUM, mu=gamma, Sigma=gammavcv, empirical=T)

        poors <- get.poors(1970)

        setup <- function(mcii) {
            if (is.null(mcii))
                return(gamma)
            as.numeric(coeffs[mcii,])
        }

        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            tas <- subera5$t2m - 273.15
            tas2 <- tas^2
            tas * ifelse(subera5$ISO %in% poors, coeffs[2], coeffs[1]) + tas2 * ifelse(subera5$ISO %in% poors, coeffs[4], coeffs[3])
        }
    } else if (name %in% c("1 Lag", "3 Lags")) {
        if (name == "1 Lag") {
            ## reg growthWDI L.growthWDI temp temp2 precip precip2 _yi_* _y2_* i.iso_id , cluster(iso_id)
            gamma <- c(.1783578, .0086793, -.0003639)
            gammavcv.vals <- c(.00173076,
                               .00001284,   .00001535,
                               2.842e-07,  -3.960e-07,   1.387e-08)
            nlags <- 1
        } else if (name == "3 Lags") {
            ## reg growthWDI L(1/3).growthWDI temp temp2 precip precip2 _yi_* _y2_* i.iso_id, cluster(iso_id)
            gamma <- c(.1973942, -.0197077, -.030039, .0062443, -.0003127)
            gammavcv.vals <- c(.00179102,
                               .00021509,   .00065659,
                               -.00004847,   .00010312,   .00039722,
                               .00001563,  -.00001312,  -3.446e-07,   .00001418,
                               2.569e-07,   5.459e-07,   1.331e-07,  -3.896e-07,   1.409e-08)
            nlags <- 3
        }

        gammavcv <- vcv.from.vals(gammavcv.vals)
        coeffs <- mvrnorm(MCNUM, mu=gamma, Sigma=gammavcv, empirical=T)

        subera5.lags <- list()

        setup <- function(mcii) {
            subera5.lags <<- list()
            if (is.null(mcii))
                return(gamma)
            as.numeric(coeffs[mcii,])
        }

        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            subera5.lags[[as.character(year)]] <<- subera5

            if (length(subera5.lags) < nlags + 1)
                subera5$t2m * NA
            else {
                tas <- subera5$t2m - 273.15
                tas2 <- tas^2

                totals <- tas * coeffs[nlags + 1] + tas2 * coeffs[nlags + 2]
                if (!contemp.only) {
                    for (yy in (year - 1):(year - nlags)) {
                        totals <- totals + subera5.lags[[as.character(yy)]]$growth * coeffs[year - yy]
                    }
                }
                totals
            }
        }
    } else if (name == "Climate interact") {
        ## S1: T, T*Tbar
        gamma <- c(0.0126, -0.0010)
        gammavcv <- matrix(c(0.0037^2, 0, 0, 0.0002^2), 2, 2)
        coeffs <- mvrnorm(MCNUM, mu=gamma, Sigma=gammavcv, empirical=T)

        years.read <- 0
        subera5.n30.0 <- data.frame()
        setup <- function(mcii) {
            years.read <<- 0
            subera5.n30.0 <<- data.frame()

            if (is.null(mcii))
                return(gamma)
            as.numeric(coeffs[mcii,])
        }

        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            if (years.read >= 30) {
                subera5.n30.0 <<- rbind(subera5.n30.0[subera5.n30.0$Year > min(subera5.n30.0$Year),],
                                        subera5)
            } else
                subera5.n30.0 <<- rbind(subera5.n30.0, subera5)
            years.read <<- years.read + 1

            if (years.read < 30)
                subera5$t2m * NA
            else {
                subera5.stats <- subera5.n30.0 %>% group_by(ISO) %>% summarize(mu=mean(t2m))
                if (contemp.only) {
                    (subera5$t2m - 273.15) * (coeffs[1] + coeffs[2] * (subera5$t2m - 273.15))
                } else {
                    subera5.both <- subera5 %>% left_join(subera5.stats)
                    (subera5.both$t2m - 273.15) * (coeffs[1] + coeffs[2] * (subera5.both$mu - 273.15))
                }
            }
        }
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Main")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
