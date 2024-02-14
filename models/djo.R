if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

    source("driver.R")
    source("utils.R")
}

get.funcs <- function(name) {
    if (name == 'Main 2.3') {
        ## Main 2.3
        ## T, Txpoor
        beta <- c(0.262, -1.610)
        se <- c(0.311, 0.485)
    } else if (name == 'Main poor-only') {
        ## Txpoor
        beta <- -1.394
        se <- 0.408
    } else if (name == '1 Lag model 3.2') {
        ## 1 Lag model 3.2
        ## Txpoor, LTxpoor, Txrich, LTxrich
        beta <- c(-1.559, 0.576, 0.215, 0.137)
        se <- c(0.522, 0.433, 0.322, 0.298)
    } else if (name == 'All FE and country specific trends 4.2') {
        ## All FE and country specific trends 4.2
        ## Txpoor, Txrich
        beta <- c(-1.723, 0.417)
        se <- c(0.603, 0.473)
    } else if (name == 'GDP data from PWT 4.5') {
        ## GDP data from PWT 4.5
        ## Txpoor, Txrich
        beta <- c(-0.860, 0.343)
        se <- c(0.299, 0.228)
    } else if (name == 'Area-weighted climate data 4.6') {
        ## Area-weighted climate data 4.6
        ## Txpoor, Txrich
        beta <- c(-0.891, 0.480)
        se <- c(0.347, 0.220)
    } else if (name == 'Medium run OLS') {
        ## Medium run OLS
        ## T, Txpoor
        beta <- c(1.325, -3.010)
        se <- c(0.980, 1.456)
    } else if (name == 'Anomalies A8.3') {
        ## Anomalies A8.3
        ## Z-score T, Txpoor
        beta <- c(0.145, -0.543)
        se <- c(0.131, 0.289)
    } else if (name == 'First differencing A14.1') {
        ## First differencing A14.1
        ## Txrich, Txpoor
        beta <- c(-1.074, 0.208)
        se <- c(0.446, 0.212)
    } else if (name == 'First differencing A14.2') {
        ## First differencing A14.2
        ## Txrich, Txpoor
        beta <- c(-1.210, 0.003)
        se <- c(0.558, 0.005)
    } else if (name == '10 Lag model A34') {
        ## 10 Lag model A34
        ## Txpoor, Txrich, LTxpoor, LTxrich, ..., L10Txpoor, L10Txrich
        beta <- c(-1.580, 0.234, 0.627, 0.168, -0.354, 0.172, -0.152, -0.137, 0.3, -0.144, -0.339, -0.479, 0.615, 0.100, 0.659, -0.318, -0.35, 0.031, -0.377, -0.063, 0.092, 0.248)
        se <- c(0.579, 0.356, 0.481, 0.323, 0.586, 0.273, 0.506, 0.286, 0.505, 0.270, 0.492, 0.290, 0.586, 0.282, 0.581, 0.354, 0.612, 0.317, 0.515, 0.332, 0.560, 0.297)
    } else {
        return(NULL)
    }

    ## In % growth terms
    beta <- beta / 100
    se <- se / 100

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    poors <- get.poors(1970)

    setup <- function(mcii) {
        ## Reset any globals that might be used
        subera5.lag <<- NULL
        years.read <<- 0
        subera5.n15.0 <<- data.frame()
        subera5.n30.0 <<- data.frame()
        subera5.lags <<- list()

        if (is.null(mcii))
            return(beta)
        coeffs[mcii, ]
    }

    if (name == 'Main 2.3') {
        ## Main 2.3
        ## T, Txpoor
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            (subera5$t2m - 273.15) * (coeffs[1] + coeffs[2] * (subera5$ISO %in% poors))
        }
    } else if (name == 'Main poor-only') {
        ## Txpoor
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            (subera5$t2m - 273.15) * coeffs[1] * (subera5$ISO %in% poors)
        }
    } else if (name == '1 Lag model 3.2') {
        ## 1 Lag model 3.2
        ## Txpoor, LTxpoor, Txrich, LTxrich
        subera5.lag <- NULL
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            subera5.lag.saved <- subera5.lag
            subera5.lag <<- subera5
            if (is.null(subera5.lag.saved)) {
                subera5$t2m * NA
            } else {
                if (contemp.only)
                    (subera5$t2m - 273.15) * (coeffs[1] * (subera5$ISO %in% poors) + coeffs[3] * !(subera5$ISO %in% poors)) +
                        (subera5.lag.saved$t2m - 273.15) * (coeffs[2] * (subera5$ISO %in% poors) + coeffs[4] * !(subera5$ISO %in% poors))
                else
                    (subera5$t2m - 273.15) * (coeffs[1] * (subera5$ISO %in% poors) + coeffs[3] * !(subera5$ISO %in% poors))
            }
        }
    } else if (name %in% c('All FE and country specific trends 4.2', 'GDP data from PWT 4.5', 'Area-weighted climate data 4.6')) {
        ## All FE and country specific trends 4.2
        ## GDP data from PWT 4.5
        ## Area-weighted climate data 4.6
        ## Txpoor, Txrich
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            (subera5$t2m - 273.15) * (coeffs[1] * (subera5$ISO %in% poors) + coeffs[2] * !(subera5$ISO %in% poors))
        }
    } else if (name == 'Medium run OLS') {
        ## Medium run OLS
        # T, Txpoor
        years.read <- 0
        subera5.n15.0 <- data.frame()
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            if (years.read >= 15) {
                subera5.n15.0 <<- rbind(subera5.n15.0[subera5.n15.0$Year > min(subera5.n15.0$Year),],
                                        subera5)
            } else
                subera5.n15.0 <<- rbind(subera5.n15.0, subera5)
            years.read <<- years.read + 1

            if (years.read < 15)
                subera5$t2m * NA
            else {
                subera5.mr <- subera5.n15.0 %>% group_by(ISO) %>% summarize(t2m.mr=mean(t2m))
                if (contemp.only) {
                    (subera5$t2m - 273.15) * (coeffs[1] + coeffs[2] * (subera5$ISO %in% poors))
                } else {
                    subera5.both <- subera5 %>% left_join(subera5.mr)
                    (subera5.both$t2m.mr - 273.15) * (coeffs[1] + coeffs[2] * (subera5.both$ISO %in% poors))
                }
            }
        }
    } else if (name == 'Anomalies A8.3') {
        ## Anomalies A8.3
        ## Z-score T, Txpoor
        years.read <- 0
        subera5.n30.0 <- data.frame()
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
                subera5.stats <- subera5.n30.0 %>% group_by(ISO) %>% summarize(mu=mean(t2m), sd=sd(t2m))
                if (contemp.only) {
                    0 * subera5$t2m
                } else {
                    subera5.both <- subera5 %>% left_join(subera5.stats)
                    ((subera5.both$t2m - subera5.both$mu) / subera5.both$sd) * (coeffs[1] + coeffs[2] * (subera5$ISO %in% poors))
                }
            }
        }
    } else if (name %in% c('First differencing A14.1', 'First differencing A14.2')) {
        ## First differencing A14.1
        ## First differencing A14.2
        ## Txrich, Txpoor
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            (subera5$t2m - 273.15) * (coeffs[1] * !(subera5$ISO %in% poors) + coeffs[2] * (subera5$ISO %in% poors))
        }
    } else if (name == '10 Lag model A34') {
        ## 10 Lag model A34
        ## Txpoor, Txrich, LTxpoor, LTxrich, ..., L10Txpoor, L10Txrich
        subera5.lags <- list()
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            subera5.lags[[as.character(year)]] <<- subera5

            if (length(subera5.lags) < 10)
                subera5$t2m * NA
            else {
                if (contemp.only) {
                    (subera5$t2m - 273.15) * (coeffs[1] * (subera5$ISO %in% poors) + coeffs[2] * !(subera5$ISO %in% poors))
                } else {
                    totals <- subera5$t2m * 0
                    for (yy in year:(year - 9)) {
                        totals <- totals +
                            (subera5.lags[[as.character(yy)]]$t2m - 273.15) * (coeffs[2*(year - yy) + 1] * (subera5$ISO %in% poors) + coeffs[2*(year - yy) + 2] * !(subera5$ISO %in% poors))
                    }
                    totals
                }
            }
        }
    } else {
        return(NULL)
    }

    list(setup=setup, simulate=simulate)
}

if (F) {
    funcs <- get.funcs('Main 2.3')
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)

    library(PBSmapping)

    polydata <- attr(importShapefile("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses/data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

    byyear <- oneres %>% left_join(polydata[, c('ADM0_A3', 'POP_EST')], by=c('ISO'='ADM0_A3')) %>%
        group_by(Year) %>% summarize(dimpact.pop=sum(dimpact * POP_EST) / sum(POP_EST))
    plot(byyear$dimpact.pop)
}
