## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("2-project/driver.R")
    source("lib/utils.R")
}

get.funcs <- function(name) {
    if (name == "Table 4, Spec. 1") {
        coeffnames <- c('DT', 'LDT')
        beta <- c(-0.00243, -0.00652)
        ts <- c(0.96, 1.68)
    } else if (name == "Table 4, Spec. 2") {
        coeffnames <- c('T', 'T2')
        beta <- c(0.00947, -0.000709)
        ts <- c(1.34, 2.1)
    } else if (name == "Table 4, Spec. 3") {
        coeffnames <- c('DT', 'LDT', 'DT:T', 'LDT:T', 'T')
        beta <- c(0.0155, 0.00612, -0.00130, -0.000960, -0.00679)
        ts <- c((1.85), (1.26), (-2.37), (-2.34), (-0.74))
    } else if (name == "Table 4, Spec. 4") {
        coeffnames <- c('DT', 'LDT', 'DT:T', 'LDT:T', 'T', 'T2')
        beta <- c(0.0165, 0.00671, -0.00140, -0.00102, -0.00846, 0.0000762)
        ts <- c((1.59), (1.15), (-1.80), (-2.17), (-0.64), (0.17))
    } else if (name == "Table 4, Spec. 5") {
        coeffnames <- paste0("L", c('DT', 'LDT', 'DT:T', 'LDT:T', 'T'))
        beta <- c(0.00641, 0.00345, -0.00109, -0.000718, -0.00675)
        ts <- c((1.04), (0.67), (-2.04), (-1.69), (-0.74))
    } else if (name == "Table 4, Spec. 6") {
        coeffnames <- paste0("L", c('DT', 'LDT', 'DT:T', 'LDT:T', 'T', 'T2'))
        beta <- c(0.00780, 0.00200, -0.00117, -0.000660, -0.00254, -0.000107)
        ts <- c((1.14), (0.32), (-1.82), (-1.29), (-0.20), (-0.23))
    }

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], abs(beta[cc] / ts[cc])) # ts = coeff / se

    subera5.lags <- list()

    setup <- function(mcii) {
        subera5.lags <<- list()

        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        if (contemp.only || length(subera5.lags) < 3) {
            dimpact <- subera5$t2m * NA
        } else {
            dimpact <- 0
            for (kk in 1:length(coeffnames)) {
                if (coeffnames[kk] == 'T')
                    dimpact <- dimpact + (subera5$t2m - 273.15) * coeffs[kk]
                else if (coeffnames[kk] == 'T2')
                    dimpact <- dimpact + (subera5$t2m - 273.15)^2 * coeffs[kk]
                else if (coeffnames[kk] == 'DT')
                    dimpact <- dimpact + (subera5$t2m - subera5.lags[[as.character(year - 1)]]$t2m) * coeffs[kk]
                else if (coeffnames[kk] == 'LDT')
                    dimpact <- dimpact + (subera5.lags[[as.character(year - 1)]]$t2m - subera5.lags[[as.character(year - 2)]]$t2m) * coeffs[kk]
                else if (coeffnames[kk] == 'DT:T')
                    dimpact <- dimpact + (subera5$t2m - subera5.lags[[as.character(year - 1)]]$t2m) * (subera5$t2m - 273.15) * coeffs[kk]
                else if (coeffnames[kk] == 'LDT:T')
                    dimpact <- dimpact + (subera5.lags[[as.character(year - 1)]]$t2m - subera5.lags[[as.character(year - 2)]]$t2m) * (subera5.lags[[as.character(year - 1)]]$t2m - 273.15) * coeffs[kk]
                else if (coeffnames[kk] == 'LLDT')
                    dimpact <- dimpact + (subera5.lags[[as.character(year - 2)]]$t2m - subera5.lags[[as.character(year - 3)]]$t2m) * coeffs[kk]
                else if (coeffnames[kk] == 'LLDT:T')
                    dimpact <- dimpact + (subera5.lags[[as.character(year - 2)]]$t2m - subera5.lags[[as.character(year - 3)]]$t2m) * (subera5.lags[[as.character(year - 2)]]$t2m - 273.15) * coeffs[kk]
                else if (coeffnames[kk] == 'LT')
                    dimpact <- dimpact + (subera5.lags[[as.character(year - 1)]]$t2m - 273.15) * coeffs[kk]
                else if (coeffnames[kk] == 'LT2')
                    dimpact <- dimpact + (subera5.lags[[as.character(year - 1)]]$t2m - 273.15)^2 * coeffs[kk]
                else
                    print(paste0("Unknown coefficient name: ", coeffnames[kk]))
            }
        }

        subera5.lags[[as.character(year)]] <<- subera5

        dimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs('Table 4, Spec. 3')
    oneres <- project.single(funcs$setup, funcs$simulate, adm.level=1)
    oneres.adm1 <- oneres %>% group_by(Year, ISO) %>% summarize(dimpact=mean(dimpact))
    oneres.adm0 <- project.single(funcs$setup, funcs$simulate, adm.level=0)
    plot((oneres.adm1 %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres.adm0 %>% filter(ISO == 'THA'))$dimpact)

    plot((oneres.adm1 %>% filter(ISO == 'NOR'))$dimpact)
    lines((oneres.adm0 %>% filter(ISO == 'NOR'))$dimpact)
}
