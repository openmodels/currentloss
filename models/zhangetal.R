## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    stopifnot(name == "Table A3")
    beta <- c(-0.0955, 0.0468, -0.9606, 0.8505) / 100
    se <- c(0.0191, 0.0438, 0.1123, 0.0978) / 100

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        stopifnot(!is.na(subera5$t2mmax5day)) # Did I generate new weather data?
        stopifnot(F) # Once I regenerate it, need to test this!
        (subera5$t2mmax5day - 273.15) * (coeffs[1] + (subera5$t2m - 273.15) * coeffs[2]) + (subera5$t2m - 273.15) * coeffs[3] +
            sqrt(subera5$t2mvaravg) * coeffs[4] # average across months of sd within month
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Table A3")

    oneres <- project.single(funcs$setup, funcs$simulate, adm.level=1)
    oneres.adm1 <- oneres %>% group_by(Year, ISO) %>% summarize(dimpact=mean(dimpact))
    oneres.adm0 <- project.single(funcs$setup, funcs$simulate, adm.level=0)
    plot((oneres.adm1 %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres.adm0 %>% filter(ISO == 'THA'))$dimpact)

    plot((oneres.adm1 %>% filter(ISO == 'NOR'))$dimpact)
    lines((oneres.adm0 %>% filter(ISO == 'NOR'))$dimpact)
}
