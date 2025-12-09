## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    stopifnot(name %in% c("Table 3, Model 1", "Table 3, Model 2", "Table 3, Model 3", "Table 3, Model 4"))
    ## All models have the same coefficients and standard errors
    beta <- c(0.011, -0.0004)
    se <- c(0.001, 0.0000)

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        (subera5$t2m - 273.15) * coeffs[1] + (subera5$t2m - 273.15)^2 * coeffs[2]
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Table 3, Model 1")

    oneres <- project.single(funcs$setup, funcs$simulate, adm.level=1)
    oneres.adm1 <- oneres %>% group_by(Year, ISO) %>% summarize(dimpact=mean(dimpact))
    oneres.adm0 <- project.single(funcs$setup, funcs$simulate, adm.level=0)
    plot((oneres.adm1 %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres.adm0 %>% filter(ISO == 'THA'))$dimpact)

    plot((oneres.adm1 %>% filter(ISO == 'NOR'))$dimpact)
    lines((oneres.adm0 %>% filter(ISO == 'NOR'))$dimpact)
}
