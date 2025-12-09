## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    stopifnot(name == "Table 2, Column 6")
    beta <- c(-0.005)
    se <- c(0.003)

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        (subera5$t2m - 273.15) * coeffs[1]
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Table 2, Column 6")

    oneres <- project.single(funcs$setup, funcs$simulate, adm.level=1)
    oneres.adm1 <- oneres %>% group_by(Year, ISO) %>% summarize(dimpact=mean(dimpact))
    oneres.adm0 <- project.single(funcs$setup, funcs$simulate, adm.level=0)
    plot((oneres.adm1 %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres.adm0 %>% filter(ISO == 'THA'))$dimpact)

    plot((oneres.adm1 %>% filter(ISO == 'NOR'))$dimpact)
    lines((oneres.adm0 %>% filter(ISO == 'NOR'))$dimpact)
}
