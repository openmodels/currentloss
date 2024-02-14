if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

    library(MASS)

    source("driver.R")
    source("utils.R")
}

get.funcs <- function(name) {
    ## diff from historical temp temp2
    beta <- c(0.019, -0.028)
    se <- c(0.014, 0.014)

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    subera5.hist <- era5 %>% filter(Year >= 1951 & Year <= 1980) %>% group_by(ISO) %>% summarize(t2m.hist=mean(t2m))

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        subera5.both <- subera5 %>% left_join(subera5.hist, by='ISO')

        tas2 <- (subera5.both$t2m - subera5.both$t2m.hist)^2

        values <- (subera5.both$t2m - subera5.both$t2m.hist) * coeffs[1] + tas2 * coeffs[2]
        values[!(subera5$ISO %in% get.africa())] <- NA
        values
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Current")
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'ETH'))$dimpact)
}
