if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    ## temp deviations from whole-period mean
    ## dtemp, Lgrowth
    if (name == 'Table 2') {
        beta <- c(-0.0057, 0.702)
        se <- c(0.04, 0.)
    } else if (name == 'Table 3') {
        beta <- c(-0.0052, 0.678)
        se <- c(0.04, 0.)
    } else if (name == 'Table 4') {
        beta <- c(-0.0066, 0.724)
        se <- c(0.03, 0.)
    } else {
        return(NULL)
    }

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    poors <- get.poors(1970)

    setup <- function(mcii) {
        subera5.lag <<- NULL

        if (is.null(mcii))
            return(beta)
        coeffs[mcii, ]
    }

    ## Calculate the avg temp over their period
    subera5.hist <- era5 %>% filter(Year >= 1960 & Year <= 2019) %>% group_by(ISO) %>% summarize(t2m.hist=mean(t2m))

    subera5.lag <- NULL
    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        if (is.null(subera5.lag)) {
            dimpact <- subera5$t2m * NA
        } else {
            subera5.both <- subera5 %>% left_join(subera5.hist, by='ISO')
            if (contemp.only) {
                dimpact <- (subera5.both$t2m - subera5.both$t2m.hist) * coeffs[1]
            } else {
                dimpact <- (subera5.both$t2m - subera5.both$t2m.hist) * coeffs[1] + subera5.lag$growth * coeffs[2]
            }
        }

        subera5.lag <<- subera5

        dimpact
    }

    list(setup=setup, simulate=simulate)
}

if (F) {
    funcs <- get.funcs('Table 2')
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
