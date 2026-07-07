## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")
library(tidyr)

if (F) {
    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    if (name == "Base IIS") {
        beta <- c(0.01032, -0.00039)
        se <- c(0.00233, 0.00007)
    } else if (name == "Adaptation IIS") {
        beta <- c(-0.03384, 0.00001, 0.00416, -0.00002)
        se <- c(0.0072, 0.00026, 0.00077, 0.00003)
    } else {
        ERROR
    }

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta)) {
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])
    }

    gdppcs <- read.csv("data/socioeconomics/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_3630804/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_3630804.csv", skip=3)
    gdppcs2 <- gdppcs %>% pivot_longer(cols=starts_with("X"), names_to="year", names_prefix="X", values_to="gdppc", values_drop_na=T)
    gdppcs3 <- gdppcs2 %>% group_by(Country.Code) %>% filter(n() > 1) %>% reframe(loggdppc=approx(as.numeric(year), log(gdppc), 1940:2025, rule=2)$y, year=1940:2025)

    setup <- function(mcii) {
        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        dimpact <- (subera5$t2m - 273.15) * coeffs[1] + (subera5$t2m - 273.15)^2 * coeffs[2]
        if (length(beta) > 2) {
            if ('loggdppc' %in% names(subera5))
                subera5 <- subera5 %>% select(!c(loggdppc, year))
            subera5b <- subera5 %>% left_join(gdppcs3[gdppcs3$year == year,], by=c('ISO'='Country.Code'))
            dimpact <- dimpact + subera5b$loggdppc * ((subera5$t2m - 273.15) * coeffs[3] + (subera5$t2m - 273.15)^2 * coeffs[4])
        }
        dimpact
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs("Base IIS")
    oneres0 <- project.single(funcs$setup, funcs$simulate)

    funcs <- get.funcs("Adaptation IIS")
    oneres1 <- project.single(funcs$setup, funcs$simulate)

    plot((oneres0 %>% filter(ISO == 'THA'))$dimpact)
    lines((oneres1 %>% filter(ISO == 'THA'))$dimpact)

    plot((oneres0 %>% filter(ISO == 'NOR'))$dimpact)
    lines((oneres1 %>% filter(ISO == 'NOR'))$dimpact)
}
