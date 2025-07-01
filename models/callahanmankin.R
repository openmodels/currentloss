if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

    source("driver.R")
    source("utils.R")
}

callahanmankin.bootstraps <- list("Main"=c("Attribution_Coefficients_Bootstrap_BHMSR.csv", "coef_t", "coef_t2"),
                                  "20th Century Reanalysis"=c("20CR_DamageFunction_Boot.csv", "beta_t1", "beta_t2"),
                                  "5 Lags"=c("Attribution_Coefficients_Bootstrap_BHMLR.csv", "coef_t", "coef_t2"),
                                  "Differentiated"=c("Attribution_Coefficients_Bootstrap_BHMRP.csv", "coef_t_poor","coef_t2_poor","coef_t_rich","coef_t2_rich"))

get.funcs <- function(name) {
    coeffs <- read.csv(file.path("../data/papers/Callahan & Mankin 2022", callahanmankin.bootstraps[[name]][1]))

    if (length(callahanmankin.bootstraps[[name]]) == 3) {
        setup <- function(mcii) {
            if (is.null(mcii))
                return(c(mean(coeffs[, callahanmankin.bootstraps[[name]][2]]),
                         mean(coeffs[, callahanmankin.bootstraps[[name]][3]])))
            as.numeric(coeffs[mcii, callahanmankin.bootstraps[[name]][2:3]])
        }

        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            tas <- subera5$t2m - 273.15
            tas2 <- tas^2
            cbind(tas, tas2) %*% coeffs
        }
    } else {
        ## Based on 1990 comparison
        poors <- get.poors(1990)

        setup <- function(mcii) {
            if (is.null(mcii))
                return(as.numeric(colMeans(coeffs[, callahanmankin.bootstraps[[name]][2:5]])))
            as.numeric(coeffs[mcii, callahanmankin.bootstraps[[name]][2:5]])
        }

        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            tas <- subera5$t2m - 273.15
            tas2 <- tas^2
            (cbind(tas, tas2) %*% coeffs[1:2]) * (subera5$ISO %in% poors) +
                (cbind(tas, tas2) %*% coeffs[3:4]) * !(subera5$ISO %in% poors)
        }
    }

    return(list(setup=setup, simulate=simulate))
}

funcs <- get.funcs("Main")
oneres <- project.single(funcs$setup, funcs$simulate)
plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
plot((oneres %>% filter(ISO == 'THA'))$dimpact)
