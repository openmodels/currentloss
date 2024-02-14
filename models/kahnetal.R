if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")

    source("driver.R")
    source("utils.R")
}

get.funcs <- function(name) {
    ## difftas-pos: Difference between yearly average temperature and 30-year average, if positive [C]
    ## difftas-neg: Difference between yearly average temperature and 30-year average, if negative [C]

    if (name == "Table 1, Spec. 1, m = 20, FE") {
        beta <- c(-0.373, -0.441)
        se <- c(0.141, 0.217)
        mm <- 20
    } else if (name == "Table 1, Spec. 1, m = 20, HPJ-FE") {
        beta <- c(-0.566, -0.500)
        se <- c(0.209, 0.249)
        mm <- 20
    } else if (name == "Table 1, Spec. 1, m = 30, FE") {
        beta <- c(-0.583, -0.699)
        se <- c(0.195, 0.346)
        mm <- 30
    } else if (name == "Table 1, Spec. 1, m = 30, HPJ-FE") {
        beta <- c(-0.894, -0.783)
        se <- c(0.291, 0.380)
        mm <- 30
    } else if (name == "Table 1, Spec. 1, m = 40, FE") {
        beta <- c(-0.701, -0.834)
        se <- c(0.248, 0.445)
        mm <- 40
    } else if (name == "Table 1, Spec. 1, m = 40, HPJ-FE") {
        beta <- c(-1.072, -0.909)
        se <- c(0.373, 0.485)
        mm <- 40
    } else if (name == "Table 2, Spec. 1, m = 20, FE") {
        beta <- c(-0.375)
        se <- c(0.142)
        mm <- 20
    } else if (name == "Table 2, Spec. 1, m = 20, HPJ-FE") {
        beta <- c(-0.523)
        se <- c(0.201)
        mm <- 20
    } else if (name == "Table 2, Spec. 1, m = 30, FE") {
        beta <- c(-0.582)
        se <- c(0.199)
        mm <- 30
    } else if (name == "Table 2, Spec. 1, m = 30, HPJ-FE") {
        beta <- c(-0.836)
        se <- c(0.284)
        mm <- 30
    } else if (name == "Table 2, Spec. 1, m = 40, FE") {
        beta <- c(-0.702)
        se <- c(0.252)
        mm <- 40
    } else if (name == "Table 2, Spec. 1, m = 40, HPJ-FE") {
        beta <- c(-0.981)
        se <- c(0.361)
        mm <- 40
    } else if (name == "Table 3, Spec. 3, m = 30, FE") {
        beta <- c(-0.551, -0.156)
        se <- c(0.235, 0.396)
        mm <- 30
    } else if (name == "Table 3, Spec. 3, m = 30, HPJ-FE") {
        beta <- c(-0.836, -0.137)
        se <- c(0.368, 0.586)
        mm <- 30
    } else if (name == "Table 3, Spec. 4, m = 30, FE") {
        beta <- c(-0.754, 0.496)
        se <- c(0.200, 0.420)
        mm <- 30
    } else if (name == "Table 3, Spec. 4, m = 30, HPJ-FE") {
        beta <- c(-1.029, 0.562)
        se <- c(0.287, 0.656)
        mm <- 30
    }

    ## Correct betas and ses
    beta <- beta * (2 / (mm + 1))
    se <- se * (2 / (mm + 1))

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    lastmm <- NULL

    setup <- function(mcii) {
        lastmm <<- NULL

        if (is.null(mcii))
            return(beta)
        as.numeric(coeffs[mcii,])
    }

    poors <- get.poors(1980)

    if (name %in% c("Table 3, Spec. 3, m = 30, FE", "Table 3, Spec. 3, m = 30, HPJ-FE")) {
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            if (year == min(years)) {
                lastmm <<- matrix(NA, nrow(subera5), 30)
                lastmm[, 1] <<- subera5$t2m - 273.15
            }

            clims <- rowMeans(lastmm, na.rm=T)
            diffs <- subera5$t2m - 273.15 - clims

            absdiff <- abs(diffs)

            lastmm[, (year - min(years)) %% 30 + 1] <<- subera5$t2m - 273.15

            if (contemp.only)
                absdiff * NA
            else
                (coeffs[1] + coeffs[2] * (subera5$ISO %in% poors)) * absdiff
        }
    } else if (name %in% c("Table 3, Spec. 4, m = 30, FE", "Table 3, Spec. 4, m = 30, HPJ-FE")) {
        return(NULL) # I don't know what global median period is, and need way to get historical averages
    } else if (length(beta) == 2) {
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            if (year == min(years)) {
                lastmm <<- matrix(NA, nrow(subera5), 30)
                lastmm[, 1] <<- subera5$t2m - 273.15
            }

            clims <- rowMeans(lastmm, na.rm=T)
            diffs <- subera5$t2m - 273.15 - clims

            posdiff <- pmax(diffs, 0)
            negdiff <- -pmin(diffs, 0)

            lastmm[, (year - min(years)) %% 30 + 1] <<- subera5$t2m - 273.15

            if (contemp.only)
                posdiff * NA
            else
                coeffs[1] * posdiff + coeffs[2] * negdiff
        }
    } else {
        simulate <- function(coeffs, year, subera5, contemp.only=F) {
            if (year == min(years)) {
                lastmm <<- matrix(NA, nrow(subera5), 30)
                lastmm[, 1] <<- subera5$t2m - 273.15
            }

            clims <- rowMeans(lastmm, na.rm=T)
            diffs <- subera5$t2m - 273.15 - clims

            absdiff <- abs(diffs)

            lastmm[, (year - min(years)) %% 30 + 1] <<- subera5$t2m - 273.15

            if (contemp.only)
                absdiff * NA
            else
                coeffs[1] * absdiff
        }
    }

    return(list(setup=setup, simulate=simulate))
}

if (F) {
    funcs <- get.funcs('Table 1, Spec. 1, m = 30, HPJ-FE')
    oneres <- project.single(setup, simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
