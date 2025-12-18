if (F) {
    setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

    source("src/2-project/driver.R")
    source("src/lib/utils.R")
}

get.funcs <- function(name) {
    ## Contemp Low, Med, High, Lag Low, Med, High
    ## Table 2 = Split on Temp, Table 3 = Split on income
    poorcold <- c('BIH', 'LSO', 'MNG', 'TJK')
    midlcold <- c('ALB', 'ARM', 'AZE', 'BGR', 'CHN', 'GEO', 'KAZ', 'KGZ', 'MDA', 'MKD', 'ROU', 'TKM', 'UKR', 'UZB')
    richcold <- c('AUT', 'BLR', 'CAN', 'CHE', 'CHL', 'CZE', 'DEU', 'DNK', 'ESP', 'EST', 'FIN', 'FRA', 'GBR', 'GRC', 'HRV', 'HUN', 'IRL', 'ISL', 'ITA', 'JPN', 'KOR', 'LTU', 'LVA', 'NLD', 'NOR', 'NZL', 'POL', 'RUS', 'SVK', 'SVN', 'SWE', 'TUR', 'USA')
    poormild <- c('AGO', 'BDI', 'BOL', 'COD', 'ETH', 'HND', 'KEN', 'MDG', 'MOZ', 'MWI', 'NPL', 'PAK', 'RWA', 'SLV', 'SYR', 'TZA', 'UGA', 'ZMB')
    midlmild <- c('BRA', 'BTN', 'BWA', 'COL', 'DOM', 'DZA', 'ECU', 'EGY', 'FJI', 'GTM', 'IRN', 'IRQ', 'JOR', 'LBN', 'MAR', 'NAM', 'PER', 'PRY', 'SWZ', 'TUN', 'ZAF', 'ZWE')
    richmild <- c('ARG', 'AUS', 'BHS', 'CYP', 'ISR', 'MEX', 'PRT', 'SAU', 'TWN', 'URY')
    poorwarm <- c('BEN', 'BFA', 'BGD', 'CAF', 'CIV', 'CMR', 'COG', 'DJI', 'GHA', 'GIN', 'GMB', 'GNB', 'HTI', 'IND', 'KHM', 'LAO', 'LBR', 'MLI', 'MMR', 'MRT', 'NER', 'NGA', 'SDN', 'SEN', 'SLE', 'TCD', 'TGO', 'VNM', 'YEM')
    midlwarm <- c('BLZ', 'CRI', 'GAB', 'GNQ', 'GUY', 'IDN', 'JAM', 'LKA', 'NIC', 'PAN', 'PHL', 'SUR', 'THA', 'VEN')
    richwarm <- c('ARE', 'BRN', 'KWT', 'MYS', 'OMN', 'QAT', 'TTO')

    if (name == 'Table 2, CCE1, Col 3') {
        beta <- c(0.443, -0.221,  -1.443, 0.101, 0.0809, -0.267)
        se <- c(0.181, 0.315, 0.359, 0.147, 0.193, 0.215)
        grp1 <- c(poorcold, midlcold, richcold)
        grp2 <- c(poormild, midlmild, richmild)
        grp3 <- c(poorwarm, midlwarm, richwarm)
    } else if (name == 'Table 2, CCE1, Col 4') {
        beta <- c(0.411, -0.430, -1.446, 0.0903,  0.181, -0.341)
        se <- c(0.175, 0.310, 0.346, 0.169, 0.232, 0.250)
        grp1 <- c(poorcold, midlcold, richcold)
        grp2 <- c(poormild, midlmild, richmild)
        grp3 <- c(poorwarm, midlwarm, richwarm)
    } else if (name == 'Table 2, CCE3, Col 5') {
        beta <- c(0.541, -0.110, -1.307, 0.485, 0.369, -0.286)
        se <- c(0.264, 0.367, 0.433, 0.240, 0.308, 0.340)
        grp1 <- c(poorcold, midlcold, richcold)
        grp2 <- c(poormild, midlmild, richmild)
        grp3 <- c(poorwarm, midlwarm, richwarm)
    } else if (name == 'Table 2, CCE3, Col 6') {
        beta <- c(0.495, -0.630, -1.471, 0.502, 0.184, -0.739)
        se <- c(0.269, 0.368, 0.434, 0.250, 0.325, 0.397)
        grp1 <- c(poorcold, midlcold, richcold)
        grp2 <- c(poormild, midlmild, richmild)
        grp3 <- c(poorwarm, midlwarm, richwarm)
    } else if (name == 'Table 3, CCE1, Col 3') {
        beta <- c(0.187, -0.861, -0.242, 0.159, 0.0893, -0.344)
        se <- c(0.238, 0.329, 0.318, 0.135, 0.204, 0.206)
        grp1 <- c(poorcold, poormild, poorwarm)
        grp2 <- c(midlcold, midlmild, midlwarm)
        grp3 <- c(richcold, richmild, richwarm)
    } else if (name == 'Table 3, CCE1, Col 4') {
        beta <- c(0.235, -0.865, -0.539, 0.161, 0.0989, -0.361)
        se <- c(0.217, 0.311, 0.326, 0.148, 0.223, 0.244)
        grp1 <- c(poorcold, poormild, poorwarm)
        grp2 <- c(midlcold, midlmild, midlwarm)
        grp3 <- c(richcold, richmild, richwarm)
    } else if (name == 'Table 3, CCE3, Col 5') {
        beta <- c(0.394, -0.542, -0.513, 0.381, 1.055, -0.931)
        se <- c(0.290, 0.410, 0.397, 0.214, 0.314, 0.369)
        grp1 <- c(poorcold, poormild, poorwarm)
        grp2 <- c(midlcold, midlmild, midlwarm)
        grp3 <- c(richcold, richmild, richwarm)
    } else if (name == 'Table 3, CCE3, Col 6') {
        beta <- c(0.701, -1.025, -1.140, 0.419, 0.809, -1.385)
        se <- c(0.268, 0.396, 0.404, 0.207, 0.334, 0.395)
        grp1 <- c(poorcold, poormild, poorwarm)
        grp2 <- c(midlcold, midlmild, midlwarm)
        grp3 <- c(richcold, richmild, richwarm)
    } else {
        return(NULL)
    }

    coeffs <- matrix(NA, MCNUM, length(beta))
    for (cc in 1:length(beta))
        coeffs[, cc] <- rnorm(MCNUM, beta[cc], se[cc])

    setup <- function(mcii) {
        subera5.lag <<- NULL

        if (is.null(mcii))
            return(beta / 100)
        coeffs[mcii, ] / 100
    }

    subera5.lag <- NULL
    simulate <- function(coeffs, year, subera5, contemp.only=F) {
        dimpact <- (subera5$ISO %in% grp1) * subera5$t2m * coeffs[1] +
            (subera5$ISO %in% grp2) * subera5$t2m * coeffs[2] +
            (subera5$ISO %in% grp3) * subera5$t2m * coeffs[3]
        if (!contemp.only) {
            dimpact <- dimpact +
                (subera5$ISO %in% grp1) * subera5.lag$t2m * coeffs[4] +
                (subera5$ISO %in% grp2) * subera5.lag$t2m * coeffs[5] +
                (subera5$ISO %in% grp3) * subera5.lag$t2m * coeffs[6]
        }
        dimpact[!(subera5$ISO %in% c(grp1, grp2, grp3))] <- NA

        subera5.lag <<- subera5

        dimpact
    }

    list(setup=setup, simulate=simulate)
}

if (F) {
    funcs <- get.funcs('Table 3, CCE3, Col 6')
    oneres <- project.single(funcs$setup, funcs$simulate)
    plot((oneres %>% filter(ISO == 'NOR'))$dimpact)
    plot((oneres %>% filter(ISO == 'THA'))$dimpact)
}
