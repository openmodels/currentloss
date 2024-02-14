library(PBSmapping)

polydata <- attr(importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

get.poors <- function(baseyear) {
    gdppcs <- read.csv("data/socioeconomics/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_3630804/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_3630804.csv", skip=3)
    gdppc0s <- data.frame()
    for (ii in 1:nrow(gdppcs)) {
        nonnas <- which(!is.na(gdppcs[ii, which(names(gdppcs) == paste0('X', baseyear)):ncol(gdppcs)]))
        if (length(nonnas) == 0)
            next
        year0 <- nonnas[1] + baseyear - 1
        gdppc0s <- rbind(gdppc0s, data.frame(ADM0=gdppcs$Country.Code[ii], year0, gdppc0=gdppcs[ii, paste0('X', year0)]))
    }

    gdppc0s$ADM0[gdppc0s$gdppc0 < median(gdppc0s$gdppc0, na.rm=T)]
}

vcv.from.vals <- function(gammavcv.vals) {
    ## For NxN, we get N*(N+1)/2 = k
    ## N2 + N - 2 k = 0 -> (sqrt(4*2*k + 1) - 1) / 2
    NN <- (sqrt(4*2*length(gammavcv.vals) + 1) - 1) / 2

    gammavcv <- matrix(NA, nrow=NN, ncol=NN)
    gammavcv[upper.tri(gammavcv, diag=T)] <- gammavcv.vals
    gammavcv[lower.tri(gammavcv)] <- t(gammavcv)[lower.tri(gammavcv)]

    gammavcv
}

get.africa <- function() {
    polydata$ADM0_A3[polydata$CONTINENT == 'Africa']
}
