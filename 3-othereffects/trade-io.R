## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(raster)
library(dplyr)

io.byyear <- list() # "YEAR"=list("TT"=matrix, labels=data.frame)

## Returns list("TT"=matrix, labels=data.frame)
## Caches the result in io.byyear
load.io <- function(year) {
    if (year < 1990)
        return(load.io(1990))
    if (year > 2016)
        return(load.io(2016))

    yearstr <- as.character(year)
    if (yearstr %in% names(io.byyear))
        return(io.byyear[[yearstr]])

    datapath <- paste0("data/I_O data/Eora26/Eora26_", year, "_bp")
    TT <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_T.txt")), sep='\t', header=F))
    VA <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_VA.txt")), sep='\t', header=F))
    FD <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_FD.txt")), sep='\t', header=F))

    rTT <- raster(TT)
    rTT2 <- aggregate(rTT, 26, sum)
    TT2 <- as.matrix(rTT2)

    labels <- read.delim(file.path(datapath, "labels_T.txt"), sep='\t', header=F)
    labels$V1 <- factor(labels$V1, levels=unique(labels$V1))

    labels$VA <- colSums(VA)

    labels2 <- labels %>% group_by(V1) %>% summarize(VA=sum(VA))

    FD2 <- matrix(0, nrow(FD), ncol(TT2))
    for (ii in 1:6)
        FD2[,] <- FD2 + FD[, seq(ii, ncol(FD), by=6)]
    labels2$FD <- colSums(FD2)

    io.byyear[[yearstr]] <<- list(TT=TT2, labels=labels2)
    return(io.byyear[[yearstr]])
}

calc.domar.loss <- function(year, isos, dimpact) {
    io <- load.io(year)

    ## Match up known impacts to IO countries
    labels <- io$labels
    isos[isos == 'SDN'] <- 'SUD'
    isos[isos == 'PSX'] <- 'PSE'
    labels2 <- labels %>% left_join(data.frame(V1=isos, dimpact=dimpact), by='V1')
    labels2$dimpact[labels2$V1 == 'ANT'] <- labels2$dimpact[labels2$V1 == 'NLD']

    ## Calculate Domar weights
    total.sales <- rowSums(io$TT) + labels2$FD
    labels2$gdp <- labels2$FD + labels2$VA
    global.gdp <- sum(labels2$gdp)
    weights <- total.sales / global.gdp

    ## Calculate global GDP loss
    dimpact.level <- exp(labels2$dimpact) - 1
    total.change <- sum(weights * ifelse(is.na(dimpact.level), 0, dimpact.level))

    ## Extract out the additional
    total.trade.effect <- total.change - sum(dimpact.level * labels2$gdp, na.rm=T) / global.gdp

    return(rep(total.trade.effect, length(isos)))
}

calc.final.demand.method <- function(year, isos, dimpact) {
    ## ...
    ## return([trade loss for each iso])
}

calc.leontief.method <- function(year, isos, dimpact) {
    ## ...
    ## return([trade loss for each iso])
}


if (F) {
    ## Test
    load("data/mcrfres-0.08.RData")
    losses <- subset(results, mc == 1 & Year == 2015)
    isos <- losses$ISO
    dimpact <- losses$dimpact

    calc.domar.loss(2015, isos, dimpact)
}
