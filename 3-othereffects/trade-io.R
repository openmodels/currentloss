## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(raster)
library(dplyr)

comtrade <- rbind(read.csv("data/trade/uncomtrade-1992.csv"), read.csv("data/trade/uncomtrade-2002.csv"),
                  read.csv("data/trade/uncomtrade-2012.csv"), read.csv("data/trade/uncomtrade-2022.csv"))

io.byyear <- list() # "YEAR"=list("TT"=matrix, labels=data.frame)
fd.byyear <- list() # "YEAR"=list("FD"=matrix, labels=data.frame)

load.fd <- function(year) {
    if (year < 1990)
        return(load.fd(1990))
    if (year > 2016)
        return(load.fd(2016))

    yearstr <- as.character(year)
    if (yearstr %in% names(fd.byyear))
        return(fd.byyear[[yearstr]])

    datapath <- paste0("data/I_O data/Eora26/Eora26_", year, "_bp")
    VA <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_VA.txt")), sep='\t', header=F))
    FD <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_FD.txt")), sep='\t', header=F))

    FD2 <- matrix(0, nrow(FD), ncol(FD) / 6)
    for (ii in 1:6)
        FD2[,] <- FD2 + FD[, seq(ii, ncol(FD), by=6)]

    labels <- read.delim(file.path(datapath, "labels_T.txt"), sep='\t', header=F)
    labels$V1 <- factor(labels$V1, levels=unique(labels$V1))

    labels$VA <- colSums(VA)

    labels2 <- labels %>% group_by(V1) %>% summarize(VA=sum(VA))

    fd.byyear[[yearstr]] <<- list(FD=FD2, labels=labels2)
    return(fd.byyear[[yearstr]])
}

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

    fd.data <- load.fd(year)
    labels2 <- fd.data$labels

    datapath <- paste0("data/I_O data/Eora26/Eora26_", year, "_bp")
    TT <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_T.txt")), sep='\t', header=F))

    rTT <- raster(TT)
    rTT2 <- aggregate(rTT, 26, sum)
    TT2 <- as.matrix(rTT2)

    labels2$FD <- colSums(fd.data$FD)

    io.byyear[[yearstr]] <<- list(TT=TT2, labels=labels2)
    return(io.byyear[[yearstr]])
}

calc.domar.change <- function(year, isos, dimpact) {
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

    total.trade.effect
}

calc.domar.distribute.method1 <- function(year, isos, dimpact) {
    domar.change <- calc.domar.change(year, isos, dimpact)

    ## Distribute domar loss
    impacts <- data.frame(ISO=isos, dimpact)
    thisyear <- data.frame()
    for (iso in isos) {
        comtrade.iso <- subset(comtrade, ReporterISO == iso & PartnerISO != 'W00')
        if (nrow(comtrade.iso) == 0)
            next

        maxgrow <- max(0, dimpact[isos == iso])

        if (year <= min(comtrade.iso$Period)) {
            calcdf <- subset(comtrade.iso, Period == min(comtrade.iso$Period)) %>% left_join(impacts, by=c('PartnerISO'='ISO'))
        } else if (year >= max(comtrade.iso$Period)) {
            calcdf <- subset(comtrade.iso, Period == max(comtrade.iso$Period)) %>% left_join(impacts, by=c('PartnerISO'='ISO'))
        } else if (year %in% comtrade.iso$Period) {
            calcdf <- subset(comtrade.iso, Period == year) %>% left_join(impacts, by=c('PartnerISO'='ISO'))
        } else {
            yearbefore <- max(comtrade.iso$Period[comtrade.iso$Period < year])
            yearafter <- min(comtrade.iso$Period[comtrade.iso$Period > year])
            portionafter <- (year - yearbefore) / (yearafter - yearbefore)
            comtrade.mix <- subset(comtrade.iso, Period == yearbefore) %>% full_join(subset(comtrade.iso, Period == yearafter), by=c('PartnerISO', 'FlowDesc'), suffix=c('.bef', '.aft'))
            comtrade.mix$Cifvalue <- ifelse(is.na(comtrade.mix$Cifvalue.bef), comtrade.mix$Cifvalue.aft,
                                     ifelse(is.na(comtrade.mix$Cifvalue.aft), comtrade.mix$Cifvalue.bef,
                                            portionafter * comtrade.mix$Cifvalue.aft + (1 - portionafter) * comtrade.mix$Cifvalue.bef))
            comtrade.mix$Fobvalue <- ifelse(is.na(comtrade.mix$Fobvalue.bef), comtrade.mix$Fobvalue.aft,
                                     ifelse(is.na(comtrade.mix$Fobvalue.aft), comtrade.mix$Fobvalue.bef,
                                            portionafter * comtrade.mix$Fobvalue.aft + (1 - portionafter) * comtrade.mix$Fobvalue.bef))
            calcdf <- comtrade.mix %>% left_join(impacts, by=c('PartnerISO'='ISO'))
        }

        ## Limit any growth to growth of country
        calcdf$cif.lost <- calcdf$Cifvalue * pmax(-calcdf$dimpact, -maxgrow)
        calcdf$fob.lost <- calcdf$Fobvalue * pmax(-calcdf$dimpact, -maxgrow)

        ## Fill in NAs, with preference based on direction
        calcdf$fob.lost[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export'] <- calcdf$cif.lost[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export']
        calcdf$Fobvalue[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export'] <- calcdf$Cifvalue[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export']
        calcdf$cif.lost[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import'] <- calcdf$fob.lost[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import']
        calcdf$Cifvalue[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import'] <- calcdf$Fobvalue[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import']

        fracloss.import <- sum(calcdf$cif.lost[calcdf$FlowDesc == 'Import'], na.rm=T) / sum(calcdf$Cifvalue[calcdf$FlowDesc == 'Import'], na.rm=T)
        fracloss.export <- sum(calcdf$fob.lost[calcdf$FlowDesc == 'Export'], na.rm=T) / sum(calcdf$Fobvalue[calcdf$FlowDesc == 'Export'], na.rm=T)

        thisyear <- rbind(thisyear, data.frame(ISO=iso, year, fracloss.import, fracloss.export))
    }

    thisyear2 <- thisyear %>% left_join(df.gdp3, by=c('ISO'='Country Code', 'year'='Year'))
    ## domar.change * sum(thisyear2$GDP.2019.est) = A * sum(thisyear2$fracloss.export * thisyear2$GDP.2019.est)
    ## scaleby <- domar.change * sum(thisyear2$GDP.2019.est, na.rm=T) / sum(ifelse(is.na(thisyear2$fracloss.export), 0, thisyear2$fracloss.export) * thisyear2$GDP.2019.est, na.rm=T)
    ## log(domar.change * sum(thisyear2$GDP.2019.est)) = log(A) + log(sum(thisyear2$fracloss.export * thisyear2$GDP.2019.est))

    list(global=data.frame(domar.change, global.gdp=sum(thisyear2$GDP.2019.est, na.rm=T), global.fracloss=sum(ifelse(is.na(thisyear2$fracloss.export), 0, thisyear2$fracloss.export) * thisyear2$GDP.2019.est, na.rm=T)),
         thisyear2=thisyear2)
}

calc.domar.distribute.method2 <- function(scaleby, isos, thisyear2) {
    thisyear2$tradeloss <- thisyear2$fracloss.export * scaleby

    domar.loss2 <- data.frame(ISO=isos) %>% left_join(thisyear2)
    domar.loss2$tradeloss
}

calc.final.demand.method <- function(year, isos, dimpact) {
    fd.data <- load.fd(year)
    FD2 <- fd.data$FD
    labels2 <- fd.data$labels

    FD3 <- matrix(0, ncol(FD2) - 1, ncol(FD2))
    for (ii in 1:26)
        FD3[,] <- FD3 + FD2[seq(ii, nrow(FD2) - 1, by=26),]

    FD4 <- rbind(FD3, FD2[nrow(FD2),])
    diag(FD4) <- 0

    ## Match up known impacts to IO countries
    isos[isos == 'SDN'] <- 'SUD'
    isos[isos == 'PSX'] <- 'PSE'
    labels3 <- labels2 %>% left_join(data.frame(V1=isos, dimpact=dimpact), by='V1')
    labels3$dimpact[labels3$V1 == 'ANT'] <- labels3$dimpact[labels3$V1 == 'NLD']
    labels3$dimpact[is.na(labels3$dimpact)] <- 0

    labels3$fdchg <- as.numeric(t(FD4) %*% labels3$dimpact)
    labels3$FD <- colSums(FD2)
    labels3$tradeloss <- -labels3$fdchg / (labels3$FD + labels3$VA)

    result <- data.frame(ISO=isos, dimpact) %>% left_join(labels3, by=c('ISO'='V1'))
    result$tradeloss[is.na(result$tradeloss)] <- 0

    result$tradeloss
}

calc.leontief.method <- function(year, isos, dimpact) {
    io <- load.io(year)

    ## Match up known impacts to IO countries
    labels <- io$labels
    isos[isos == 'SDN'] <- 'SUD'
    isos[isos == 'PSX'] <- 'PSE'
    labels2 <- labels %>% left_join(data.frame(V1=isos, dimpact=dimpact), by='V1')
    labels2$dimpact[labels2$V1 == 'ANT'] <- labels2$dimpact[labels2$V1 == 'NLD']
    labels2$dimpact[is.na(labels2$dimpact)] <- 0

    total.sales <- rowSums(io$TT) + labels2$FD

    AA <- io$TT
    for (ii in 1:ncol(AA))
        AA[, ii] <- io$TT[, ii] / total.sales[ii]
    LL <- solve(diag(ncol(AA)) - AA)
    labels2$allloss <- as.numeric(-(LL %*% labels2$dimpact))

    result <- data.frame(ISO=isos, dimpact) %>% left_join(labels2, by=c('ISO'='V1'), suffix=c('', '.x'))
    result$tradeloss <- result$allloss + result$dimpact
    result$tradeloss[is.na(result$tradeloss)] <- 0

    result$tradeloss
}


if (F) {
    ## Test
    load("data/mcrfres-0.08.RData")
    losses <- subset(results, mc == 1 & Year == 2015)
    isos <- losses$ISO
    dimpact <- losses$dimpact

    calc.domar.change(2015, isos, dimpact)
}
