setwd("~/research/currentloss")

library(dplyr)
library(ggplot2)

available.years <- 1940:2023

combo <- data.frame()
for (year in available.years) {
    print(year)
    df <- read.csv(paste0("popt2m/era5-t2m-", year, "-adm0.csv"))
    ## In 2023, got doubled observations-- one normal, one 0 or -30 C; Otherwise, only 1 obs per time x ADM0, so no effect
    df <- df %>% group_by(time, ADMIN, ADM0_A3) %>% summarize(t2m=max(t2m), t2mmin=max(t2mmin), t2mmax=max(t2mmax))
    df2a <- df %>% group_by(ADMIN, ADM0_A3) %>% summarize(t2m=mean(t2m))
    df$month <- sapply(df$time, function(dd) substring(dd, 1, 7))
    df2b <- df %>% group_by(month, ADMIN, ADM0_A3) %>%
        summarize(t2mavg=mean(t2m), t2mmin=min(t2mmin), t2mmax=max(t2mmax), t2mvar=var(t2m)) %>%
        group_by(ADMIN, ADM0_A3) %>% summarize(t2mminavg=mean(t2mmin), t2mmaxavg=mean(t2mmax),
                                               t2mavgmin=min(t2mavg), t2mavgmax=max(t2mavg),
                                               t2mavgvar=var(t2mavg), t2mvaravg=mean(t2mvar))
    stopifnot(all(df2a$ADM0_A3 == df2b$ADM0_A3))
    df3 <- data.frame(Year=year, Country=df2a$ADMIN, ISO=df2a$ADM0_A3, t2m=df2a$t2m,
                      t2mminavg=df2b$t2mminavg, t2mmaxavg=df2b$t2mmaxavg,
                      t2mavgmin=df2b$t2mavgmin, t2mavgmax=df2b$t2mavgmax,
                      t2mavgvar=df2b$t2mavgvar, t2mvaravg=df2b$t2mvaravg)

    combo <- rbind(combo, df3)
}

all(table(combo$ISO) == table(combo$ISO)[1])

## Drop extremely cold regions
combo <- subset(combo, !(ISO %in% c('ATA', 'GRL', 'KAS')))

write.csv(combo, "era5-t2m-combo-adm0.csv", row.names=F)

combo <- data.frame()
for (year in available.years) {
    print(year)
    for (seg in 0:22) {
        df <- read.csv(paste0("popt2m/era5-t2m-", year, "-adm1-", seg, ".csv"))
        ## In 2023, got doubled observations-- one normal, one 0 or -30 C; Otherwise, only 1 obs per time x ADM1, so no effect
        df <- df %>% group_by(time, admin, adm0_a3, name, diss_me) %>% summarize(t2m=max(t2m), t2mmin=max(t2mmin), t2mmax=max(t2mmax))
        df2a <- df %>% group_by(admin, adm0_a3, name, diss_me) %>% summarize(t2m=mean(t2m))
        df$month <- sapply(df$time, function(dd) substring(dd, 1, 7))
        df2b <- df %>% group_by(month, admin, adm0_a3, name, diss_me) %>%
            summarize(t2mavg=mean(t2m), t2mmin=min(t2mmin), t2mmax=max(t2mmax), t2mvar=var(t2m)) %>%
            group_by(admin, adm0_a3, name, diss_me) %>% summarize(t2mminavg=mean(t2mmin), t2mmaxavg=mean(t2mmax),
                                               	    t2mavgmin=min(t2mavg), t2mavgmax=max(t2mavg),
                                               	    t2mavgvar=var(t2mavg), t2mvaravg=mean(t2mvar))
        stopifnot(all(df2a$name == df2b$name))
        df3 <- data.frame(Year=year, Country=df2a$admin, ISO=df2a$adm0_a3, ADM1=df2a$name, ADM1_Code=df2a$diss_me, t2m=df2a$t2m,
                          t2mminavg=df2b$t2mminavg, t2mmaxavg=df2b$t2mmaxavg,
                          t2mavgmin=df2b$t2mavgmin, t2mavgmax=df2b$t2mavgmax,
                          t2mavgvar=df2b$t2mavgvar, t2mvaravg=df2b$t2mvaravg)

        combo <- rbind(combo, df3)
    }
}

all(table(combo$ADM1_Code) == table(combo$ADM1_Code)[1])

write.csv(combo, "era5-t2m-combo-adm1.csv", row.names=F)

## combo <- read.csv("era5-t2m-combo-adm0.csv")
ggplot(combo, aes(Year, t2m - 273.15, group=ISO)) +
    geom_line(alpha=.5, size=.1) + theme_bw() +
    scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Annual average temperature")

gmst <- read.csv("pres/gmst.csv")
gmst$Anomaly.base <- (gmst %>% filter(Year >= 1940 & Year < 1959) %>% dplyr::summarize(Anomaly.base=mean(Anomaly)))$Anomaly.base
gmst %>% filter(Year >= 1850 & Year <= 1900) %>% dplyr::summarize(preind.base=mean(Anomaly)) # -0.166 C

library(PBSmapping)
polydata <- attr(importShapefile("regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"), 'PolyData')

combo.glopop <- combo %>% left_join(polydata, by=c('ISO'='ADM0_A3')) %>% group_by(Year) %>%
    dplyr::summarize(t2m=sum(t2m * POP_EST) / sum(POP_EST))

pdf <- rbind(combo[, c('Year', 't2m', 'ISO')], cbind(combo.glopop, ISO='GLO'))
pdf2 <- pdf %>% left_join(pdf %>% filter(Year >= 1940 & Year < 1959) %>% group_by(ISO) %>% dplyr::summarize(t2m.base=mean(t2m)))

## Plot 1: GMST only
ggplot(pdf2, aes(Year, t2m - t2m.base)) +
    geom_line(data=subset(pdf2, Year > 1950 & ISO != 'GLO'), aes(group=ISO, colour="Country temperatures"), alpha=0, size=.1) +
    geom_line(data=subset(gmst, Year > 1950), aes(y=Anomaly - Anomaly.base, colour="Global mean surface temperature")) +
    geom_line(data=subset(pdf2, Year > 1950 & ISO == 'GLO'), aes(colour="Pop-Weighted temperature"), alpha=0) +
    geom_hline(yintercept=0) +
    theme_bw() + coord_cartesian(ylim=c(-1.5, 2.5)) +
    scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Annual average temperature, relative to 1950 (°C)") +
    scale_colour_discrete(NULL, breaks=c("Global mean surface temperature", "Pop-Weighted temperature", "Country temperatures")) +
    theme(legend.justification=c(0,1), legend.position=c(0.01,.99))

## Plot 2: Add on pop-weighted
ggplot(pdf2, aes(Year, t2m - t2m.base)) +
    geom_line(data=subset(pdf2, Year > 1950 & ISO != 'GLO'), aes(group=ISO, colour="Country temperatures"), alpha=0, size=.1) +
    geom_line(data=subset(gmst, Year > 1950), aes(y=Anomaly - Anomaly.base, colour="Global mean surface temperature")) +
    geom_line(data=subset(pdf2, Year > 1950 & ISO == 'GLO'), aes(colour="Pop-Weighted temperature")) +
    geom_hline(yintercept=0) +
    theme_bw() + coord_cartesian(ylim=c(-1.5, 2.5)) +
    scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Annual average temperature, relative to 1950 (°C)") +
    scale_colour_discrete(NULL, breaks=c("Global mean surface temperature", "Pop-Weighted temperature", "Country temperatures")) +
    theme(legend.justification=c(0,1), legend.position=c(0.01,.99))

## Plot 3: All
ggplot(pdf2, aes(Year, t2m - t2m.base)) +
    geom_line(data=subset(pdf2, Year > 1950 & ISO != 'GLO'), aes(group=ISO, colour="Country temperatures"), alpha=.5, size=.1) +
    geom_line(data=subset(gmst, Year > 1950), aes(y=Anomaly - Anomaly.base, colour="Global mean surface temperature")) +
    geom_line(data=subset(pdf2, Year > 1950 & ISO == 'GLO'), aes(colour="Pop-Weighted temperature")) +
    geom_hline(yintercept=0) +
    theme_bw() + coord_cartesian(ylim=c(-1.5, 2.5)) +
    scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Annual average temperature, relative to 1950 (°C)") +
    scale_colour_discrete(NULL, breaks=c("Global mean surface temperature", "Pop-Weighted temperature", "Country temperatures")) +
    theme(legend.justification=c(0,1), legend.position=c(0.01,.99))

