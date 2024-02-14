setwd("~/research/currentloss")

library(dplyr)
library(ggplot2)

if (F) {
    ## Compare pop-weighted and area-weighted

    df1 <- read.csv("rawt2m/era5-t2m-1940.csv")
    df2 <- read.csv("popt2m/era5-t2m-1940.csv")

    df <- df1 %>% group_by(ADMIN, ADM0_A3) %>% summarize(t2m=mean(t2m)) %>%
        left_join(df2 %>% group_by(ADMIN, ADM0_A3) %>% summarize(t2m=mean(t2m)), by='ADM0_A3', suffix=c('.area', '.pop'))

    ggplot(subset(df, t2m.pop > 0), aes(t2m.area - 273.15, t2m.pop - 273.15)) +
        geom_abline(intercept=0, slope=1) +
        geom_label(aes(label=ADM0_A3, fill=t2m.area > t2m.pop), label.padding=unit(0.1, "lines"), size=2) +
        theme_bw() + scale_fill_discrete("Population adjustment", labels=c("Pop. > Area", "Area > Pop.")) +
        ylim(265 - 273.15, 301.5 - 273.15) + xlab("Area-weighted temperature in 1940 (°C)") +
        ylab("Population-weighted temperature in 1940 (°C)")
    ggsave("figures/poparea.pdf", width=6.5, height=5)
}

available.years <- 1940:2022

df.raw.1940 <- read.csv("rawt2m/era5-t2m-1940.csv") %>% group_by(ADM0_A3) %>% summarize(t2m=mean(t2m))
df.pop.1940 <- read.csv("popt2m/era5-t2m-1940.csv") %>% group_by(ADM0_A3) %>% summarize(t2m=mean(t2m))
df.raw.1990 <- read.csv("rawt2m/era5-t2m-1990.csv") %>% group_by(ADM0_A3) %>% summarize(t2m=mean(t2m))
df.pop.1990 <- read.csv("popt2m/era5-t2m-1990.csv") %>% group_by(ADM0_A3) %>% summarize(t2m=mean(t2m))
df.raw.2022 <- read.csv("rawt2m/era5-t2m-2022.csv") %>% group_by(ADM0_A3) %>% summarize(t2m=mean(t2m))
df.pop.2022 <- read.csv("popt2m/era5-t2m-2022.csv") %>% group_by(ADM0_A3) %>% summarize(t2m=mean(t2m))

df.bc <- df.raw.1940 %>% left_join(df.pop.1940, by='ADM0_A3', suffix=c('', '.pop.1940')) %>%
    left_join(df.raw.1990, by='ADM0_A3', suffix=c('', '.raw.1990')) %>%
    left_join(df.pop.1990, by='ADM0_A3', suffix=c('', '.pop.1990')) %>%
    left_join(df.raw.2022, by='ADM0_A3', suffix=c('', '.raw.2022')) %>%
    left_join(df.pop.2022, by='ADM0_A3', suffix=c('.raw.1940', '.pop.2022'))

df.bc$t2m.pop.1940[df.bc$t2m.pop.1940 == 0] <- df.bc$t2m.raw.1940[df.bc$t2m.pop.1940 == 0]
df.bc$t2m.pop.1990[df.bc$t2m.pop.1990 == 0] <- df.bc$t2m.raw.1990[df.bc$t2m.pop.1990 == 0]
df.bc$t2m.pop.2022[df.bc$t2m.pop.2022 == 0] <- df.bc$t2m.raw.2022[df.bc$t2m.pop.2022 == 0]

df.bc$diff.1940 <- df.bc$t2m.pop.1940 - df.bc$t2m.raw.1940
df.bc$diff.1990 <- df.bc$t2m.pop.1990 - df.bc$t2m.raw.1990
df.bc$diff.2022 <- df.bc$t2m.pop.2022 - df.bc$t2m.raw.2022
df.bc$diff.slope.early <- (df.bc$diff.1990 - df.bc$diff.1940) / (1990 - 1940)
df.bc$diff.slope.late <- (df.bc$diff.2022 - df.bc$diff.1990) / (2022 - 1990)

combo <- data.frame()
for (year in available.years) {
    print(year)
    if (year < 1990) {
        df.bc2 <- data.frame(ADM0_A3=df.bc$ADM0_A3, toadd=df.bc$diff.1940 + df.bc$diff.slope.early * (year - 1940))
    } else if (year > 1990) {
        df.bc2 <- data.frame(ADM0_A3=df.bc$ADM0_A3, toadd=df.bc$diff.2022 + df.bc$diff.slope.late * (year - 1990))
    } else
        df.bc2 <- data.frame(ADM0_A3=df.bc$ADM0_A3, toadd=df.bc$diff.1990)
    df <- read.csv(paste0("rawt2m/era5-t2m-", year, ".csv"))
    df2a <- df %>% group_by(ADMIN, ADM0_A3) %>% summarize(t2m.raw=mean(t2m)) %>% left_join(df.bc2, by='ADM0_A3') %>% mutate(t2m=t2m.raw + toadd)
    df$month <- sapply(df$time, function(dd) substring(dd, 1, 7))
    df2b <- df %>% group_by(month, ADMIN, ADM0_A3) %>%
        summarize(t2mavg=mean(t2m), t2mmin=min(t2mmin), t2mmax=max(t2mmax), t2mvar=var(t2m)) %>%
        group_by(ADMIN, ADM0_A3) %>% summarize(t2mminavg=mean(t2mmin), t2mmaxavg=mean(t2mmax),
                                               t2mavgmin=min(t2mavg), t2mavgmax=max(t2mavg),
                                               t2mavgvar=var(t2mavg), t2mvaravg=mean(t2mvar))
    df2b2 <- df2b %>% left_join(df.bc2, by='ADM0_A3') %>%
        mutate(t2mminavg=t2mminavg + toadd, t2mmaxavg=t2mmaxavg + toadd, t2mavgmin=t2mavgmin + toadd,
               t2mavgmax=t2mavgmax + toadd, t2mavgvar=t2mavgvar, t2mvaravg=t2mvaravg)
    stopifnot(all(df2a$ADM0_A3 == df2b2$ADM0_A3))
    df3 <- data.frame(Year=year, Country=df2a$ADMIN, ISO=df2a$ADM0_A3, t2m=df2a$t2m,
                      t2mminavg=df2b2$t2mminavg, t2mmaxavg=df2b2$t2mmaxavg,
                      t2mavgmin=df2b2$t2mavgmin, t2mavgmax=df2b2$t2mavgmax,
                      t2mavgvar=df2b2$t2mavgvar, t2mvaravg=df2b2$t2mvaravg)

    combo <- rbind(combo, df3)
}

all(table(combo$ISO) == table(combo$ISO)[1])

if (F) {
    ## Until all downloaded, add on google results
    era5.goog <- read.csv("yearly_means.csv")

    iso.2.adm <- data.frame(Country=df2a$ADMIN, ISO=df2a$ADM0_A3)

    era5.goog2 <- era5.goog %>% left_join(iso.2.adm, by='ISO')
    unique(era5.goog2$ISO[is.na(era5.goog2$Country)])
    unique(combo$ISO)[!(unique(combo$ISO) %in% unique(era5.goog2$ISO))]
    era5.goog2 <- era5.goog2[!is.na(era5.goog2$Country) & era5.goog2$Year > max(combo$Year),]

    combo <- rbind(combo, data.frame(Year=era5.goog2$Year, Country=era5.goog2$Country, ISO=era5.goog2$ISO, t2m=era5.goog2$mean_2m_air_temperature,
                                     t2mmin=era5.goog2$minimum_2m_air_temperature, t2mmax=era5.goog2$maximum_2m_air_temperature))
}

## Drop extremely cold regions
combo <- subset(combo, !(ISO %in% c('ATA', 'GRL', 'KAS')))

write.csv(combo, "era5-t2m-combo.csv", row.names=F)
## combo <- read.csv("era5-t2m-combo.csv")

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

