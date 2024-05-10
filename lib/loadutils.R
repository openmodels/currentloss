library(readxl)
library(reshape2)

read.wb <- function(filepath, value.name) {
    df <- read_xls(filepath, skip=3)
    df2 <- melt(df[, c(-1, -3, -4)], 'Country Code', variable.name='Year', value.name=value.name)
    df2$Year <- as.numeric(as.character(df2$Year))
    df2
}

load.gdp3 <- function() {
    df.gdp2 <- read.wb("data/capital/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_5871893.xls", 'GDP.2015')
    df.gdp3 <- subset(df.gdp2, `Country Code` %in% unique(df.gdp2$`Country Code`[!is.na(df.gdp2$GDP.2015)]) & !(`Country Code` %in% c("LIE", 'NCL'))) %>% group_by(`Country Code`) %>%
        reframe(Year=Year, GDP.2015.est=approx(Year, GDP.2015, Year, rule=2)$y)
    df.gdp3$GDP.2019.est <- df.gdp3$GDP.2015.est * 106.87654 / 100

    df.gdp3.last <- subset(df.gdp3, Year == 2022)
    df.gdp3.last$Year <- 2023
    df.gdp3 <- rbind(df.gdp3, df.gdp3.last)

    df.gdp3
}

load.slr2 <- function(df.gdp3) {
    slr <- read.csv("data/slrbyadm0-final.csv")
    slr2 <- slr %>% left_join(df.gdp3, by=c('ISO'='Country Code', 'year'='Year')) %>%
        group_by(ISO, year) %>% reframe(mc=1:30, slrloss=rnorm(30, mu / GDP.2019.est, ((q83 - q17) / diff(qnorm(c(.17, .83)))) / GDP.2019.est))
    slr2
}

load.tradeloss <- function(method, persist) {
    tradeloss.all <- data.frame()
    for (year in 1940:2023) {
        load(paste0(paste0("data/tradeloss-", method, "/tradeloss-", year, "-", persist, ".RData")))
        tradeloss.all <- rbind(tradeloss.all, tradeloss)
    }

    tradeloss.all
}

load.solowsum <- function(persist, trade.method) {
    solowsum <- data.frame()
    for (mc in 1:30) {
        filepath <- paste0("data/solow-", persist, "-", trade.method, "/solow-v4-", persist, "-", mc, ".csv")
        if (file.exists(filepath))
            solowsum <- rbind(solowsum, read.csv(filepath))
    }

    solowsum
}
