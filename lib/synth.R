library(sf)

get.weighted.mcts <- function(allyr.ww, iso.weight, do.for.subset) {
    df.gdp2 <- read.wb("data/capital/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_5871893.xls", 'GDP.2015')
    df.gdp2.last <- df.gdp2 %>% group_by(`Country Code`) %>%
        dplyr::summarize(GDP.Year=ifelse(any(!is.na(GDP.2015)), Year[tail(which(!is.na(GDP.2015)), 1)], NA),
                         GDP.2015=ifelse(any(!is.na(GDP.2015)), GDP.2015[tail(which(!is.na(GDP.2015)), 1)], NA))

    df.pop2 <- read.wb("data/capital/API_SP.POP.TOTL_DS2_en_excel_v2_5871620.xls", 'POP')
    df.pop3 <- df.pop2 %>% group_by(`Country Code`) %>%
        dplyr::summarize(POP.Year=ifelse(any(!is.na(POP)), Year[tail(which(!is.na(POP)), 1)], NA),
                         POP=ifelse(any(!is.na(POP)), POP[tail(which(!is.na(POP)), 1)], NA))

    allyr2 <- allyr.ww %>%
        left_join(df.gdp2.last, by = c('ISO' = 'Country Code')) %>%
        left_join(df.pop3, by = c('ISO' = 'Country Code')) %>%
        left_join(polydata, by = c('ISO' = 'ADM0_A3'))

    if (do.for.subset == "L+MIC") {
        allyr2 <- allyr2 %>%
            filter(INCOME_GRP %in% c("5. Low income", "4. Lower middle income", "3. Upper middle income"))
    }

    if (iso.weight == 'pop')
        allyr2$iso.weight <- allyr2$POP
    else
        allyr2$iso.weight <- allyr2$GDP.2015

    allyr3 <- allyr2 %>% group_by(mc, Year) %>%
        dplyr::summarize(totimpact = wtd.mean(totimpact, weights = iso.weight, normwt = T),
                         slrloss = wtd.mean(slrloss, weights = iso.weight, normwt = T),
                         tradeloss = wtd.mean(tradeloss, weights = iso.weight, normwt = T),
                         solow = ifelse(all(is.na(product.chg)), NA, wtd.mean(log2lev(product.chg - totimpact - tradeloss - slrloss), weights = iso.weight, normwt = T)),
                         total = ifelse(all(is.na(product.chg)), wtd.mean(log2lev(totimpact - tradeloss - slrloss), weights = iso.weight, normwt = T), wtd.mean(log2lev(product.chg), weights = iso.weight, normwt = T)),
                         weight2 = wtd.mean(weight.norm, weights = iso.weight))

    allyr3
}

get.weighted.ts <- function(allyr.ww, iso.weight, do.for.subset) {
    allyr3 <- get.weighted.mcts(allyr.ww, iso.weight, do.for.subset) %>%
        group_by(Year) %>%
        dplyr::summarize(solow = ifelse(all(is.na(total)), NA, wtd.median(total - totimpact - tradeloss - slrloss, weights = weight2, normwt = T)),
                         prod25 = ifelse(all(is.na(total)), ifelse(all(is.na(totimpact)), NA, wtd.quantile(totimpact - tradeloss - slrloss, .25, weights = weight2, normwt = T)), wtd.quantile(total, .25, weights = weight2, normwt = T)),
                         prod75 = ifelse(all(is.na(total)), ifelse(all(is.na(totimpact)), NA, wtd.quantile(totimpact - tradeloss - slrloss, .75, weights = weight2, normwt = T)), wtd.quantile(total, .75, weights = weight2, normwt = T)),
                         total = ifelse(all(is.na(total)), ifelse(all(is.na(totimpact)), NA, wtd.median(totimpact - tradeloss - slrloss, weights = weight2, normwt = T)), wtd.median(total, weights = weight2, normwt = T)),
                         totimpact = ifelse(all(is.na(totimpact)), NA, wtd.median(totimpact, weights = weight2, normwt = T)),
                         slrloss = ifelse(all(is.na(slrloss)), NA, wtd.median(slrloss, weights = weight2, normwt = T)),
                         tradeloss = ifelse(all(is.na(tradeloss)), NA, wtd.median(tradeloss, weights = weight2, normwt = T)))

    allyr3
}

log2lev <- function(xx) {
    exp(xx) - 1
}

prep.levels.allyr.ww <- function(allyr.ww) {
    allyr.ww$total <- ifelse(is.na(allyr.ww$product.chg), allyr.ww$totimpact - allyr.ww$tradeloss - allyr.ww$slrloss, allyr.ww$product.chg)

    polydata <- st_read("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
    df.gdp3 <- load.gdp3()

    df.pro2b <- read.iw("data/capital/tabula-C-produced.csv", 'Produced Capital') %>%
        filter(!is.na(ISO)) %>% group_by(ISO) %>%
        reframe(`Produced Capital Est` = approx(Year, `Produced Capital`, 1960:2023, rule=2)$y, Year=1960:2023)

    df.ren2b <- read.iw("data/capital/tabula-A2-renewable.csv", 'Renewable Capital') %>%
        filter(!is.na(ISO)) %>% group_by(ISO) %>%
        reframe(`Renewable Capital Est` = approx(Year, `Renewable Capital`, 1960:2023, rule=2)$y, Year=1960:2023)

    allyr.ww %>%
        left_join(polydata, by = c('ISO' = 'ADM0_A3')) %>%
        left_join(df.gdp3, by = c('Year', 'ISO' = 'Country Code')) %>%
        left_join(df.pro2b, by = c('Year', 'ISO')) %>%
        left_join(df.ren2b, by = c('Year', 'ISO')) %>%
        ## want f * NoCC, But (1+f) * NoCC = Obs, So (f / (1+f)) Obs
        mutate(total.usd=(log2lev(total) / (1 + log2lev(total))) * GDP.2015.est / 1e9,
               totimpact.usd=(log2lev(totimpact) / (1 + log2lev(totimpact))) * GDP.2015.est / 1e9,
               tradeimpact.usd=-(log2lev(tradeloss) / (1 + log2lev(tradeloss))) * GDP.2015.est / 1e9,
               slrimpact.usd=-(log2lev(slrloss) / (1 + log2lev(slrloss))) * GDP.2015.est / 1e9,
               solow=product.chg - totimpact - -tradeloss - -slrloss,
               solow.usd=(log2lev(solow) / (1 + log2lev(solow))) * GDP.2015.est / 1e9,
               procapchg.usd=((log2lev(procap.chg) / (1 + log2lev(procap.chg))) * `Produced Capital Est` * 100 / 83.6),
               rencapchg.direct.usd=((log2lev(rencap.chg.ccpc) / (1 + log2lev(rencap.chg.ccpc))) * `Renewable Capital Est` * 100 / 83.6),
               rencapchg.feedback.usd=((log2lev(rencap.chg) / (1 + log2lev(rencap.chg))) * `Renewable Capital Est` * 100 / 83.6) - rencapchg.direct.usd,
               cumul.allcap.usd=pmax((allcap.true - allcap.nocc) / 1e9, -(`Produced Capital Est` + `Renewable Capital Est`)) * 100 / 83.6,
               cumul.allcap.usd.nn=ifelse(!is.na(cumul.allcap.usd), cumul.allcap.usd, 0)) %>%
        group_by(ISO, mc) %>%
        mutate(allcap.usd=cumul.allcap.usd - lag(cumul.allcap.usd))
        ##mutate(allcap.usd=cumul.allcap.usd.nn - lag(cumul.allcap.usd.nn))
}

get.allyr.ww <- function(persist, trade.method) {
    load(paste0("data/allyr-ww-", persist, "-", trade.method, ".RData"))
    allyr.ww[allyr.ww$ISO == 'SDN', which(is.na(allyr.ww[allyr.ww$ISO == 'ABW', ][1, ]))] <- NA # country change affects
    for2023 <- subset(allyr.ww, Year == 2022)
    stopifnot(all(for2023$ISO == allyr.ww$ISO[allyr.ww$Year == 2023]))
    for2023[, 1:8] <- subset(allyr.ww, Year == 2023)[, 1:8]
    allyr.ww <- rbind(subset(allyr.ww, Year <= 2022), for2023)
    allyr.ww <- allyr.ww %>% group_by(ISO, mc) %>% arrange(Year) %>%
        mutate(across(dimpact:weight.norm, ~ stats::filter(., rep(1 / 10, 10), sides=1)))

    allyr.ww
}
