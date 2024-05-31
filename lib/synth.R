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
                         solow = ifelse(all(is.na(product.chg)), NA, wtd.mean(product.chg - totimpact - tradeloss - slrloss, weights = iso.weight, normwt = T)),
                         total = ifelse(all(is.na(product.chg)), wtd.mean(totimpact - tradeloss - slrloss, weights = iso.weight, normwt = T), wtd.mean(product.chg, weights = iso.weight, normwt = T)),
                         weight2 = wtd.mean(weight.norm, weights = iso.weight))

    allyr3
}

get.weighted.ts <- function(allyr.ww, iso.weight, do.for.subset) {
    get.weighted.mcts(allyr.ww, iso.weight, do.for.subset) %>%
        group_by(Year) %>%
        dplyr::summarize(solow = ifelse(all(is.na(total)), NA, wtd.median(total - totimpact - tradeloss - slrloss, weights = weight2, normwt = T)),
                         prod25 = ifelse(all(is.na(total)), wtd.quantile(totimpact - tradeloss - slrloss, .25, weights = weight2, normwt = T), wtd.quantile(total, .25, weights = weight2, normwt = T)),
                         prod75 = ifelse(all(is.na(total)), wtd.quantile(totimpact - tradeloss - slrloss, .75, weights = weight2, normwt = T), wtd.quantile(total, .75, weights = weight2, normwt = T)),
                         total = ifelse(all(is.na(total)), wtd.median(totimpact - tradeloss - slrloss, weights = weight2, normwt = T), wtd.median(total, weights = weight2, normwt = T)),
                         totimpact = wtd.median(totimpact, weights = weight2, normwt = T),
                         slrloss = wtd.median(slrloss, weights = weight2, normwt = T),
                         tradeloss = wtd.median(tradeloss, weights = weight2, normwt = T))

    allyr3$totalloess <- tail(predict(loess(total ~ Year, allyr3, span = .25)), nrow(allyr3))
    allyr3$prod25loess <- tail(predict(loess(prod25 ~ Year, allyr3, span = .25)), nrow(allyr3))
    allyr3$prod75loess <- tail(predict(loess(prod75 ~ Year, allyr3, span = .25)), nrow(allyr3))

    allyr3
}
