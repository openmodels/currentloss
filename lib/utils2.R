library(dplyr)
library(countrycode)
source("src/lib/loadutils.R")

## Grab pre-Solow results for countries without capital info
load(paste0("data/mcrfres-", persist, ".RData"))
results2 <- results %>% group_by(ISO, mc) %>%
    mutate(totimpact=stats::filter(c(rep(0, 30), dimpact), (1 - as.numeric(persist))^(0:30), sides=1)[-1:-30])

df.gdp3 <- load.gdp3()
slr2 <- load.slr2(df.gdp3)
tradeloss <- load.tradeloss(persist)

tradeloss.global <- tradeloss %>% group_by(year) %>% dplyr::summarize(tradeloss=mean(tradeloss, na.rm=T))

read.iw <- function(filepath, value.name) {
    df.pro <- read.csv(filepath)
    df.pro$ISO <- countryname(df.pro$Country, 'iso3c')
    df.pro2 <- melt(df.pro[, -1:-2], 'ISO', variable.name='XYear', value.name=value.name)
    df.pro2$Year <- sapply(df.pro2$XYear, function(ss) as.numeric(substring(ss, 2, 5)))
    df.pro2
}

load.solowdata <- function() {
    df.gdp2 <- read.wb("data/capital/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_5871893.xls", 'GDP.2015')
    df.gdp2$GDP.2005 <- df.gdp2$GDP.2015 * 83.6 / 100
    df.lab2 <- read.wb("data/capital/API_SL.TLF.TOTL.IN_DS2_en_excel_v2_5871833.xls", 'Labor')
    df.pop2 <- read.wb("data/capital/API_SP.POP.TOTL_DS2_en_excel_v2_5871620.xls", 'Population')
    df.sav2 <- read.wb("data/capital/API_NY.GNS.ICTR.ZS_DS2_en_excel_v2_5871648.xls", 'SavingRate')
    df.nat2 <- read.wb("data/capital/API_NV.AGR.TOTL.ZS_DS2_en_excel_v2_5871737.xls", 'NaturalGDP')

    df.pro <- read.csv("data/capital/tabula-C-produced.csv")
    df.pro$ISO <- factor(countryname(df.pro$Country, 'iso3c'))
    df.pro2 <- read.iw("data/capital/tabula-C-produced.csv", 'Produced Capital')
    df.hum2 <- read.iw("data/capital/tabula-B-human.csv", 'Human Capital')
    df.non2 <- read.iw("data/capital/tabula-A1-nonrenewable.csv", 'Nonrenewable Capital')
    df.ren2 <- read.iw("data/capital/tabula-A2-renewable.csv", 'Renewable Capital')

    era5 <- read.csv("data/era5-t2m-combo-adm0.csv")
    era5b <- era5 %>% left_join(subset(era5, Year < 1960) %>% group_by(ISO) %>% summarize(t2m=mean(t2m)), by='ISO', suffix=c('', '.hist'))
    era5b$warming <- era5b$t2m - era5b$t2m.hist

    assign("df.gdp2", df.gdp2, envir = .GlobalEnv)
    assign("df.lab2", df.lab2, envir = .GlobalEnv)
    assign("df.pop2", df.pop2, envir = .GlobalEnv)
    assign("df.sav2", df.sav2, envir = .GlobalEnv)
    assign("df.nat2", df.nat2, envir = .GlobalEnv)

    assign("df.pro", df.pro, envir = .GlobalEnv)
    assign("df.pro2", df.pro2, envir = .GlobalEnv)
    assign("df.hum2", df.hum2, envir = .GlobalEnv)
    assign("df.non2", df.non2, envir = .GlobalEnv)
    assign("df.ren2", df.ren2, envir = .GlobalEnv)

    assign("era5b", era5b, envir = .GlobalEnv)
}

load.solowdata.mc <- function(mcii) {
    df <- data.frame(ISO=levels(df.pro$ISO)) %>%
        left_join(subset(results2, mc == mcii & Year >= 1960), by='ISO') %>%
        left_join(df.gdp2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(df.pro2[, c('ISO', 'Year', 'Produced Capital')], by=c('Year', 'ISO')) %>%
        left_join(df.hum2[, c('ISO', 'Year', 'Human Capital')], by=c('Year', 'ISO')) %>%
        left_join(df.non2[, c('ISO', 'Year', 'Nonrenewable Capital')], by=c('Year', 'ISO')) %>%
        left_join(df.ren2[, c('ISO', 'Year', 'Renewable Capital')], by=c('Year', 'ISO')) %>%
        left_join(df.lab2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(df.pop2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(df.sav2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(df.nat2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(subset(slr2, mc == mcii), by=c('Year'='year', 'ISO')) %>%
        left_join(subset(tradeloss, mc == mcii), by=c('Year'='year', 'ISO')) %>%
        left_join(era5b[, c('Year', 'ISO', 'warming')], by=c('Year', 'ISO'))
    df$ISO <- factor(df$ISO, levels=levels(df.pro$ISO))
    df$slrloss[is.na(df$slrloss)] <- 0

    df2 <- subset(df, !is.na(ISO)) %>% group_by(ISO) %>%
        reframe(Year=Year, denom=min(Population, na.rm=T), GDP.2005=GDP.2005 / denom,
                `Produced Capital`=1e9 * `Produced Capital` / denom,
                `Per Person Human Capital`=1e9 * `Human Capital` / Population, # Note: Per person
                `Nonrenewable Capital`=1e9 * `Nonrenewable Capital` / denom,
                `Renewable Capital`=1e9 * `Renewable Capital` / denom,
                Labor=Labor / denom, Population=Population / denom,
                SavingRate=SavingRate / 100, NaturalGDP=NaturalGDP / 100,
                gdpgrowshock_contemp=-dimpact, gdpgrowshock_cumul=-(totimpact - tradeloss - slrloss), warming=warming)

    assign("df", df, envir = .GlobalEnv)
    assign("df2", df2, envir = .GlobalEnv)
}

make.stan.data <- function(iso) {
    status.1990 <- df2[df2$Year == 1990 & df2$ISO == iso,]

    stan.data <- list(T=diff(range(df2$Year)) + 1,
                      pop=df2$Population[df2$ISO == iso],

                      maxrencap0=2 * status.1990$`Renewable Capital` + 1,
                      maxprocap0=status.1990$`Produced Capital` + 1,
                      maxhumcap0=status.1990$`Per Person Human Capital` + 1,

                      N1=sum(!is.na(df2$GDP.2005) & df2$ISO == iso),
                      gdp=df2$GDP.2005[!is.na(df2$GDP.2005) & df2$ISO == iso],
                      gdp_year=df2$Year[!is.na(df2$GDP.2005) & df2$ISO == iso] - 1959,

                      N2=sum(!is.na(df2$`Produced Capital`) & df2$ISO == iso),
                      rencap=df2$`Renewable Capital`[!is.na(df2$`Produced Capital`) & df2$ISO == iso],
                      procap=df2$`Produced Capital`[!is.na(df2$`Produced Capital`) & df2$ISO == iso],
                      humcap=df2$`Per Person Human Capital`[!is.na(df2$`Produced Capital`) & df2$ISO == iso],
                      cap_year=df2$Year[!is.na(df2$`Produced Capital`) & df2$ISO == iso] - 1959,

                      N3=sum(!is.na(df2$NaturalGDP) & df2$ISO == iso),
                      natgdp=df2$NaturalGDP[!is.na(df2$NaturalGDP) & df2$ISO == iso],
                      natgdp_year=df2$Year[!is.na(df2$NaturalGDP) & df2$ISO == iso] - 1959,

                      deprrate_prior=0.05, gdpgrowshock_contemp=df2$gdpgrowshock_contemp[df2$ISO == iso],
                      gdpgrowshock_cumul=df2$gdpgrowshock_cumul[df2$ISO == iso],
                      warming=df2$warming[df2$ISO == iso])

    stan.data$rencap[is.na(stan.data$rencap) | stan.data$rencap == 0] <- 0.1
    if (any(is.na(stan.data$gdpgrowshock_contemp)))
        stan.data$gdpgrowshock_contemp[is.na(stan.data$gdpgrowshock_contemp)] <- -(df$totimpact[df$ISO == iso] - df$slrloss[df$ISO == iso] - tradeloss.global$tradeloss[tradeloss.global$year >= 1960])[is.na(stan.data$gdpgrowshock_contemp)]
    if (any(is.na(stan.data$gdpgrowshock_cumul)))
        stan.data$gdpgrowshock_cumul[is.na(stan.data$gdpgrowshock_cumul)] <- -(df$totimpact[df$ISO == iso] - df$slrloss[df$ISO == iso] - tradeloss.global$tradeloss[tradeloss.global$year >= 1960])[is.na(stan.data$gdpgrowshock_cumul)]

    if (sum(!is.na(df2$SavingRate) & df2$ISO == iso) == 0) {
        rows <- df2[sample(which(!is.na(df2$SavingRate)), 10),]
        stan.data$N4 <- 10
        stan.data$sav <- rows$SavingRate
        stan.data$sav_year <- rows$Year - 1959
    } else {
        stan.data$N4 <- sum(!is.na(df2$SavingRate) & df2$ISO == iso)
        stan.data$sav <- df2$SavingRate[!is.na(df2$SavingRate) & df2$ISO == iso]
        stan.data$sav_year <- df2$Year[!is.na(df2$SavingRate) & df2$ISO == iso] - 1959
    }

    stan.data
}

model.solow <- function(la, stan.data, withcc, rencaptrue=NULL) {
    product <- matrix(0, 1000, stan.data$T-1)
    rencap_model <- matrix(NA, 1000, stan.data$T)
    procap_model <- matrix(NA, 1000, stan.data$T)
    humcap_model <- matrix(NA, 1000, stan.data$T)

    rencap_model[, 1] = la$rencap0part * stan.data$maxrencap0
    procap_model[, 1] = la$procap0part * stan.data$maxprocap0
    humcap_model[, 1] = la$humcap0part * stan.data$maxhumcap0

    for (tt in 2:stan.data$T) {
        product[, tt-1] = (la$tfp + la$dtfpdt * (tt-1)) * (rencap_model[, tt-1]^(la$shares0[, 1] + (tt-2) * (la$sharesT[, 1] - la$shares0[, 1]) / (stan.data$T-2))) * (procap_model[, tt-1]^(la$shares0[, 2] + (tt-2) * (la$sharesT[, 2] - la$shares0[, 2]) / (stan.data$T-2))) * (humcap_model[, tt-1]^(la$shares0[, 3] + (tt-2) * (la$sharesT[, 3] - la$shares0[, 3]) / (stan.data$T-2))) * (stan.data$pop[tt-1]^(la$shares0[, 4] + (tt-2) * (la$sharesT[, 4] - la$shares0[, 4]) / (stan.data$T-2)))
        if (withcc == T || withcc == "prodonly")
            product[, tt-1] <- product[, tt-1] * (1 - (stan.data$gdpgrowshock_contemp[tt-1] + la$cumulpart[tt-1] * (stan.data$gdpgrowshock_cumul[tt-1] - stan.data$gdpgrowshock_contemp[tt-1])))
        if (withcc == T)
            rickerr2 <- (1 - la$renwarmeffect * stan.data$warming[tt-1]) * la$rickerr
        else
            rickerr2 <- la$rickerr
        rencap_model[, tt] = rencap_model[, tt-1] * (1 + rickerr2 * exp(-la$rickerb * rencap_model[, tt-1] / stan.data$maxrencap0)) - (la$shares0[, 1] + (tt-2) * (la$sharesT[, 1] - la$shares0[, 1]) / (stan.data$T-2)) * product[, tt-1] / (1 + (la$shares0[, 1] + (tt-2) * (la$sharesT[, 1] - la$shares0[, 1]) / (stan.data$T-2)) * product[, tt-1] / rencap_model[, tt-1])
        if (!is.null(rencaptrue) & withcc == F) {
            ## shift this toward true with geometric mean; otherwise can shift unrealistically
            rencap_model[, tt] <- (rencap_model[, tt]^9 * rencaptrue[, tt])^.1
        }
        procap_model[, tt] = procap_model[, tt-1] + (la$saverate0 + la$dsaveratedt * (tt-2)) * product[, tt-1] - la$deprrate * procap_model[, tt-1]
        humcap_model[, tt] = humcap_model[, tt-1] * (1 + la$dloghumcapdt)
        if (any(is.na(product[, tt-1])))
            print(c("Product", tt))
        if (any(is.na(rencap_model[, tt])))
            print(c("RenCap", tt))
        if (any(is.na(procap_model[, tt])))
            print(c("ProCap", tt))
        if (any(is.na(humcap_model[, tt])))
            print(c("HumCap", tt))
    }

    list("product"=product, "rencap_model"=rencap_model, "procap_model"=procap_model, "humcap_model"=humcap_model)
}

aosis <- c('Antigua and Barbuda', 'Bahamas', 'Barbados', 'Belize', 'Cuba', 'Dominica', 'Dominican Republic', 'Grenada', 'Guyana', 'Haiti', 'Jamaica', 'Saint Kitts and Nevis', 'Saint Lucia', 'Saint Vincent and the Grenadines', 'Suriname', 'Trinidad and Tobago', 'Cook Islands', 'Federated States of Micronesia', 'Fiji', 'Kiribati', 'Nauru', 'Niue', 'Palau', 'Papua New Guinea', 'Republic of the Marshall Islands', 'Samoa', 'Solomon Islands', 'Tonga', 'Tuvalu', 'Vanuatu', 'Cabo Verde', 'Comoros', 'Guinea Bissau', 'Maldives', 'Mauritius', 'Sao Tome and Principe', 'Seychelles', 'Singapore', 'Timor Leste')
g77 <- c("Afghanistan", "Algeria", "Angola", "Antigua and Barbuda", "Argentina", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belize", "Benin", "Bhutan", "Bolivia (Plurinational State of)", "Botswana", "Brazil", "Brunei Darussalam", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Costa Rica", "Côte d'Ivoire", "Cuba", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Fiji", "Gabon", "Gambia", "Ghana", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Jamaica", "Jordan", "Kenya", "Kiribati", "Kuwait", "Lao People's Democratic Republic", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Marshall Islands", "Mauritania", "Mauritius", "Micronesia (Federated States of)", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Nicaragua", "Niger", "Nigeria", "Oman", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Qatar", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Seychelles", "Sierra Leone", "Singapore", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Sri Lanka", "State of Palestine", "Sudan", "Suriname", "Syrian Arab Republic", "Tajikistan", "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "United Arab Emirates", "United Republic of Tanzania", "Uruguay", "Vanuatu", "Venezuela (Bolivarian Republic of)", "Viet Nam", "Yemen", "Zambia", "Zimbabwe")
ailac <- c("Chile", "Colombia", "Costa Rica", "Guatemala", "Honduras", "Panama", "Paraguay", "Peru")
grulac <- c("Argentina", "Bahamas", "Bolivia (Plurinational State Of)", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Guyana", "Haiti", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad And Tobago", "Uruguay", "Venezuela (Bolivarian Republic Of)")
cvf <- c("Afghanistan", "Bangladesh", "Barbados", "Bhutan", "Costa Rica", "Ethiopia", "Ghana", "Kenya", "Kiribati", "Madagascar", "Maldives", "Nepal", "Philippines", "Rwanda", "Saint Lucia", "Tanzania", "Timor-Leste", "Tuvalu", "Vanuatu", "Vietnam")
ldc <- c("Afghanistan", "Angola", "Bangladesh", "Benin", " Bhutan", "Burkina Faso", "Burundi", "Cambodia", "Central African Republic", "Chad", "Comoros", "Democratic Republic of the Congo", "Djibouti", "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Haiti", "Kiribati", "Lao People’s Dem. Republic", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", "Myanmar", "Nepal", "Niger", "Rwanda", "Sao Tome and Principe", "Senegal", "Sierra Leone", "Solomon Islands", "Somalia", "South Sudan", "Sudan", "Timor-Leste", "Togo", "Tuvalu", "Uganda", "United Republic of Tanzania", "Yemen", "Zambia")
africa <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cape Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Côte D'Ivoire", "DR Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Eswatini", "Gabon", "Gambia (Republic of The)", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Togo", "Tunisia", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe")
eu <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
umbrella <- c("Australia", "Canada", "Japan", "New Zealand", "Kazakhstan", "Norway", "the Russian Federation", "Ukraine", "United States")
lmdc <- c("Algeria", "Bangladesh", "Bolivia", "China", "Cuba", "Ecuador", "Egypt", "El Salvador", "India", "Indonesia", "Iran", "Iraq", "Jordan", "Kuwait", "Malaysia", "Mali", "Nicaragua", "Pakistan", "Saudi Arabia", "Sri Lanka", "Sudan", "Syria", "Venezuela", "Vietnam")
alba <- c("Antigua and Barbuda", "Bolivia", "Cuba", "Dominica", "Grenada", "Nicaragua", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Venezuela")
eig <- c("Switzerland", "Korea", "Mexico", "Liechtenstein", "Monaco", "Georgia")
arab <- c("Algeria", "Bahrain", "Comoros", "Djibouti", "Egypt", "Iraq", "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", "Mauritania", "Oman", "Palestine", "Qatar", "Saudi Arabia", "Somalia", "Sudan", "Syria", "Tunisia", "United Arab Emirates", "Yemen")
groupings <- list("AOSIS"=aosis, "G77"=g77, "AILAC"=ailac, "GRULAC"=grulac, "CVF"=cvf, "LDCs"=ldc, "Africa"=africa, "EU"=eu, "Umbrella"=umbrella, "LMDC"=lmdc, "ALBA"=alba, "EIG"=eig, "Arab"=arab)
