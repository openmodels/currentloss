## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(dplyr)
library(reshape2)
library(countrycode)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mcstart <- 1
persist <- "0.08"

source("src/lib/utils2.R")

load.solowdata()

stan.model <- "
data {
  int<lower=0> T;
  vector[T] pop;

  real maxrencap0;
  real maxprocap0;
  real maxhumcap0;

  int<lower=0> N1;
  real gdp[N1];
  int<lower=0, upper=T> gdp_year[N1];

  int<lower=0> N2;
  real procap[N2];
  real rencap[N2];
  real humcap[N2];
  int<lower=0, upper=T> cap_year[N2];

  int<lower=0> N3;
  real natgdp[N3];
  int<lower=0, upper=T> natgdp_year[N3];

  int<lower=0> N4;
  real sav[N4];
  int<lower=0, upper=T> sav_year[N4];

  real<lower=0, upper=1> deprrate_prior;

  vector[T] gdpgrowshock_contemp; // instantaneous shock
  vector[T] gdpgrowshock_cumul; // cumulative shock
  vector[T] warming;
}
parameters {
  real<lower=0> tfp;
  real dtfpdt;

  // Renewable Capital
  real<lower=0, upper=1> rencap0part;
  real<lower=0, upper=0.1> renwarmeffect;
  real<lower=0, upper=1> rickerr;
  real<lower=1, upper=10> rickerb;
  real<lower=0> rencap_error;
  real<lower=0> rencapshare_error;

  // Produced Capital
  real<lower=0, upper=1> procap0part;
  real<lower=0, upper=1> saverate0;
  real<lower=-.1, upper=.1> dsaveratedt;
  real<lower=0, upper=1> deprrate;
  real<lower=0> procap_error;
  real<lower=0> sav_error;

  // Human Capital
  real<lower=0, upper=1> humcap0part;
  real<lower=-.1, upper=.1> dloghumcapdt;
  real<lower=0> humcap_error;

  // GDP production
  simplex[4] shares0; // ren, pro, hum, pop
  simplex[4] sharesT; // ren, pro, hum, pop
  real<lower=0> shares_error;
  vector<lower=0, upper=1>[T-1] cumulpart;

  real<lower=0> gdp_error;
}
transformed parameters {
  vector<lower=0>[T-1] product; // calculates year 1 product for year 2 capital
  vector<lower=0>[T] rencap_model;
  vector<lower=0>[T] procap_model;
  vector<lower=0>[T] humcap_univ;

  vector<lower=0>[T-1] product_nocc; // calculates year 1 product for year 2 capital
  vector<lower=0>[T] rencap_nocc;
  vector<lower=0>[T] procap_nocc;

  rencap_model[1] = rencap0part * maxrencap0;
  procap_model[1] = procap0part * maxprocap0;
  humcap_univ[1] = humcap0part * maxhumcap0;

  rencap_nocc[1] = rencap0part * maxrencap0;
  procap_nocc[1] = procap0part * maxprocap0;

  for (tt in 2:T) {
    // Y = TFP * GDPLoss * R^alpha * K^beta * H^gamma * L^(1 - alpha - beta - gamma)
    product[tt-1] = (tfp + dtfpdt * (tt-1)) * (1 - (gdpgrowshock_contemp[tt-1] + cumulpart[tt-1] * (gdpgrowshock_cumul[tt-1] - gdpgrowshock_contemp[tt-1]))) * pow(rencap_model[tt-1], (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2))) * pow(procap_model[tt-1], (shares0[2] + (tt-2) * (sharesT[2] - shares0[2]) / (T-2))) * pow(humcap_univ[tt-1], (shares0[3] + (tt-2) * (sharesT[3] - shares0[3]) / (T-2))) * pow(pop[tt-1], (shares0[4] + (tt-2) * (sharesT[4] - shares0[4]) / (T-2)));
    rencap_model[tt] = rencap_model[tt-1] * (1 + (1 - renwarmeffect * warming[tt-1]) * rickerr * exp(-rickerb * rencap_model[tt-1] / maxrencap0)) - (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2)) * product[tt-1] / (1 + (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2)) * product[tt-1] / rencap_model[tt-1]);
    procap_model[tt] = procap_model[tt-1] + (saverate0 + dsaveratedt * (tt-2)) * product[tt-1] - deprrate * procap_model[tt-1];
    humcap_univ[tt] = humcap_univ[tt-1] * (1 + dloghumcapdt);

    // Same calculations, but without climate change effect
    product_nocc[tt-1] = (tfp + dtfpdt * (tt-1)) * pow(rencap_nocc[tt-1], (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2))) * pow(procap_nocc[tt-1], (shares0[2] + (tt-2) * (sharesT[2] - shares0[2]) / (T-2))) * pow(humcap_univ[tt-1], (shares0[3] + (tt-2) * (sharesT[3] - shares0[3]) / (T-2))) * pow(pop[tt-1], (shares0[4] + (tt-2) * (sharesT[4] - shares0[4]) / (T-2)));
    rencap_nocc[tt] = rencap_nocc[tt-1] * (1 + rickerr * exp(-rickerb * rencap_nocc[tt-1] / maxrencap0)) - (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2)) * product_nocc[tt-1] / (1 + (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2)) * product_nocc[tt-1] / rencap_nocc[tt-1]);
    procap_nocc[tt] = procap_nocc[tt-1] + (saverate0 + dsaveratedt * (tt-2)) * product_nocc[tt-1] - deprrate * procap_nocc[tt-1];
  }
}
model {
  // Match observations
  for (ii in 1:N1) {
    if (gdp_year[ii] > 1)
      gdp[ii] ~ lognormal(log(product[gdp_year[ii]-1]), gdp_error);
  }
  for (ii in 1:N2) {
    rencap[ii] ~ lognormal(log(rencap_model[cap_year[ii]]), rencap_error);
    procap[ii] ~ lognormal(log(procap_model[cap_year[ii]]), procap_error);
    humcap[ii] ~ lognormal(log(humcap_univ[cap_year[ii]]), humcap_error);
  }
  for (ii in 1:N3) {
    natgdp[ii] ~ normal(shares0[1] + (natgdp_year[ii]-2) * (sharesT[1] - shares0[1]) / (T-2), rencapshare_error);
  }
  for (ii in 1:N4) {
    sav[ii] ~ normal(saverate0 + dsaveratedt * (sav_year[ii]-2), sav_error);
  }

  gdpgrowshock_cumul[2:T] ~ normal(-(log(product) - (log(product_nocc) - (1 - cumulpart) .* gdpgrowshock_cumul[2:T])), gdp_error); // gdpgrowshock is a log quantity, so gdp_error can apply to both

  // Model logic
  dsaveratedt ~ normal(0, sav_error);

  // Priors
  deprrate ~ normal(deprrate_prior, .1);
  sharesT[1] - shares0[1] ~ normal(0, shares_error);
  sharesT[2] - shares0[2] ~ normal(0, shares_error);
  sharesT[3] - shares0[3] ~ normal(0, shares_error);
  sharesT[4] - shares0[4] ~ normal(0, shares_error);
  shares_error ~ normal(0, 0.1);
}"

if (mcstart == 'x')
    pastsolow <- rbind(read.csv(paste0("solow-v4-", persist, ".csv"))) #, read.csv("solow-v4-11.csv"), read.csv("solow-v4-21.csv"), read.csv("solow-v4-26.csv"))
if (mcstart == 1 && file.exists(paste0("solow-v4-", persist, ".csv"))) {
    sumbymc <- read.csv(paste0("solow-v4-", persist, ".csv"))
} else if (mcstart != 1 && file.exists(paste0("solow-v4-", persist, "-", mcstart, ".csv"))) {
    sumbymc <- read.csv(paste0("solow-v4-", persist, "-", mcstart, ".csv"))
} else
    sumbymc <- data.frame()

fit <- NA
if (mcstart == 'x') {
    allmc <- 30:1
} else
    allmc <- mcstart:30

for (mcii in allmc) {
    load.solowdata.mc(mcii)

    for (iso in levels(df.pro$ISO)) {
        if (mcstart == 'x') {
            if (paste(iso, mcii) %in% paste(pastsolow$ISO, pastsolow$mc))
                next
        } else if (paste(iso, mcii) %in% paste(sumbymc$ISO, sumbymc$mc))
            next
        print(c(iso, mcii))
        stan.data <- make.stan.data(iso)

        fit <- tryCatch({
            stan(model_code=stan.model, data=stan.data, open_progress=F, fit=fit, chains=1)
        }, error=function(ee) {
            NULL
        })
        if (is.null(fit))
            next
        la <- extract(fit, permute=T)
        if (is.null(la))
            next

        ## Simulate without climate change
        solowout <- model.solow(la, stan.data, F, rencaptrue=la$rencap_model)

        ess <- mean(stan_ess(fit)$data$stat)
        lp <- mean(la$lp__)

        row <- data.frame(ISO=iso, mc=mcii, totimpact.end=df$totimpact[df$ISO == iso & df$Year == max(df$Year)],
                          itlimpact.end=-df$fracloss[df$ISO == iso & df$Year == max(df$Year)],
                          product.end.true=mean(la$product[, 62]), product.end.nocc=mean(solowout$product[, 62]),
                          rencap.end.true=mean(la$rencap_model[, 63]), rencap.end.nocc=mean(solowout$rencap_model[, 63]),
                          procap.end.true=mean(la$procap_model[, 63]), procap.end.nocc=mean(solowout$procap_model[, 63]),
                          humcap.end.true=mean(la$humcap_univ[, 63]), humcap.end.nocc=mean(solowout$humcap_univ[, 63]),
                          renwarmeffect=mean(la$renwarmeffect), ess, lp)
        row$product.chg <- 1 - row$product.end.nocc / row$product.end.true
        row$rencap.chg <- 1 - row$rencap.end.nocc / row$rencap.end.true
        row$procap.chg <- 1 - row$procap.end.nocc / row$procap.end.true
        row$humcap.chg <- 1 - row$humcap.end.nocc / row$humcap.end.true

        save(la, file=paste0("data/solow-", persist, "/v4-", iso, "-", mcii, ".RData"))

        sumbymc <- rbind(sumbymc, row)
        if (mcstart == 1)
            write.csv(sumbymc, paste0("solow-v4-", persist, ".csv"), row.names=F)
        else
            write.csv(sumbymc, paste0("solow-v4-", persist, "-", mcstart, ".csv"), row.names=F)
    }
}
