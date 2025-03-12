## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
setwd("~/research/currentloss")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

args = commandArgs(trailingOnly=TRUE)

library(readxl)
library(dplyr)
library(reshape2)
library(countrycode)
library(rstan)
library(parallel)

do.parallel <- T

if (length(args) == 1) {
  do.mcs <- as.numeric(args)
} else {
  do.mcs <- 1:30
}
persist <- "0.36"
trade.method <- 'dd-mcr2all'

source("src/lib/utils2.R")

load.solowdata()

stan.model <- "
data {
  int<lower=0> T;
  vector[T] pop;

  real maxprocap0;

  int<lower=0> N1;
  real gdp[N1];
  int<lower=0, upper=T> gdp_year[N1];

  int<lower=0> N2;
  real procap[N2];
  int<lower=0, upper=T> cap_year[N2];

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

  // Produced Capital
  real<lower=0, upper=1> procap0part;
  real<lower=0, upper=1> saverate0;
  real<lower=-.1, upper=.1> dsaveratedt;
  real<lower=0, upper=1> deprrate;
  real<lower=0> procap_error;
  real<lower=0> sav_error;

  // GDP production
  simplex[2] shares0; // pro, pop
  simplex[2] sharesT; // pro, pop
  real<lower=0> shares_error;
  vector<lower=0, upper=1>[T-1] cumulpart;

  real<lower=0> gdp_error;
}
transformed parameters {
  vector<lower=0>[T-1] product; // calculates year 1 product for year 2 capital
  vector<lower=0>[T] procap_model;

  vector<lower=0>[T-1] product_nocc; // calculates year 1 product for year 2 capital
  vector<lower=0>[T] procap_nocc;

  procap_model[1] = procap0part * maxprocap0;
  procap_nocc[1] = procap0part * maxprocap0;

  for (tt in 2:T) {
    // Y = TFP * GDPLoss * R^alpha * K^beta * H^gamma * L^(1 - alpha - beta - gamma)
    product[tt-1] = (tfp + dtfpdt * (tt-1)) * (1 - (gdpgrowshock_contemp[tt-1] + cumulpart[tt-1] * (gdpgrowshock_cumul[tt-1] - gdpgrowshock_contemp[tt-1]))) *
       pow(procap_model[tt-1], (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2))) * pow(pop[tt-1], (shares0[2] + (tt-2) * (sharesT[2] - shares0[2]) / (T-2)));
    procap_model[tt] = procap_model[tt-1] + (saverate0 + dsaveratedt * (tt-2)) * product[tt-1] - deprrate * procap_model[tt-1];

    // Same calculations, but without climate change effect
    product_nocc[tt-1] = (tfp + dtfpdt * (tt-1)) * pow(procap_nocc[tt-1], (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2))) * 
      pow(pop[tt-1], (shares0[2] + (tt-2) * (sharesT[2] - shares0[2]) / (T-2)));
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
    procap[ii] ~ lognormal(log(procap_model[cap_year[ii]]), procap_error);
  }
  for (ii in 1:N4) {
    sav[ii] ~ normal(saverate0 + dsaveratedt * (sav_year[ii]-2), sav_error);
  }

  // gdpgrowshock is a log quantity, so gdp_error can apply to both
  gdpgrowshock_cumul[2:T] ~ normal(-(log(product) - (log(product_nocc) - (1 - cumulpart) .* gdpgrowshock_contemp[2:T])), gdp_error);
  // Last term says that as cumulpart -> 0, target loss can be an extra contemp effect, because cumul is already fully reflected

  // Model logic
  dsaveratedt ~ normal(0, sav_error);

  // Priors
  deprrate ~ normal(deprrate_prior, .1);
  sharesT[1] - shares0[1] ~ normal(0, shares_error);
  sharesT[2] - shares0[2] ~ normal(0, shares_error);
  shares_error ~ normal(0, 0.1);
}"

mod <- stan_model(model_code=stan.model)

dir.create(paste0("data/solow-", persist, "-", trade.method, "-prodonly"))

for (mcii in do.mcs) {
    if (file.exists(paste0("data/solow-", persist, "-", trade.method, "-prodonly/solow-v4-", persist, "-", mcii, ".csv")))
        next
    print(mcii)
    load.solowdata.mc(mcii)

    if (do.parallel) {
        cl <- makeCluster(detectCores())
    	clusterEvalQ(cl, {
            library(rstan)
        })

	clusterExport(cl, c("df", "df2", "mod", "mcii", "make.stan.data", "model.solow.prodonly", "persist", "tradeloss.global", "trade.method"))
	mylapply <- function(xx, func) {
	  parLapply(cl, xx, func)
	}
    } else {
        mylapply <- lapply
    }
    
    allrows <- mylapply(levels(df.pro$ISO), function(iso) {
        print(c(iso, mcii))
        stan.data <- make.stan.data(iso)

        fit <- tryCatch({
            sampling(mod, data=stan.data, open_progress=F, chains=1, cores=1)
        }, error=function(ee) {
            NULL
        })
        if (is.null(fit))
	    return(data.frame())
        la <- extract(fit, permute=T)
        if (is.null(la))
	    return(data.frame())

        ## Simulate without climate change
        solowout <- model.solow.prodonly(la, stan.data, F)

        ess <- mean(stan_ess(fit)$data$stat)
        lp <- mean(la$lp__)

        row <- data.frame(ISO=iso, mc=mcii, totimpact.end=df$totimpact[df$ISO == iso & df$Year == max(df$Year)],
                          slrimpact.end=-df$slrloss[df$ISO == iso & df$Year == max(df$Year)],
                          itlimpact.end=-df$tradeloss[df$ISO == iso & df$Year == max(df$Year)],
                          product.end.true=mean(la$product[, 62]), product.end.nocc=mean(solowout$product[, 62]),
                          procap.end.true=mean(la$procap_model[, 63]), procap.end.nocc=mean(solowout$procap_model[, 63]),
                          ess, lp)
        row$product.chg <- 1 - row$product.end.nocc / row$product.end.true
        row$procap.chg <- 1 - row$procap.end.nocc / row$procap.end.true

        save(la, file=paste0("data/solow-", persist, "-", trade.method, "-prodonly/v4-", iso, "-", mcii, ".RData"))

        row
    })

    if (do.parallel)
        stopCluster(cl)

    sumbymc <- data.frame()
    for (ii in 1:length(allrows))
        sumbymc <- rbind(sumbymc, allrows[[ii]])

    write.csv(sumbymc, paste0("data/solow-", persist, "-", trade.method, "-prodonly/solow-v4-", persist, "-", mcii, ".csv"), row.names=F)
}
