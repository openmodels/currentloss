persist.decay <- function(x, persist, lag=30L) {
  stats::filter(c(rep(0, lag), x), (1 - persist)^(0:lag), sides=1)[-seq_len(lag)]
}

remove.lags <- function(dimpact, lags, persist) { # lags is K-1 in my notation
    if (lags == 0)
        return(dimpact)
    result <- dimpact[1]
    withpersist <- dimpact[1]
    for (tt in 1:lags) {
        result[tt+1] <- dimpact[tt+1] - (1 - persist) * withpersist[tt]
        withpersist[tt+1] <- dimpact[tt+1]
    }
    for (tt in (lags + 1):(length(dimpact)-1)) {
        nn <- tt - lags
        result[tt+1] <- dimpact[tt+1] + sum((1 - persist)^(lags + 1 + (1:nn)) * dimpact[nn - (1:nn) + 1]) - (1 - persist) * withpersist[tt]
        withpersist[tt+1] <- result[tt+1] + (1 - persist) * withpersist[tt]
    }

    return(result)
}

persist.waidelich <- function(x, drop.lags, persist) {
    x + c(rep(0, drop.lags), cumsum(x) * (1 - persist))[1:length(x)]
}

persist.general <- function(x, persist, drop.lags, do.waidelich) {
    if (do.waidelich) {
        persist.waidelich(x, drop.lags, persist)
    } else {
        persist.decay(remove.lags(x, drop.lags, persist), persist)
    }
}


