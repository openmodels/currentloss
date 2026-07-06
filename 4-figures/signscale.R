## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

source("src/lib/myPBSmapping.R")
library(dplyr)
library(Hmisc)
library(ggplot2)
source("src/lib/loadutils.R")

loadmaps.distance <- 1200
source("src/lib/loadmaps.R")

persist <- 0.6

allres <- load.allres(persist)

allres2 <- allres %>% filter(Year >= 2014) %>% group_by(name, paper, ISO) %>%
    dplyr::summarize(dimpact=mean(dimpact, na.rm=T))
rm('allres')

## Portion agree, across models
allres3 <- allres2 %>% group_by(ISO) %>%
    dplyr::summarize(fracagree=pmax(.5, mean(sign(dimpact) == median(sign(dimpact), na.rm=T), na.rm=T)))

shp2 <- shp %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(allres3, by=c('ADM0_A3'='ISO'))
centroids2 <- centroids %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(allres3, by=c('ADM0_A3'='ISO'))

format.percent <- function(xx) {
    ifelse(is.na(xx), NA, paste0(round(xx * 100), "%"))
}

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=fracagree, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=subset(centroids2, show), aes(label=format.percent(fracagree)), size=3, label.padding=unit(0.1, "lines")) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_gradient("Portion of models that agree with the median impact sign:", low = "white", high = scales::muted("blue"), labels=scales::percent, guide=guide_colorbar(barwidth=10, barheight=1), limits=c(.5, 1)) +
    theme_bw() + theme(legend.position="bottom", legend.key.width=unit(10,"in"),
                       legend.key.height=unit(1,"in"))
ggsave(paste0("figures/signscale-model-", persist, ".pdf"), width=10, height=5.5)

## Portion agree, across MCs

source("src/lib/synth.R")

trade.method <- 'dd-mcr2all'
allyr.ww <- get.allyr.ww(persist, trade.method)

wtd.median <- function(xx, weights=NULL, normwt=F) {
    if (all(is.na(xx)))
        return(NA)
    wtd.quantile(xx, 0.5, weights=weights, normwt=normwt)
}

sumbymc2 <- allyr.ww %>% group_by(ISO, mc) %>%
    dplyr::summarize(across(dimpact:weight.norm, ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
    group_by(ISO, mc) %>%
    mutate(slrimpact=-slrloss, tradeimpact=-tradeloss,
           rencap.chg=1 - rencap.nocc / rencap.true,
           allcap.chg=1 - allcap.nocc / allcap.true, procap.chg=allcap.chg - rencap.chg,
           weight=weight.norm,
           total=ifelse(all(is.na(product.chg)), wtd.median(totimpact + tradeimpact + slrimpact, weights=weight, normwt=T), wtd.median(product.chg, weights=weight, normwt=T)))

sumbymc3 <- sumbymc2 %>% group_by(ISO) %>%
    dplyr::summarize(fracagree=pmax(.5, mean(sign(total) == wtd.median(sign(total), weight, normwt=T), na.rm=T)))

shp2 <- shp %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(sumbymc3, by=c('ADM0_A3'='ISO'))
centroids2 <- centroids %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(sumbymc3, by=c('ADM0_A3'='ISO'))

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=fracagree, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=subset(centroids2, show), aes(label=format.percent(fracagree)), size=3, label.padding=unit(0.1, "lines")) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_gradient("Portion of Monte Carlo that agree with the median impact sign:", low = "white", high = scales::muted("blue"), labels=scales::percent, guide=guide_colorbar(barwidth=10, barheight=1)) +
    theme_bw() + theme(legend.position="bottom", legend.key.width=unit(10,"in"),
                       legend.key.height=unit(1,"in"))
ggsave(paste0("figures/signscale-mc-", persist, "-", trade.method, ".pdf"), width=10, height=5.5)

## Portion agree, across modeling assumptions

rm(allyr.ww)
df.gdp3 <- load.gdp3()

allassumps <- data.frame()
for (trade.method.suffix in c('', '-mcr2all', '-mcpaperall')) {
    for (persist in c(0, 0.36, 0.6, 0.78, 1)) {
        if (persist == 1) {
            if (trade.method.suffix == '') {
                results2 <- read.metaanal("mcrfres-0.6")
            } else if (trade.method.suffix == '-mcr2all') {
                results2 <- read.metaanal('mcr2res-0.6-Total R2')
            } else if (trade.method.suffix == '-mcpaperall') {
                results2 <- read.metaanal('mcpaperres-0.6-all')
            }

            sumbymc3 <- results2 %>% group_by(ISO) %>%
                dplyr::summarize(total=mean(dimpact, na.rm=T))

            allassumps <- rbind(allassumps, cbind(trade.method.suffix=trade.method.suffix,
                                                  persist=persist, trade.method='none',
                                                  solow.conf='none', slrconf='none',
                                                  sumbymc3))
            next
        }
        for (trade.method in c('dd', 'fd', 'li')) {
            for (solow.conf in c('', '-prodonly', '-additive')) {
                if (!file.exists(paste0("data/allyr-ww-", persist, "-", trade.method, trade.method.suffix, solow.conf, ".RData")))
                    next

                allyr.ww <- get.allyr.ww(persist, paste0(trade.method, trade.method.suffix, solow.conf))

                for (slrconf in c('none', 'market', 'optadapt', 'noadapt')) {
                    if (slrconf == 'none') {
                        slr <- expand.grid(ISO=unique(allyr.ww$ISO), year=unique(allyr.ww$Year)) %>%
                            mutate(mu=0, q17=0, q83=0)
                    } else if (slrconf == 'market') {
                        slr <- read.csv("data/slrbyadm0-final.csv")
                    } else if (slrconf == 'optadapt') {
                        slr <- read.csv("data/slrbyadm0-final-optimalfixed.csv")
                    } else if (slrconf == 'noadapt') {
                        slr <- read.csv("data/slrbyadm0-final-noAdaptation.csv")
                    }
                    slr2 <- slr %>% left_join(df.gdp3, by=c('ISO'='Country Code', 'year'='Year')) %>%
                        group_by(ISO, year) %>% reframe(mc=1:30, slrloss=rnorm(30, mu / GDP.2019.est, ((q83 - q17) / diff(qnorm(c(.17, .83)))) / GDP.2019.est))

                    allyr.ww2 <- allyr.ww %>% select(!slrloss) %>%
                        left_join(slr2, by=c('Year'='year', 'ISO', 'mc'))

                    sumbymc2 <- allyr.ww2 %>% group_by(ISO, mc) %>%
                        dplyr::summarize(across(dimpact:slrloss, ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
                        group_by(ISO, mc) %>%
                        mutate(slrimpact=-slrloss, tradeimpact=-tradeloss,
                               weight=weight.norm,
                               total=ifelse(all(is.na(product.chg)), totimpact + tradeimpact + slrimpact, product.chg))

                    sumbymc3 <- sumbymc2 %>% group_by(ISO) %>%
                        dplyr::summarize(total=wtd.mean(total, weight, na.rm=T))

                    allassumps <- rbind(allassumps, cbind(trade.method.suffix=trade.method.suffix,
                                                          persist=persist, trade.method=trade.method,
                                                          solow.conf=solow.conf, slrconf=slrconf,
                                                          sumbymc3))

                    if (trade.method == 'dd') {
                        ## Also consider no trade
                        sumbymc2 <- allyr.ww2 %>% group_by(ISO, mc) %>%
                            dplyr::summarize(across(dimpact:slrloss, ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
                            group_by(ISO, mc) %>%
                            mutate(slrimpact=-slrloss, tradeimpact=0,
                                   weight=weight.norm,
                                   total=ifelse(all(is.na(product.chg)), totimpact + tradeimpact + slrimpact, product.chg))

                        sumbymc3 <- sumbymc2 %>% group_by(ISO) %>%
                            dplyr::summarize(total=wtd.mean(total, weight, na.rm=T))

                        allassumps <- rbind(allassumps, cbind(trade.method.suffix=trade.method.suffix,
                                                              persist=persist, trade.method='none',
                                                              solow.conf=solow.conf, slrconf=slrconf,
                                                              sumbymc3))
                    }
                    if (solow.conf == '') {
                        ## Also consider no capital
                        sumbymc2 <- allyr.ww2 %>% group_by(ISO, mc) %>%
                            dplyr::summarize(across(dimpact:slrloss, ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
                            group_by(ISO, mc) %>%
                            mutate(slrimpact=-slrloss, tradeimpact=-tradeloss,
                                   weight=weight.norm,
                                   total=totimpact + tradeimpact + slrimpact)

                        sumbymc3 <- sumbymc2 %>% group_by(ISO) %>%
                            dplyr::summarize(total=wtd.mean(total, weight, na.rm=T))

                        allassumps <- rbind(allassumps, cbind(trade.method.suffix=trade.method.suffix,
                                                              persist=persist, trade.method=trade.method,
                                                              solow.conf='none', slrconf=slrconf,
                                                              sumbymc3))
                    }
                    if (trade.method == 'dd' && solow.conf == '') {
                        ## Also consider no trade and no capital
                        sumbymc2 <- allyr.ww2 %>% group_by(ISO, mc) %>%
                            dplyr::summarize(across(dimpact:slrloss, ~ ifelse(all(is.na(.)), NA, tail(.[!is.na(.)], 1)))) %>%
                            group_by(ISO, mc) %>%
                            mutate(slrimpact=-slrloss, tradeimpact=0,
                                   weight=weight.norm,
                                   total=totimpact + tradeimpact + slrimpact)

                        sumbymc3 <- sumbymc2 %>% group_by(ISO) %>%
                            dplyr::summarize(total=wtd.mean(total, weight, na.rm=T))

                        allassumps <- rbind(allassumps, cbind(trade.method.suffix=trade.method.suffix,
                                                              persist=persist, trade.method='none',
                                                              solow.conf='none', slrconf=slrconf,
                                                              sumbymc3))
                    }
                }
            }
        }
    }
}

save(allassumps, file="data/allassumps-byiso.RData")
apply(allassumps[, 1:5], 2, function(xx) table(xx) / 258)

allassumps2 <- allassumps %>% group_by(ISO) %>%
    dplyr::summarize(fracagree=pmax(.5, mean(sign(total) == median(sign(total), na.rm=T), na.rm=T)))

shp2 <- shp %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(allassumps2, by=c('ADM0_A3'='ISO'))
centroids2 <- centroids %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(allassumps2, by=c('ADM0_A3'='ISO'))

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=fracagree, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    geom_label(data=subset(centroids2, show), aes(label=format.percent(fracagree)), size=3, label.padding=unit(0.1, "lines")) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_gradient("Portion of assumptions that agree with the median impact sign:", low = "white", high = scales::muted("blue"), labels=scales::percent, guide=guide_colorbar(barwidth=10, barheight=1)) +
    theme_bw() + theme(legend.position="bottom", legend.key.width=unit(10,"in"),
                       legend.key.height=unit(1,"in"))
ggsave(paste0("figures/signscale-assump.pdf"), width=10, height=5.5)

## Apply regression tree to each and see zones of agreement
library(rpart)

get_rpart_split <- function(fit, node = 1, pretty = 0) {
  if (!inherits(fit, "rpart")) stop("fit must be an rpart object")

  fr <- fit$frame
  node_chr <- as.character(node)

  if (!(node_chr %in% rownames(fr))) stop("node not found in fit$frame")
  node_row <- match(node_chr, rownames(fr))

  if (fr$var[node_row] == "<leaf>") stop("node is a leaf; no split to extract")

  left_node <- 2 * node
  right_node <- 2 * node + 1

  if (!(as.character(left_node) %in% rownames(fr)) ||
      !(as.character(right_node) %in% rownames(fr))) {
    stop("Could not find child nodes in fit$frame")
  }

  variable <- fr$var[node_row]
  left_mean_y <- unname(fr[as.character(left_node), "yval"])
  right_mean_y <- unname(fr[as.character(right_node), "yval"])

  # path.rpart returns the sequence of rules to reach a node
  left_path <- path.rpart(fit, nodes = left_node, print.it = FALSE)[[1]]
  right_path <- path.rpart(fit, nodes = right_node, print.it = FALSE)[[1]]

  # Drop the "root" entry if present, then take the last rule
  clean_last_rule <- function(path_vec) {
    path_vec <- path_vec[path_vec != "root"]
    if (length(path_vec) == 0) return(NA_character_)
    tail(path_vec, 1)
  }

  left_condition <- clean_last_rule(left_path)
  right_condition <- clean_last_rule(right_path)

  # Optionally prettify spacing a little
  if (pretty > 0) {
    prettify <- function(s) {
      s <- gsub("< ", "< ", s, fixed = TRUE)
      s <- gsub(">=", ">= ", s, fixed = TRUE)
      s <- gsub("=  ", "= ", s, fixed = TRUE)
      s
    }
    left_condition <- prettify(left_condition)
    right_condition <- prettify(right_condition)
  }

  list(
    variable = variable,
    node = node,
    left = list(
      node = left_node,
      condition = left_condition,
      mean_y = left_mean_y
    ),
    right = list(
      node = right_node,
      condition = right_condition,
      mean_y = right_mean_y
    )
  )
}

allassumps$trade.method.suffix <- factor(allassumps$trade.method.suffix)

treeres <- data.frame()
for (iso in unique(allassumps$ISO)) {
    mod <- rpart(total ~ trade.method.suffix + persist + trade.method + solow.conf + slrconf,
                 data=subset(allassumps, ISO == iso & persist != 0 & trade.method.suffix != '')) #  & trade.method != 'li'
    if (is.null(mod$split))
        next
    split <- get_rpart_split(mod, 1, 1)
    treeres <- rbind(treeres, data.frame(ISO=iso, variable=split$variable, left.condition=split$left$condition, left.value=split$left$mean_y, right.condition=split$right$condition, right.value=split$right$mean_y))
}

treeres2 <- treeres %>%
    mutate(large.condition=ifelse(abs(left.value) > abs(right.value), left.condition, right.condition),
           large.value=ifelse(abs(left.value) > abs(right.value), left.value, right.value))
table(treeres2$large.condition)

library(xtable)
print(xtable(treeres %>% mutate(left.value=100 * left.value, right.value = 100 * right.value), digits=1),
      include.rownames=F)

treeres$label <- ifelse(treeres$variable == "trade.method", "Trade Method",
                 ifelse(treeres$variable == "solow.conf", "Capital Model",
                 ifelse(treeres$variable == "persist", "Persistence",
                 ifelse(treeres$variable == "trade.method.suffix", "Meta-analysis",
                 ifelse(treeres$variable == "slrconf", "Coastal Impacts", "Unknown")))))

shp2 <- shp %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(treeres, by=c('ADM0_A3'='ISO'))
centroids2 <- centroids %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(treeres, by=c('ADM0_A3'='ISO'))

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=label, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_discrete(name="Key assumption dimension:") +
    theme_bw() + theme(legend.position="bottom")
ggsave(paste0("figures/assumpsplit.pdf"), width=10, height=5.5)

## Compare to RFFSPs

## 1. Evaluate middle block
load("data/allassumps-byiso.RData")

library(arrow)
library(dplyr)

rffdf <- data.frame()
for (ii in 1:10) {
    print(ii)
    df <- read_feather(paste0("data/rffsps/grows-", ii, ".feather"))

    ## df2 <- df %>% group_by(num, ISO) %>%
    ##     mutate(year0=c(2015, floor((period[-n()] + period[-1]) / 2)),
    ##            year1=c(floor((period[-n()] + period[-1]) / 2), 2350)) %>%
    ##     filter(period <= 2075) %>% mutate(year1=ifelse(period == 2075, 2078, year1)) %>%
    ##     dplyr::summarize(gdppc.change=prod(exp(gdppc.grow * (year1 - year0))))

    df2 <- df %>% group_by(num, ISO) %>%
        mutate(year0=c(2015, floor((period[-n()] + period[-1]) / 2)),
               year1=c(floor((period[-n()] + period[-1]) / 2), 2350)) %>%
        filter(period <= 2020) %>%
        dplyr::summarize(gdppc.grow=gdppc.grow[1], gdppc.change=prod(exp(gdppc.grow * (year1 - year0))))

    rffdf <- rbind(rffdf, df2)
}

## Question is, what range of the of growth effects that corresponds to a given extreme range

modelrange <- allres2 %>%
    ## filter((paper != "Zhao et al. 2018" | !(name %in% c("Table 3, Col. 2", "Table 3, Col. 3", "Table 3, Col. 5", "Table 3, Col. 6", "Table 3, Col. 7"))) &
    ##        (paper != "Kotz et al. 2022" | name != "With Linear Trends")) %>%
    group_by(ISO) %>%
    dplyr::summarize(range=max(dimpact, na.rm=T) - min(dimpact, na.rm=T),
                     paper=paper[which.max(abs(dimpact))], name=name[which.max(abs(dimpact))])
modelrange$paper[modelrange$range == 0] <- NA
modelrange$name[modelrange$range == 0] <- NA
mcrange <- sumbymc2 %>% group_by(ISO) %>%
    dplyr::summarize(range=max(exp(total), na.rm=T) / min(exp(total), na.rm=T))
assumprange <- allassumps %>% group_by(ISO) %>%
    dplyr::summarize(range=max(exp(total), na.rm=T) / min(exp(total), na.rm=T),
                     range.omegax0=max(exp(total[persist != 0]), na.rm=T) / min(exp(total[persist != 0]), na.rm=T),
                     trade.method.suffix=trade.method.suffix[which.max(abs(total))],
                     persist=persist[which.max(abs(total))],
                     trade.method=trade.method[which.max(abs(total))],
                     solow.conf=solow.conf[which.max(abs(total))],
                     slrconf=slrconf[which.max(abs(total))])

rangecomp <- data.frame()
for (iso in unique(rffdf2$ISO)) {
    rffdf.iso <- subset(rffdf, ISO == iso)
    modelrange.iso <- modelrange$range[modelrange$ISO == iso]
    mcrange.iso <- mcrange$range[mcrange$ISO == iso]
    assumprange.iso <- assumprange$range[assumprange$ISO == iso]
    assumprangeomegax0.iso <- assumprange$range.omegax0[assumprange$ISO == iso]
    if (length(modelrange.iso) == 0 || length(mcrange.iso) == 0 || length(assumprange.iso) == 0)
        next
    if (max(rffdf.iso$gdppc.grow) - min(rffdf.iso$gdppc.grow) < modelrange.iso) {
        modelsoln <- 0.5
    } else {
        modelsoln <- optimize(function(half) {
            rffrange <- quantile(rffdf.iso$gdppc.grow, .5 + half) - quantile(rffdf.iso$gdppc.grow, .5 - half)
            (rffrange - modelrange.iso)^2
        }, c(0, .5))$minimum
    }
    if (max(rffdf.iso$gdppc.change) / min(rffdf.iso$gdppc.change) < mcrange.iso) {
        mcsoln <- 0.5
    } else {
        mcsoln <- optimize(function(half) {
            rffrange <- quantile(rffdf.iso$gdppc.change, .5 + half) / quantile(rffdf.iso$gdppc.change, .5 - half)
            (rffrange - mcrange.iso)^2
        }, c(0, .5))$minimum
    }
    if (max(rffdf.iso$gdppc.change) / min(rffdf.iso$gdppc.change) < assumprange.iso) {
        assumpsoln <- 0.5
    } else {
        assumpsoln <- optimize(function(half) {
            rffrange <- quantile(rffdf.iso$gdppc.change, .5 + half) / quantile(rffdf.iso$gdppc.change, .5 - half)
            (rffrange - assumprange.iso)^2
        }, c(0, .5))$minimum
    }
    if (max(rffdf.iso$gdppc.change) / min(rffdf.iso$gdppc.change) < assumprangeomegax0.iso) {
        assumpomegax0soln <- 0.5
    } else {
        assumpomegax0soln <- optimize(function(half) {
            rffrange <- quantile(rffdf.iso$gdppc.change, .5 + half) / quantile(rffdf.iso$gdppc.change, .5 - half)
            (rffrange - assumprangeomegax0.iso)^2
        }, c(0, .5))$minimum
    }

    rangecomp <- rbind(rangecomp,
                       data.frame(ISO=iso, rffrange=max(rffdf.iso$gdppc.change) / min(rffdf.iso$gdppc.change),
                                  modelrange=modelrange.iso, modelsoln=2 * modelsoln,
                                  mcrange=mcrange.iso, mcsoln=2 * mcsoln, assumprange=assumprange.iso, assumpsoln=2 * assumpsoln,
                                  assumprangeomegax0=assumprangeomegax0.iso, assumpomegax0soln=2 * assumpomegax0soln,
                                  trade.method.suffix=assumprange$trade.method.suffix[assumprange$ISO == iso],
                                  persist=assumprange$persist[assumprange$ISO == iso],
                                  trade.method=assumprange$trade.method[assumprange$ISO == iso],
                                  solow.conf=assumprange$solow.conf[assumprange$ISO == iso],
                                  slrconf=assumprange$slrconf[assumprange$ISO == iso]))
}

library(xtable)
print(xtable(rangecomp[, 1:10], digits=2), include.rownames=F)

rangecomp$maxsoln <- ifelse(rangecomp$mcsoln > rangecomp$assumpomegax0soln,
                            rangecomp$mcsoln, -rangecomp$assumpomegax0soln)

shp2 <- shp %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(rangecomp, by=c('ADM0_A3'='ISO'))
centroids2 <- centroids %>% left_join(polydata[, c('PID', 'ADM0_A3')]) %>% left_join(rangecomp, by=c('ADM0_A3'='ISO'))

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=modelsoln, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_distiller("RFFSP quantile range of model impacts:", palette="YlOrRd", direction=1, labels=scales::percent, limits=c(0, 1)) +
    theme_bw() + theme(legend.position="bottom")
ggsave(paste0("figures/rffcomp-model.pdf"), width=10, height=5.5)

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=mcsoln, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_distiller("RFFSP quantile range of preferred outcomes:", palette="YlOrRd", direction=1, labels=scales::percent, limits=c(0, 1)) +
    theme_bw() + theme(legend.position="bottom")
ggsave(paste0("figures/rffcomp-mc.pdf"), width=10, height=5.5)

gg <- ggplot(shp2, aes(X, Y)) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill='#808080', colour=NA) +
    geom_polygon(aes(fill=assumpomegax0soln, group=paste(PID, SID))) +
    geom_polygon(data=shpl, aes(group=paste(PID, SID)), fill=NA, colour='black', linewidth=.01) +
    xlab(NULL) + ylab(NULL) + coord_map(ylim=c(-50, 65)) +
    scale_fill_distiller("RFFSP quantile range of available assumptions:", palette="YlOrRd", direction=1, labels=scales::percent, limits=c(0, 1)) +
    theme_bw() + theme(legend.position="bottom")
ggsave(paste0("figures/rffcomp-assump.pdf"), width=10, height=5.5)

mean(rangecomp$mcsoln)
