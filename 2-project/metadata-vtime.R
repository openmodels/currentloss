## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(dplyr)
library(reshape2)
library(ggplot2)

source("src/lib/loadmetadata.R")

metadata2.mu <- metadata %>% group_by(Paper) %>% summarize(Year=as.numeric(substring(Paper[1], nchar(Paper[1]) - 3, nchar(Paper[1]))),
                                                           `Data Range`=NA,
                                                           `Weather Population-Weighting`=mean(Q.Weather),
                                                           `Poor-Country Handling`=mean(Q.Poverty),
                                                           `Temperature Flexibility`=mean(Q.Temp),
                                                           `Precipitation Flexibility`=mean(Q.Prec),
                                                           `Year FE Extensiveness`=mean(Q.YearFE),
                                                           `Trends Flexibility`=mean(Q.Trends),
                                                           `Other FE Extensiveness`=mean(Q.OtherFE),
                                                           `Controls Extensiveness`=mean(Q.Control),
                                                           `Growth Lags Number`=mean(as.numeric(`Growth Lags`)))
metadata2.mu <- metadata2.mu %>% group_by(Year) %>% mutate(Year=seq(Year[1], Year[1]+1, length.out=length(Year)+1)[-(length(Year)+1)])

metadata2.lo <- metadata %>% group_by(Paper) %>% summarize(Year=as.numeric(substring(Paper[1], nchar(Paper[1]) - 3, nchar(Paper[1]))),
                                                           `Data Range`=median(first.year),
                                                           `Weather Population-Weighting`=quantile(Q.Weather, 0.0),
                                                           `Poor-Country Handling`=quantile(Q.Poverty, 0.0),
                                                           `Temperature Flexibility`=quantile(Q.Temp, 0.0),
                                                           `Precipitation Flexibility`=quantile(Q.Prec, 0.0),
                                                           `Year FE Extensiveness`=quantile(Q.YearFE, 0.0),
                                                           `Trends Flexibility`=quantile(Q.Trends, 0.0),
                                                           `Other FE Extensiveness`=quantile(Q.OtherFE, 0.0),
                                                           `Controls Extensiveness`=quantile(Q.Control, 0.0),
                                                           `Growth Lags Number`=quantile(as.numeric(`Growth Lags`), 0.0))
metadata2.lo <- metadata2.lo %>% group_by(Year) %>% mutate(Year=seq(Year[1], Year[1]+1, length.out=length(Year)+1)[-(length(Year)+1)])

metadata2.hi <- metadata %>% group_by(Paper) %>% summarize(Year=as.numeric(substring(Paper[1], nchar(Paper[1]) - 3, nchar(Paper[1]))),
                                                           `Data Range`=median(last.year),
                                                           `Weather Population-Weighting`=quantile(Q.Weather, 1.0),
                                                           `Poor-Country Handling`=quantile(Q.Poverty, 1.0),
                                                           `Temperature Flexibility`=quantile(Q.Temp, 1.0),
                                                           `Precipitation Flexibility`=quantile(Q.Prec, 1.0),
                                                           `Year FE Extensiveness`=quantile(Q.YearFE, 1.0),
                                                           `Trends Flexibility`=quantile(Q.Trends, 1.0),
                                                           `Other FE Extensiveness`=quantile(Q.OtherFE, 1.0),
                                                           `Controls Extensiveness`=quantile(Q.Control, 1.0),
                                                           `Growth Lags Number`=quantile(as.numeric(`Growth Lags`), 1.0))
metadata2.hi <- metadata2.hi %>% group_by(Year) %>% mutate(Year=seq(Year[1], Year[1]+1, length.out=length(Year)+1)[-(length(Year)+1)])

metadata3 <- melt(metadata2.mu, c('Paper', 'Year')) %>% left_join(melt(metadata2.lo, c('Paper', 'Year')), by=c('Paper', 'Year', 'variable'), suffix=c('', '.lo')) %>%
    left_join(melt(metadata2.hi, c('Paper', 'Year')), by=c('Paper', 'Year', 'variable'), suffix=c('.mu', '.hi'))

ggplot(metadata3, aes(Year, value.mu)) +
    facet_wrap(~ variable, scales='free_y', ncol=2) +
    geom_point(data=subset(metadata3, value.lo == value.hi)) +
    geom_linerange(aes(ymin=value.lo, ymax=value.hi)) +
    theme_bw() + xlab("Publication Year") + ylab("Year (for data) or Relative Quality (otherwise)")
ggsave("figures/metabyyear.pdf", width=6.5, height=6)
