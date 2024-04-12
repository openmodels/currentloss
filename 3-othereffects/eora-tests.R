## setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/My Drive/Current Losses")
## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

datapath <- "data/I_O data/Eora26/Eora26_2015_bp"
TT <- as.matrix(read.delim(file.path(datapath, "Eora26_2015_bp_T.txt"), sep='\t', header=F))
VA <- as.matrix(read.delim(file.path(datapath, "Eora26_2015_bp_VA.txt"), sep='\t', header=F))
FD <- as.matrix(read.delim(file.path(datapath, "Eora26_2015_bp_FD.txt"), sep='\t', header=F))

## Test 1: Sums
xout <- rowSums(TT) + rowSums(FD)
xin <- colSums(TT) + colSums(VA)

quantile(xout / xin, na.rm=T)

## Aggregation
library(raster)
library(dplyr)

rTT <- raster(TT)
rTT2 <- aggregate(rTT, 26, sum)
TT2 <- as.matrix(rTT2)

labels <- read.delim(file.path(datapath, "labels_T.txt"), sep='\t', header=F)
labels$V1 <- factor(labels$V1, levels=unique(labels$V1))

labels$VA <- colSums(VA)
labels$FD <- rowSums(FD)

labels2 <- labels %>% group_by(V1) %>% summarize(VA=sum(VA), FD=sum(FD))

## Test 2: Sums
xout <- rowSums(TT2) + labels2$FD
xin <- colSums(TT2) + labels2$VA

quantile(xout / xin)

library(ggplot2)
ggplot(data.frame(xx=xout / xin), aes(xx)) +
    geom_histogram() + theme_bw() + xlab("Xout / Xin")

## Test 3: Calculate Z
AA2 <- TT2 %*% diag(1 / xin)
LL2 <- solve(diag(dim(AA2)[1]) - AA2)

range(LL2)
