## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(reshape2)
library(ggplot2)
source("src/3-othereffects/trade-io.R")

io <- load.io(2016)

pdf <- melt(io[['TT']])
pdf$seller <- io[['labels']]$V1[pdf$Var1]
pdf$buyer <- io[['labels']]$V1[pdf$Var2]

pdf$seller <- factor(pdf$seller, levels=rev(io[['labels']]$V1))
pdf$buyer <- factor(pdf$buyer, levels=io[['labels']]$V1)

ggplot(pdf, aes(buyer, seller, fill=value)) +
    geom_raster() +
    scale_fill_distiller(palette="Blues", direction=1, trans='log10') +
    xlab("Buyer") + ylab("Seller") +
    theme(text=element_text(size=4))

fd <- load.fd(2016)

FD2 <- fd$FD
labels2 <- fd$labels

FD3 <- matrix(0, ncol(FD2) - 1, ncol(FD2))
for (ii in 1:26)
    FD3[,] <- FD3 + FD2[seq(ii, nrow(FD2) - 1, by=26),]

FD4 <- rbind(FD3, FD2[nrow(FD2),])

pdf <- melt(FD4)
pdf$seller <- io[['labels']]$V1[pdf$Var1]
pdf$buyer <- io[['labels']]$V1[pdf$Var2]

pdf$seller <- factor(pdf$seller, levels=rev(io[['labels']]$V1))
pdf$buyer <- factor(pdf$buyer, levels=io[['labels']]$V1)

ggplot(pdf, aes(buyer, seller, fill=value)) +
    geom_raster() +
    scale_fill_distiller(palette='Reds', direction=1, trans='log10') +
    xlab("Buyer") + ylab("Seller") +
    theme(text=element_text(size=4))

labels2$buyer <- factor(labels2$V1, levels=io[['labels']]$V1)
labels2$seller <- factor(c('USA'), levels=rev(io[['labels']]$V1))

ggplot(labels2, aes(buyer, seller, fill=VA)) +
    geom_raster() +
    scale_fill_distiller(palette='Purples', direction=1, trans='log10') +
    xlab("Buyer") +
    theme(text=element_text(size=4))

labels3 <- io[['labels']]

labels3$buyer <- factor(labels3$V1, levels=io[['labels']]$V1)
labels3$seller <- factor(c('USA'), levels=rev(io[['labels']]$V1))

ggplot(labels3, aes(buyer, seller, fill=FD)) +
    geom_raster() +
    scale_fill_distiller(palette='Reds', direction=1, trans='log10') +
    xlab("Buyer") +
    theme(text=element_text(size=4))

pdf <- subset(comtrade, Period == 2022 & FlowDesc == 'Export')
pdf$buyer <- factor(pdf$ReporterISO, levels=io[['labels']]$V1)
pdf$seller <- factor(pdf$PartnerISO, levels=rev(io[['labels']]$V1))

ggplot(pdf, aes(buyer, seller, fill=PrimaryValue)) +
    geom_raster() +
    scale_fill_distiller(palette='Greens', direction=1, trans='log10') +
    xlab("Buyer") +
    theme(text=element_text(size=4))
