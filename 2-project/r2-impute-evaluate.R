## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(readxl)
library(dplyr)
library(mice)

do.include.all <- T
source("src/lib/loadmetadata.R")

hist(metadata$`Total R2`)

## Test: Predict logs
for (col in c('Total R2', 'Adjusted R2', 'Within R2')) {
    metadata[!is.na(metadata[, col]) & metadata[, col] <= 0, col] <- NA
    ## mod <- lm(as.formula(paste0("log(`", col, "`) ~ 0 + Paper")), metadata)
    ## metadata[-mod$na.action, paste(col, "Resid")] <- resid(mod)
    metadata[, paste(col, "Log")] <- log(metadata[, col])
}

results <- data.frame()
for (paper in unique(metadata$Paper)) {
    print(paper)
    for (col in paste(c('Total R2', 'Adjusted R2', 'Within R2'), "Log")) {
        if (any(!is.na(metadata[metadata$Paper == paper, col]))) {
            rows <- !is.na(metadata[, col]) & metadata$Paper == paper
            metadata.x <- metadata
            metadata.x[metadata$Paper == paper, col] <- NA
            micemodel <- mice(metadata.x[, c(grep("Q.", names(metadata)), which(names(metadata) == "Dep. Averages"), grep("R2 Log", names(metadata)))], method='pmm', m=10, maxit=10)
            metadata2 <- complete(micemodel)

            results <- rbind(results, data.frame(Paper=paper, Name=metadata$Name[rows], Column=col,
                                                 True=metadata[rows, col, drop=T], Imputed=metadata2[rows, col, drop=T]))
        }
    }
}

library(ggplot2)

ggplot(results, aes(exp(True), exp(Imputed), colour=Column)) +
    geom_point() +
    geom_abline() +
    scale_colour_discrete(name="Evaluated:") +
    theme_bw() + scale_x_log10(name="True R²") + scale_y_log10(name="Imputed R²")
ggsave("figures/r2-impute.pdf", width=6.5, height=5)

cor(results$True, results$Imputed, use='complete') * cor(results$True[results$Column == 'Total R2 Log'], results$Imputed[results$Column == 'Total R2 Log'], use='complete')

## pmm: 0.09785218

cor(results$True, results$Imputed, use='complete') # 0.5666143
cor(results$True[results$Column == 'Total R2 Log'], results$Imputed[results$Column == 'Total R2 Log'], use='complete') # 0.1726963
