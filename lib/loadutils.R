library(readxl)
library(reshape2)

read.wb <- function(filepath, value.name) {
    df <- read_xls(filepath, skip=3)
    df2 <- melt(df[, c(-1, -3, -4)], 'Country Code', variable.name='Year', value.name=value.name)
    df2$Year <- as.numeric(as.character(df2$Year))
    df2
}
