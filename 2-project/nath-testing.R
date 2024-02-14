library(ggplot2)

df <- data.frame()
for (omega in c(0, 0.5, 1, 0.08)) {
    qq <- c(0, -0.01)
    for (tt in 1:9)
        qq <- c(qq, (1 - omega)*qq[length(qq)])
    df <- rbind(df, data.frame(omega, tt=0:10, qq))
}

ggplot(df, aes(tt, qq, colour=factor(omega))) +
    geom_line()

df <- data.frame()
for (omega in c(0, 0.5, 1, 0.08)) {
    qq <- stats::filter(c(rep(0, 30), rep(-0.02, 30)), (1 - omega)^(0:30), sides=1)[-1:-30]
    df <- rbind(df, data.frame(omega, tt=-2:5, qq=c(0, 0, qq[1:6])))
}
df$tq <- df$qq + (0:7)*.03

ggplot(df, aes(tt, 1 + tq, colour=factor(omega))) +
    geom_line(data=data.frame(omega='base', tt=-2:5, tq=(0:7)*.03)) +
    geom_line()
