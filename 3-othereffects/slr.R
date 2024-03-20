load("/mnt/423808E91650DDB1/shared/currentloss/slr/totalcosts.RData")

library(dplyr)

df$ssp = substr(df$scenario, 1, 4)
df$adm0 = substr(df$adm1, 1, 3)

df2 = df %>% filter(year <= 2022) %>%
  group_by(adm0, iam, quantile, scenario, ssp, year, case_type) %>%
  summarize(costs=sum(costs)) %>% # sum to ADM0
  group_by(adm0, year, case_type) %>% summarize(q05=quantile(costs, .05), mu=mean(costs), q95=quantile(costs, .95))

write.csv(df2, "data/slrbyadm0.csv", row.names=F)