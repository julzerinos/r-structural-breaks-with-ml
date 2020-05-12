# Chow test
# F-test to check if break-points is actually a break-point
# https://www.aptech.com/structural-breaks/

library("readr")
library("strucchange")

Developed_3_Factors <- read_csv("sample_data/Developed_3_Factors.csv")

df <- Developed_3_Factors[, 4]

df

df <- as.ts(df)

bp <- breakpoints(df ~ 1)

summary(bp)

ci_ts <- confint(bp)

plot(df)
lines(bp)
lines(ci_ts)

