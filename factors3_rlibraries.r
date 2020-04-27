# https://r-forge.r-project.org/scm/viewvc.php/*checkout*/tutorial/TutorialExplainingStrucChangeBasics.pdf?root=bfast

library("readr")
library("ggplot2")
library("bfast")
library("zoo")
library("strucchange")

Developed_3_Factors <- read_csv("sample_data/Developed_3_Factors.csv")

Developed_3_Factors

df <- Developed_3_Factors[, c(1,4)]
df$t <- 1:nrow(df) 

# Initial data plot
plot(df, type='l')
abline(h=mean(df$HML),col='blue')

# CUMSUM & MOSUM

mean <- mean(df$HML)

cumsum <- cumsum(df$HML - mean)
plot(cumsum, type='l')

mosum <- rollapply(df$HML - mean, 15, sum)
plot(mosum, type='l')

# REC CUMSUM

rec_cumsum <- cumsum(c(0, recresid(lm(df$HML ~ 1))))
plot(rec_cumsum, type='l')

# OLS-CUMSUM, OLS-MOSUM, OLS-REC_CUMSUM

opar <- par(mfrow=c(2,2))

ols_cumsum <- efp(df$HML ~ 1, type = "OLS-CUSUM")
plot(ols_cumsum)

ols_mosum <- efp(df$HML ~ 1, type = "OLS-MOSUM")
plot(ols_mosum)

ols_rec_cumsum <- efp(df$HML ~ 1, type = "Rec-CUSUM")
plot(ols_rec_cumsum)

par (opar)

# Determine breakpoint
# Min redisueal sum of squares

data_len <- dim(df)

plot(df$t, sapply(df$t, function(i){
  before <- 1:i
  after <- (i+1):data_len[1]
  res <- c(df$HML[before] - mean(df$HML[before]), df$HML[after] - mean(df$HML[after]))
  sum(res^2)
}), type = "l")

# strucchange date of change

bp <- breakpoints(df$HML ~ 1)
fac <- breakfactor(bp, breaks = 4 )
fm1 <- lm(df$HML ~ fac - 1)

plot(bp)

opar <- par(mfrow=c(2,1), mar=c(2,2,0,2))

plot(ols_cumsum, alt.boundary = F)

plot(df, type='l')
abline(v= 115, lty=2, col='red')
lines(ts(predict(fm1),start=1,freq=1), col='darkgreen',lwd=2)

par(opar)