  library("zoo")
  library(ggplot2)
  #library("bfast")
  library(readxl)
  library("strucchange")
  
  # Read and transfrom data
  pwt91 <- read_excel("sample_data/pwt91.xlsx", sheet = "Data")
  country_gdp <- subset(pwt91,countrycode=="SWZ",select = c("year","rgdpe"))
  # remove rows with NA
  country_gdp <- country_gdp[complete.cases(country_gdp),]
  country_gdp <- ts(country_gdp$rgdpe,start = country_gdp$year[1], frequency=1)
  start_year = time(country_gdp)[1]
  
  
  # calculating percentage growth in time
  prc <- sapply(1:length(country_gdp)-1, function(i){
    1-country_gdp[i]/country_gdp[i+1]
  })
  start_year = start_year + 1
  country_gdp <- ts(unlist(prc, use.names=FALSE),start = start_year,frequency=1)

  
  # Plot the data and CUSUM & MOSUM 
  plot(country_gdp)
  # cusum and mosum for residuals (value - mean) 
  #plot cumsum
  CUSUM = cumsum(country_gdp - mean(country_gdp))
  CUSUM <- ts(CUSUM,start =start_year)
  plot(CUSUM, type ="l")
  abline(a=0,b=0, col="green")
  # plot moving sum
  MOSUM = rollapply(country_gdp - mean(country_gdp), 15, sum)
  plot(MOSUM)
  abline(a=0,b=0, col="green")
  
  # plot rec-cumsum (mean of all observations prior to given time)
  rec_cusum <- numeric(length(country_gdp))
  for (i in 1:length(country_gdp)){
    rec_cusum[i] = mean(country_gdp[1:(i-1)])
  }
  rec_cusum <- ts(rec_cusum,start = start_year,frequency = 1)
  
  plot(country_gdp)
  lines(rec_cusum,col="red")
  
  # plot cumulated recursive residuals
  model <- lm(country_gdp ~ 1)
  plot(country_gdp)
  abline(model)

  # calculate residual sum of squares
    
  rss =  sapply(5:(length(country_gdp)-5), function(i) {
    before <- 1:i
    after <- (i+1):length(country_gdp)
    res <- c(Nile[before] - mean(Nile[before]), Nile[after] - mean(Nile[after]))
    sum(res^2)
    })
  
  plot(start_year + 5:(length(country_gdp)-5), rss, type = "b", xlab = "Time", ylab = "RSS")
    
  rss <- ts(rss,start=start_year-1+5,frequency=1)
  breakpoint <- time(rss)[which.min(rss)]
   
    
    
  plot(country_gdp)
  abline(v=breakpoint, col="red",lty=2)
  lines(ts(predict(lm((country_gdp[1:(breakpoint-start_year+1)]) ~ 1)),start=start_year,freq=1),col='darkgreen',lwd=2)
  lines(ts(predict(lm((country_gdp[(breakpoint-start_year+1):length(country_gdp)]) ~ 1)),start=breakpoint,freq=1),col='darkgreen',lwd=2)
  
  
  # Strucchange based calculations
    
  bp.nile <- breakpoints(country_gdp ~ 1)
  nile.fac <- breakfactor(bp.nile, breaks = length(bp.nile$breakpoints) )
  fm1.nile <- lm(country_gdp ~ nile.fac - 1)
  plot(bp.nile)
  
  plot(country_gdp, ylab="RGDPE")
  # abline(h= mean(country_gdp),col='blue')
  abline(v= breakpoint-1, lty=2, col='red')
  lines(ts(predict(fm1.nile),start=start_year,freq=1), col='darkgreen',lwd=2)
  ci_ts <- confint(bp.nile)
  lines(ci_ts)
  
  ## test the null hypothesis that the GDP growth remains constant
  ## over the years
  ## compute OLS-based CUSUM process and plot
  ## with standard and alternative boundaries
  #TODO: 4 plots combined with all efp tests
  
  par(mfrow=c(2,2))
  ocus <- efp(country_gdp ~ 1, type = "OLS-CUSUM")
  omos <- efp(country_gdp ~ 1, type = "OLS-MOSUM")
  rmos <- efp(country_gdp ~ 1, type = "Rec-CUSUM")
  rcus <- efp(country_gdp ~ 1, type = "Rec-MOSUM")
  plot(ocus)
  plot(omos)
  plot(rmos)
  plot(rcus)
  par(mfrow=c(1,1))
  
  #abline(v= breakpoint-1, lty=2, col='red')
  plot(ocus, alpha = 0.01, alt.boundary = TRUE)
  ## calculate corresponding test statistic
  sctest(ocus)
    
  ## F statistics at 5% significance level h=0.15
  fs <- Fstats(country_gdp ~ 1,from=0.1)
  plot(fs)


  