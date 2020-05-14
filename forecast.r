  library("zoo")
  # install.packages('forecast', dependencies = TRUE)
  library(forecast)
  library(ggplot2)
  library("bfast")
  library(readxl)
  library("strucchange")
  pwt91 <- read_excel("sample_data/pwt91.xlsx", sheet = "Data")
  
    greece <- subset(pwt91,countrycode=="YEM",select = c("year","rgdpe"))
  # remove rows with NA
  greece <- greece[complete.cases(greece),]
  
  # plot gdp 
  # ggplot(greece, aes(x = year, y = rgdpe)) + geom_line()
  
  #start_year <-greece$year[1]
  greece <- ts(greece$rgdpe,start = greece$year[1], frequency=1)
  # greece <- Nile
  start_year = time(greece)[1]
  
  # calculating percentage growth in time
  prc <- sapply(1:length(greece)-1, function(i){
    1-greece[i]/greece[i+1]
  })
  start_year = start_year + 1
  greece <- ts(unlist(prc, use.names=FALSE),start = start_year,frequency=1)
  
  plot(greece)
  # cusum and mosum for residuals (value - mean) 
  #plot cumsum
  CUSUM = cumsum(greece - mean(greece))
  CUSUM <- ts(CUSUM,start =start_year)
  plot(CUSUM, type ="l")
  abline(a=0,b=0, col="green")
  
    
  # plot moving sum
  MOSUM = rollapply(greece - mean(greece), 15, sum)
  plot(MOSUM)
  abline(a=0,b=0, col="green")
  
  # plot rec-cumsum (mean of all observations prior to given time)
  rec_cusum <- numeric(length(greece))
  for (i in 1:length(greece)){
    rec_cusum[i] = mean(greece[1:(i-1)])
  }
  rec_cusum <- ts(rec_cusum,start = start_year,frequency = 1)
  
  plot(greece)
  lines(rec_cusum,col="red")
  
  # plot cumulated recursive residuals
  model <- lm(greece ~ 1)
  plot(greece)
  abline(model)
  # one step ahead prediction errors
  
  #rec_residuals <- numeric(length(greece))
  #for (i in 2:length(greece)){
  #  rec_residuals[i-1] = predict.lm(model,greece[i])
  #}
    # calculate residual sum of squares
    
    rss =  sapply(5:(length(greece)-5), function(i) {
       before <- 1:i
       after <- (i+1):length(greece)
      res <- c(Nile[before] - mean(Nile[before]), Nile[after] - mean(Nile[after]))
       sum(res^2)
       })
  
    plot(start_year + 5:(length(greece)-5), rss, type = "b", xlab = "Time", ylab = "RSS")
    
    rss <- ts(rss,start=start_year-1+5,frequency=1)
    #breakpoint <- min(rss)
    breakpoint <- time(rss)[which.min(rss)]
   
    
    
    plot(greece)
    abline(v=breakpoint, col="red",lty=2)
    lines(ts(predict(lm((greece[1:(breakpoint-start_year+1)]) ~ 1)),start=start_year,freq=1),col='darkgreen',lwd=2)
    lines(ts(predict(lm((greece[(breakpoint-start_year+1):length(greece)]) ~ 1)),start=breakpoint,freq=1),col='darkgreen',lwd=2)
  
  
  # stuff from paper
    
       bp.nile <- breakpoints(greece ~ 1)
       nile.fac <- breakfactor(bp.nile, breaks = length(bp.nile$breakpoints) )
       fm1.nile <- lm(greece ~ nile.fac - 1)
       plot(bp.nile)
       
       plot(greece, ylab="RGDPE")
      # abline(h= mean(greece),col='blue')
        abline(v= breakpoint-1, lty=2, col='red')
       lines(ts(predict(fm1.nile),start=start_year,freq=1), col='darkgreen',lwd=2)
       ci_ts <- confint(bp.nile)
       lines(ci_ts)
  
  
  
