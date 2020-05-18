      library(zoo)         # time series library
      library(ggplot2)     # graphing library
      library(readxl)      # reading excel files library
      library(strucchange) # library implementing breakpoint analysis
      library(ggfortify)   # extension for ggplot time series graphing
      
      # Country codes list
      # https://web.stanford.edu/~chadj/shcodes.txt
      country_code = "ZWE"
        
      # Read and transfrom data
      pwt91 <- read_excel("sample_data/pwt91.xlsx", sheet = "Data")
      country_gdp <- subset(pwt91,countrycode==country_code,select = c("year","rgdpe"))
      # remove rows with NA
      country_gdp <- country_gdp[complete.cases(country_gdp),]
      country_gdp <- ts(country_gdp$rgdpe,start = country_gdp$year[1], frequency=1)
      start_year = time(country_gdp)[1]
      autoplot(country_gdp) + labs(title=paste("RGDP growth in",country_code),x="year",y="RGDP value")
      
      # calculating percentage growth in time
      prc <- sapply(1:length(country_gdp)-1, function(i){
        1-country_gdp[i]/country_gdp[i+1]
      })
      start_year = start_year + 1
      country_gdp <- ts(unlist(prc, use.names=FALSE),start = start_year,frequency=1)
    
      
      # Plot the data and CUSUM & MOSUM 
      autoplot(country_gdp) + labs(title=paste("Percentage change of RGDP per year in",country_code), x="year", y="percent in decimal")
      # cusum and mosum for residuals (value - mean) 
      #plot cumsum
      CUSUM = cumsum(country_gdp - mean(country_gdp))
      CUSUM <- ts(CUSUM,start =start_year)
      autoplot(CUSUM)+ geom_hline(yintercept = 0,color="blue")+labs(title="Cumulative Sum", x="year", y="percent in decimal")
      
      # plot moving sum
      MOSUM = rollapply(country_gdp - mean(country_gdp), 15, sum)
      autoplot(MOSUM)+ geom_hline(yintercept = 0,color="blue")+labs(title="Moving Sum from 15 observations", x="year", y="percent in decimal")
      
      # plot rec-cumsum (mean of all observations prior to given time)
      rec_cusum <- numeric(length(country_gdp))
      for (i in 1:length(country_gdp)){
        rec_cusum[i] = mean(country_gdp[1:(i-1)])
      }
      rec_cusum <- ts(rec_cusum,start = start_year,frequency = 1)
      autoplot(rec_cusum)+labs(title="Recursive Cumulative Sum", x="year", y="percent in decimal")
    
    
      # calculate residual sum of squares
      rss =  sapply(5:(length(country_gdp)-5), function(i) {
        before <- 1:i
        after <- (i+1):length(country_gdp)
        res <- c(Nile[before] - mean(Nile[before]), Nile[after] - mean(Nile[after]))
        sum(res^2)
        })
    
      rss <- ts(rss,start=start_year-1+5,frequency=1)
      autoplot(rss) + labs(title = "Residual Sum of Squares",x="Year",y="RSS")
      
      # find suspected breakpoint
      breakpoint <- time(rss)[which.min(rss)]
      plot(country_gdp)
      abline(v=breakpoint, col="red",lty=2)
      lines(ts(predict(lm((country_gdp[1:(breakpoint-start_year+1)]) ~ 1)),start=start_year,freq=1),col='darkgreen',lwd=2)
      lines(ts(predict(lm((country_gdp[(breakpoint-start_year+1):length(country_gdp)]) ~ 1)),start=breakpoint,freq=1),col='darkgreen',lwd=2)
      
      #-------------------------------
      # Strucchange based calculations
      # 1. Simple model country_gdp ~ 1   (y = β0) β0 = mean
      
      # F statistics at 5% significance level, h=0.1 (partition size)
      fs <- Fstats(country_gdp ~ 1,from=0.1)
      plot(fs,main="F test")
      cat("Potential breakpoint form F test at:", time(country_gdp)[fs$breakpoint],"\n")
      
      # test the null hypothesis that the GDP growth remains constant over the years
      # compute OLS-based CUSUM & MOSUM process and plot
      
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
      
      # finding date of the breakpoint
      bp <- breakpoints(country_gdp ~ 1)
      for(i in bp$breakpoints){cat("Breakpoint found at year:",time(country_gdp)[i],"\n")}
      # finding segmentation of the data
      bfac <- breakfactor(bp, breaks = length(bp$breakpoints))
      # model
      fm <- lm(country_gdp ~ bfac - 1)
      plot(bp)
      autoplot(bp)
      
      plot(country_gdp, ylab="RGDPE")
      lines(ts(predict(fm),start=start_year,freq=1), col='darkgreen',lwd=2)
      # compute 95% confidence intervals
      ci_ts <- confint(bp)
      lines(ci_ts)
      
      # 2. model with two explanatory variables
    
      gdp_lag <- cbind(country_gdp, lag(country_gdp, k = -1), lag(country_gdp, k = -3))
      colnames(gdp_lag) <- c("y", "ylag1", "ylag3")
      
      gdp_lag <- window(gdp_lag, start = time(gdp_lag)[4], end = time(gdp_lag)[length(gdp_lag)/3-3])
      model <- lm(y~ylag1 + ylag3,data=gdp_lag)
      # EFP 
      re.seat <- efp(y ~ ylag1 + ylag3, data = gdp_lag, type = "RE")
      plot(re.seat)
      sctest(re.seat)
      # F statistics        
      fs <- Fstats(y ~ ylag1 + ylag3, data = gdp_lag, from = 0.1)
      plot(fs)
      cat("Potential breakpoint form F test (multivariable regression) at:", time(gdp_lag[,"y"])[fs$breakpoint],"\n")
      
      plot(gdp_lag[,"y"],main="Breapoint from F test",ylab="RGDP",xlab="year")
      abline(v=time(gdp_lag)[fs$breakpoint],col="red",lty=2)
      # reset plot arguments
      par(mfrow=c(1,1))
  
    
  
  
  
    