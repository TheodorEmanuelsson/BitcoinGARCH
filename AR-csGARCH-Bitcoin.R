#############################
# Bitcoin Price data
#############################

#######
### libraries
library(rugarch)
library(PerformanceAnalytics)
# Need xts installed
# Need tseries installed
# Need forecast installed
# Need FinTS installed
#######


#######
### Plot function for a nicer ACF plot
acf_rugarch = function(x, ...)
{   
  T = length(x)
  insample = 1:T
  xseries = x
  lag.max = as.integer(10*log10(T))
  acfx    = acf(xseries, lag.max = lag.max, plot = FALSE)
  clim0   = qnorm((1 + 0.95)/2)/sqrt(acfx$n.used)
  ylim 	= range(c(-clim0, clim0, as.numeric(acfx$acf)[-1]))
  clx     = vector(mode = "character", length = lag.max)
  clx[which(as.numeric(acfx$acf)[-1]>=0)] = "steelblue"
  clx[which(as.numeric(acfx$acf)[-1]<0)] = "orange"
  barplot(height = as.numeric(acfx$acf)[-1], names.arg = as.numeric(acfx$lag)[-1], ylim = 1.2*ylim, col = clx,
          ylab = "ACF", xlab="lag", main = "ACF", cex.main = 0.8)
  abline(h = c(clim0, -clim0), col = "tomato1", lty = 2)
  abline(h = 0, col = "black", lty = 1)
  box()   
  grid()
}
#######

#######
# Import data
Bitcoin.data <- readr::read_csv("BTC_USD_2013-10-01_2020-12-26-CoinDesk.csv") # Edit to the file name of current data
df <- data.frame(Bitcoin.data$Date, Bitcoin.data$`Closing Price (USD)`) # Create a dataframe of data and closing price
#######

#######
### Timeseries object
df <- xts::xts(df$Bitcoin.data..Closing.Price..USD.., order.by = df$Bitcoin.data.Date)
plot(df, main = "Timeseries Bitcoin price", ylab = "USD", col = "Blue")
acf_rugarch(df) # Autokorrelation plot
pacf(df) # Partial autocorrelation plot
tseries::adf.test(df) # Augmented-Dickey-Fuller test: Timeseries not stationary

hist(df) # histogram of the price data
#######
# Calculate Returns
df.returns <- CalculateReturns(df)
df.returns <- df.returns[-1,] # remove the first observation
plot(df.returns, col = "Blue") # Timeseries plot of returns
acf_rugarch(df.returns) # Autokorrelation plot: Low dependancy structure left after calculating returns
hist(df.returns) # Histogram of returns data
tseries::adf.test(df.returns) # Augmented-Dickey-Fuller test: Timeseries stationary by test


# PLOTS for report
par(mfrow=c(2,1))
plot(df, main = "Timeseries Bitcoin Price", ylab = "USD", col = "blue")
plot(df.returns, main = "Timeseries Bitcoin Returns", ylab = "Return", col = "blue")

#######
### Arima modelling. Using a algorithm estimating best model based on AIC.
# Returns
SearchARIMA <- forecast::auto.arima(df.returns, trace = TRUE, test="kpss", ic="aic", stationary = TRUE, stepwise = FALSE, approximation = FALSE, seasonal = FALSE)
print(SearchARIMA) # Best model ARIMA(4,0,1)
coefficients(SearchARIMA) # A look at AR and MA coefficients
plot(SearchARIMA) # A look at the inverse roots

# A look at the residuals from the ARIMA(4,0,1)
par(mfrow=c(2,1))
acf_rugarch(SearchARIMA$residuals) # Few and low autokorrelations above confidence bounds
qqnorm(SearchARIMA$residuals) # Not approx. normally distributed.


#########
## ARCH test using ArchTest() function
# We are testing for the existance of ARCH effects in the data
ArchTest.price <- FinTS::ArchTest(df, lags = 100, demean = TRUE)
ArchTest.price # There is ARCH effects in the price series
# Rejected NULLHYP: ARCH effects exists
0
ArchTest.returns <- FinTS::ArchTest(df.returns, lags = 100, demean = TRUE)
ArchTest.returns
# Rejected NULLHYP: ARCH effects exists

# A noteworthy result: 
# 1) ADF-test shows return series stationary
# 2) ArchTest shows ARCH effects in series

#######

#######
### AR(1)-Garch(1,1) model

# Model for the price series
spec <- ugarchspec(variance.model = list(model = "csGARCH",
                   garchOrder = c(1,1)),
                   
                   mean.model = list(armaOrder = c(1,0),
                                     external.regressors = NULL),
                   distribution.model = "std")
# Using the component sGARCH model and assuming distribution model as t-distribution.

garch <- ugarchfit(spec = spec, data = df, solver = "lbfgs")
print(garch)

par(mfrow=c(3,1))
plot(garch) # Interesting plots are 1, 2 and 3
par(mfrow=c(2,2))
plot(garch) # Interesting plots are 8, 9 and 10.

# Few and farily low autocorrelations above confidence bounds.
# Residuals approx. t-distributed.

#######

### Model for the returns series
specreturns <- ugarchspec(variance.model = list(model = "csGARCH",
                                         garchOrder = c(1,1)),
                   
                   mean.model = list(armaOrder = c(1,0),
                                     external.regressors = NULL),
                   distribution.model = "std")
garch.returns <- ugarchfit(spec = specreturns, data = df.returns, solver = "hybrid")
garch.returns

par(mfrow=c(3,1)) # Interesting plots are 1, 2 and 3
plot(garch.returns)
par(mfrow=c(2,2)) # Interesting plots are 8, 9 and 10.
plot(garch.returns)

# Better autokorrelations and residuals and stronger indication that residuals are following t-distribution

#######

### Forecasting

## Rolling forecast
# 
modelroll <- ugarchroll(spec = spec, data = df, n.ahead = 1, forecast.length = 50 ,
                        n.start = NULL, refit.every = 5, refit.window = c("moving"),
                        window.size = NULL, solver = "hybrid", fit.control = list(),
                        solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05),
                        cluster = NULL, keep.coef = TRUE)
par(mfrow=c(1,1))
plot(modelroll) # Series forecast at plot 3


