# Bitcoin AR(1)-csGARCH(1,1)
Project in Statistics: Timeseries analysis (STAH14) at Lund University. The project is about the price and returns of Bitcoin, modelled using an AR(1)-GARCH(1,1).

Data is collected from https://www.coindesk.com/price/bitcoin

The project is about modelling the price (closing) and returns of Bitcoin over the available timeperiod.

The most suitable ARIMA model is found by an algorithm based on Akaike information criterion and residuals are compared with a AR(1)-csGARCH(1,1) model.

Data for closing price and returns are tested for stationarity and presence of ARCH-effects.

AR-GARCH model for returns show the best fit while closing price model is acceptable. 

Finally a rolling-forecast is used to predict the last 50 days of the series. Forecasting one day ahead and refitting model every five days.
