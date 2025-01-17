# Load Library 
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(pdfetch)
AAPL <- pdfetch_YAHOO ('AAPL')
summary <- as.data.frame(summary(AAPL))
head(summary)
tail(summary)
chartSeries(AAPL, subset = 'last 10 month', type = 'auto',theme=chartTheme('white'))
candleChart(AAPL$AAPL.adjclose, TA=c(addMACD(),
                                     addVo()), subset = '2022', theme=chartTheme('white'))#  Import Last 11 month data
# Again import data from yahoo as before and create new variable.
# But here just import last 11month of data 
AAPL_last_11_month  <-pdfetch_YAHOO('AAPL', from = '2022-01-01', to = '2022-11-01')
# Now select only Adjusted Closed Price from 
data <- AAPL_last_11_month$AAPL.adjclose
library(quantmod)
Daily_return <- Delt(data)
hist(Daily_return)