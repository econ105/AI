install.packages("tidyverse")
install.packages("forecast")
install.packages("prophet")
library(tidyverse)
library(forecast)
library(prophet)
install.packages("quantmod")
library(quantmod)
getSymbols("AAPL", src = "yahoo", from = "2018-01-01", to = "2023-01-01")
aapl_data <- Cl(AAPL)  # Get closing prices
# Plotting the time series
autoplot(aapl_data, main = "Apple Stock Price")
# Fitting the ARIMA model
fit_arima <- auto.arima(aapl_data)
summary(fit_arima)
# Forecasting for the next 30 days
forecast_arima <- forecast(fit_arima, h = 30)
# Plotting the forecast
autoplot(forecast_arima, main = "ARIMA Forecast for Apple Stock Price")


# Fitting the Exponential Smoothing model
fit_ets <- ets(aapl_data)
summary(fit_ets)


# Preparing data for Prophet
aapl_df <- data.frame(ds = index(aapl_data), y = as.numeric(aapl_data))
# Fitting the Prophet model
model_prophet <- prophet(aapl_df)
# Making future dataframe for predictions
future <- make_future_dataframe(model_prophet, periods = 30)
forecast_prophet <- predict(model_prophet, future)
# Plotting the forecast
plot(model_prophet, forecast_prophet) + ggtitle("Prophet Forecast for Apple Stock Price")


accuracy(forecast_arima, aapl_data)
accuracy(forecast_ets, aapl_data)