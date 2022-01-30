#------------------------------------------------------------#
# R in Action (3rd ed): Chapter 15                           #
# Time series                                                #
# requires forecast, tseries, xts, patchwork, ggplot2,       #
#   and drectlabels packages                                 #
# install.packages(c("forecast", "tseries", "xts", ggplot2", #
#                    "patchwork", "directlabels"))           #
#------------------------------------------------------------#


# Listing 15.1 - Creating a time series object in R
library(xts)
sales = c(18, 33, 41,  7, 34, 35, 24, 25, 24, 21, 25, 20, 
          22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
date = seq(as.Date("2018/1/1"), as.Date("2019/12/1"), by="month")

sales.xts <- xts(sales, date)

# Listing 15.2 - Plotting time-series
library(ggplot2)
library(forecast)
autoplot(sales.xts)

autoplot(sales.xts) +  
  scale_x_date(date_breaks="1 months", 
               date_labels="%b %y") +
   geom_line(color="blue") +
  labs(x="", 
       y="Sales",
       title="Monthly sales data") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1),
        panel.grid.minor.x=element_blank())



# Listing 15.3 - Simple moving averages
library(forecast)
library(ggplot2)
theme_set(theme_bw())
ylim <- c(min(Nile), max(Nile))
autoplot(Nile, main="Raw time series") +
  scale_y_continuous(limits=ylim)
autoplot(ma(Nile, 3), main="Simple Moving Averages (k=3)") +
  scale_y_continuous(limits=ylim)
autoplot(ma(Nile, 7), main="Simple Moving Averages (k=7)") +
  scale_y_continuous(limits=ylim)
autoplot(ma(Nile, 15), main="Simple Moving Averages (k=15)") +
  scale_y_continuous(limits=ylim)

# Listing 15.4 Seasonal decomposition using stl()
library(forecast)
library(ggplot2)
autoplot(AirPassengers)                                   
lAirPassengers <- log(AirPassengers)
autoplot(lAirPassengers, ylab="log(AirPassengers)")

fit <- stl(lAirPassengers, s.window="period")             
autoplot(fit)
fit$time.series                                            
exp(fit$time.series)

# Listing 15.5 Month and season plots
library(forecast)
library(ggplot2)
library(directlabels)

ggmonthplot(AirPassengers)  +                   
  labs(title="Month plot: AirPassengers", 
       x="", 
       y="Passengers (thousands)")

p <- ggseasonplot(AirPassengers) + geom_point() +  
  labs(title="Seasonal plot: AirPassengers",
       x="",
       y="Passengers (thousands)")
direct.label(p)


# Listing 15.6 - Simple exponential smoothing
library(forecast) 
fit <- ets(nhtemp, model="ANN")  
summary(fit)
fit

options(digits=5)
forecast(fit, 1)  

autoplot(forecast(fit, 1)) +
  labs(x="Year", y=expression(paste("Temperature (", degree*F,")",)),
       title="New Haven Annual Mean Temperature")

accuracy(fit)                                            


# Listing 15.7 - Exponential smoothing with level, slope, 
# and seasonal components
library(forecast)
fit <- ets(log(AirPassengers), model="AAA")       
fit

accuracy(fit)

pred <- forecast(fit, 5)                                  
pred

autoplot(pred) +
  labs(title = "Forecast for Air Travel",
       y = "Log(AirPassengers)", 
       x ="Time")
     
pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
p


# Listing 15.8 - Automatic exponential forecasting with ets()
library(forecast)
fit <- ets(JohnsonJohnson)
fit
autoplot(forecast(fit)) +
  labs(x = "Time",
       y = "Quarterly Earnings (Dollars)",
       title="Johnson and Johnson Forecasts")


# Listing 15.9 - Transforming the time series and assessing stationarity
library(forecast)
library(tseries)
autoplot(Nile) + labs(x="")
ndiffs(Nile)
dNile <- diff(Nile)                                             
autoplot(dNile) + labs(y="diff(Nile)")  
adf.test(dNile)

# identifying one or more reasonable models
autoplot(Acf(dNile))
autoplot(Pacf(dNile))

# Listing 15.10 - Fit an ARIMA model
fit <- Arima(Nile, order=c(0,1,1))                                 
fit
accuracy(fit)


# Listing 15.11 - Evaluating the model fit
df <- data.frame(resid = as.numeric(fit$residuals))
ggplot(df, aes(sample = resid)) +
  stat_qq() + stat_qq_line() +
  labs(title="Normal Q-Q Plot")

Box.test(fit$residuals, type="Ljung-Box")


# Listing 15.12 - Forecasting with an ARIMA model
forecast(fit, 3)
autoplot(forecast(fit, 3)) +
  labs(x="Year", y="Annual Flow")


# Listing 15.13 - Automated ARIMA forecasting
library(forecast)
fit <- auto.arima(sunspots)
fit
forecast(fit, 3)
accuracy(fit)


