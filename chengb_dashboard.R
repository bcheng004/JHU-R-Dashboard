remove(list=ls())
cat("\014")
setwd("C:\\Users\\bchen\\Desktop\\JHU-R-Dashboard\\")
# remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)

library(dplyr)

coronavirus<-download_jhu_csse_covid19_data(cached = TRUE)
coronavirus<-as.data.frame(coronavirus)
head(coronavirus, 1)

coronavirus_us <- coronavirus %>% filter(country == "US") %>% select(c("date", "confirmed"))

head(coronavirus_us,5)
class(coronavirus_us)
class(coronavirus$date)

library(tidyverse)
library(fpp2)
library(zoo)
library(xts)
library(fabletools)

daily_confirmed_us<-xts(coronavirus_us[,-1], order.by = coronavirus_us[,1])
# names(daily_confirmed_us) <- "confirmed_cases"
head(daily_confirmed_us,5)
autoplot(daily_confirmed_us)
str(daily_confirmed_us)
# index(daily_confirmed_us)

# us_confirmed_ses<-ses(daily_confirmed_us, alpha = 0.99, h = 7)
# us_confirmed_ses$model$par

us_confirmed_arima<-auto.arima(daily_confirmed_us)
us_confirmed_arima$fitted
summary(us_confirmed_arima)

# autoplot(us_confirmed_ses) + autolayer(us_confirmed_ses$fitted, series="SES Model") +
#   ylab("Confirmed Cases") + xlab("Days Since 2020-01-22")
arima_forecast<-forecast(us_confirmed_arima,7)
require(scales)
autoplot(arima_forecast, main="Confirmed Cases Forecast") + autolayer(us_confirmed_arima$fitted, series="ARIMA Model") +
    ylab("Confirmed Cases") + xlab("Days Since 2020-01-22") + scale_y_continuous(labels = comma)

map_covid19(coronavirus, type = "confirmed", region = "North America")