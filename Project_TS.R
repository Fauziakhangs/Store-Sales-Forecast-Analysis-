setwd("~/TimeSeriesAnalysis/Project")

library(plotrix)
library(ggplot2)
library(dplyr)
library(tseries)
library(fBasics)
library(zoo)
library(ggplot2)
library(ggfortify)
library(dplyr)
source("backtest.R")
source("eacf.R")
library(urca)

mydata <- read.csv(file = "train.csv", sep= ",", header = T)
head(mydata)
library(tidyverse)
summary(mydata)

length(rownames(mydata))



mydata$date <- as.Date(mydata$date, format = "%Y-%m-%d")

product_data <- mydata %>%
  group_by(family) %>%
  summarize(Total_Sales = sum(sales)) %>%
  arrange(desc(Total_Sales))

top_3_products <- head(product_data, 3)$family

top_products_data <- mydata %>%
  filter(family %in% top_3_products)




library(dplyr)
library(lubridate)
library(forecast)

#grocery_sales <- complete_sales_data %>% filter(family == "GROCERY I")
grocery_data <- subset(mydata, family == "GROCERY I" & sales > 0)
head(grocery_data)
length(rownames(grocery_data))

qqnorm(grocery_data$sales)
qqline(grocery_data$sales, col = "blue")
#plot(grocery_data)
################################ Remove extreme outliers #########
grocery_data <- grocery_data %>%
  arrange(date) %>%
  mutate(sales = ifelse(sales > 50000, 
                        (lead(sales, 1, default = last(sales)) + lag(sales, 1, default = first(sales))) / 2,
                        sales))

ggplot(grocery_data, aes(x = date, y = sales)) +
  geom_point(color = 'blue', alpha = 0.5) +
  labs(title = "Scatter Plot of Sales Over Time",
       x = "Date",
       y = "Sales") +
  theme_minimal()


weekly_grocery_sales <- grocery_data %>%
  mutate(week = as.Date(cut(date, breaks = "week", start.on.monday = FALSE))) %>%
  group_by(week) %>%
  summarize(weekly_sales = sum(sales))

weekly_grocery_sales_zoo <- zoo(weekly_grocery_sales$weekly_sales, order.by = weekly_grocery_sales$week)

lag.plot(weekly_grocery_sales_zoo)

start_year <- as.numeric(format(start(weekly_grocery_sales_zoo), "%Y"))
start_week <- as.numeric(format(start(weekly_grocery_sales_zoo), "%U")) + 1
end_year <- as.numeric(format(end(weekly_grocery_sales_zoo), "%Y"))
end_week <- as.numeric(format(end(weekly_grocery_sales_zoo), "%U")) + 1

weekly_grocery_sales_ts <- ts(weekly_grocery_sales$weekly_sales, 
                              start = c(start_year, start_week), 
                              end = c(end_year, end_week), 
                              frequency = 52)

start_window <- c(2013, 3)
end_window <- c(2017, 34)

window_ts <- window(weekly_grocery_sales_ts, start = start_window, end = end_window)

plot(log(window_ts), type = "l", col = "blue", lwd = 2, 
     main = "Weekly Sales of GROCERY I",
     xlab = "Year", ylab = "Weekly Sales")



############REPLACE OUTLIERS##################
# Identify outliers using IQR method

# Assuming grocery_data is already loaded as you have shown
Q1 <- quantile(grocery_data$sales, 0.25)
Q3 <- quantile(grocery_data$sales, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Replace outliers with the average of surrounding values
grocery_data <- grocery_data %>%
  arrange(date) %>%
  mutate(sales = ifelse(sales < lower_bound | sales > upper_bound, 
                        lead(sales, 1) + lag(sales, 1, default = first(sales)) / 2,
                        sales))

#grocery_data <- grocery_data %>%
#  arrange(date) %>%
#  mutate(sales = ifelse(sales < lower_bound | sales > upper_bound | sales > 4000, 
#                        (lead(sales, 1) + lag(sales, 1, default = first(sales))) / 2,
#                        sales))

# Create a scatter plot to visualize sales
ggplot(grocery_data, aes(x = date, y = sales)) +
  geom_point(color = 'blue', alpha = 0.5) +
  labs(title = "Scatter Plot of Sales Over Time",
       x = "Date",
       y = "Sales") +
  theme_minimal()

View(grocery_data)


weekly_grocery_sales <- grocery_data %>%
  mutate(week = as.Date(cut(date, breaks = "week", start.on.monday = FALSE))) %>%
  group_by(week) %>%
  summarize(weekly_sales = sum(sales))

# Convert to zoo object
weekly_grocery_sales_zoo <- zoo(weekly_grocery_sales$weekly_sales, order.by = weekly_grocery_sales$week)

# Create a lag plot
lag.plot(weekly_grocery_sales_zoo)

# Define start and end for the time series
start_year <- as.numeric(format(start(weekly_grocery_sales_zoo), "%Y"))
start_week <- as.numeric(format(start(weekly_grocery_sales_zoo), "%U")) + 1
end_year <- as.numeric(format(end(weekly_grocery_sales_zoo), "%Y"))
end_week <- as.numeric(format(end(weekly_grocery_sales_zoo), "%U")) + 1

# Convert to time series
weekly_grocery_sales_ts <- ts(weekly_grocery_sales$weekly_sales, 
                              start = c(start_year, start_week), 
                              end = c(end_year, end_week), 
                              frequency = 52)

# Define the start and end dates for the window
start_window <- c(2013, 3)
end_window <- c(2017, 34)

# Create the window
window_ts <- window(weekly_grocery_sales_ts, start = start_window, end = end_window)

# Plot the windowed time series
plot(window_ts, type = "l", col = "blue", lwd = 2, 
     main = "Weekly Sales of GROCERY I",
     xlab = "Year", ylab = "Weekly Sales")
frequency(window_ts)

autoplot(mstl(window_ts))

###################### Analysis ######################

Acf(window_ts, lag.max = 20) # non-stationary series
pacf(window_ts, lag.max = 20)#might be AR(2)
eacf(window_ts) # non-stationary series top row X's
Box.test(window_ts, lag =  17, type = "L") # white noise

adf.test(window_ts)
kpss.test(window_ts)

###########  MOdel 1 ###############
fit1 <- Arima(window_ts, order = c(0,1,1), seasonal = list(order = c(0, 1, 1), period =2))
fit1
library(lmtest)


coeftest(fit1) # all are significant

fit3 <- auto.arima(window_ts)
fit3 
coeftest(fit3)
Acf(fit3$residuals)
pacf(fit3$residuals) # white noise
Box.test(fit3$residuals, type = "L")
pq1 <- forecast(fit3, h = 10)
plot(pq1)

qqnorm(fit3$residuals)
qqline(fit3$residuals, col = "red")


Acf(fit1$residuals)
pacf(fit1$residuals) # white noise
Box.test(fit1$residuals, type = "L")
pq1 <- forecast(fit1, h = 10)
plot(pq1)

qqnorm(fit1$residuals)
qqline(fit1$residuals, col = "red")

########################## MOdel 2 #############
Acf(window_ts) # non-stationary series
pacf(window_ts)#might be AR(2)
eacf(window_ts)
fit1 <- Arima(window_ts, order = c(1,1,3), seasonal = list(order = c(0, 1, 1), period =4))
fit1

coeftest(fit1) 


Acf(fit1$residuals)
pacf(fit1$residuals) # white noise
Box.test(fit1$residuals, type = "L")
pq1 <- forecast(fit1, h = 10)
plot(pq1)

################# MOdel 3  #################
Acf(window_ts) # non-stationary series
pacf(window_ts)#might be AR(2)
eacf(window_ts)
fit1 <- Arima(window_ts, order = c(3,1,3), seasonal = list(order = c(0, 1, 1), period =2))
fit1

coeftest(fit1) 


Acf(fit1$residuals)
pacf(fit1$residuals) # white noise
Box.test(fit1$residuals, type = "L")
qqnorm(fit1$residuals)
qqline(fit1$residuals, col = "red")
pq1 <- forecast(fit1, h = 10)
plot(pq1)

nTest <-  0.90 * length(window_ts)  # 90% of the data

pm1 <- backtest(fit1, window_ts, orig = nTest, h =1)
pm1


################# MOdel 4  #################
Acf(window_ts) # non-stationary series
pacf(window_ts)#might be AR(2)
eacf(window_ts)
fit3 <-auto.arima(window_ts)
fit3
coeftest(fit3)
fit1 <- Arima(window_ts, order = c(2,0,3), seasonal = list(order = c(0, 0, 1), period =4))
fit1

coeftest(fit1) 


Acf(fit1$residuals)
pacf(fit1$residuals) # white noise
Box.test(fit1$residuals, type = "L")
pq1 <- forecast(fit1, h = 10)
plot(pq1)


################# MOdel 6 ################################

fit1 <- Arima(window_ts, order = c(3,1,3), seasonal = list(order = c(0, 0, 1), period =52))
fit1

coeftest(fit1) 


Acf(fit1$residuals)
pacf(fit1$residuals) # white noise
Box.test(fit1$residuals, type = "L")
pq1 <- forecast(fit1, h = 10)
plot(pq1)


################# MOdel 7 : with seasonaliy and drift #################

fit1 <- Arima(window_ts, order = c(1,0,2), seasonal = list(order = c(0, 0, 1), period =2), include.drift = T)
fit1

coeftest(fit1)
Acf(fit1$residuals)
pacf(fit1$residuals)

range(window_ts)
#air <- ts(fit1$residuals, start=  c(2013, 3), frequency = 52)
#autoplot(air)

# look for residual spectrum
spectrum(air, log="no", spans=c(2, 2), plot=T, xlab="Frequency (Cycles/Year)")





############### Harmonic Regression ################

library(forecast)

# Define Fourier terms
fourier_terms <- fourier(window_ts, K = 5)

fit0 <- tslm(window_ts ~ trend + fourier_terms)
summary(fit0)

# Check residuals
#checkresiduals(fit0)
Acf(fit0$residuals, lag.max=24)
pacf(fit0$residuals, lag.max=24)

future_fourier <- fourier(window_ts, K = 5, h = 52) # Forecast for the next 52 periods
forecasted_values <- forecast(fit0, newdata = data.frame(fourier_terms = future_fourier))
plot(forecasted_values, main = "Forecasted Weekly Sales with Fourier Terms")



f = fourier(window_ts, K=2)
autoplot(f)
head(f)

plot(f[, 1], type="l", main="Plot of the first sine term")
plot(f[, 2], type="l", main="Plot of the first cosine term")
plot(f[, 3], type="l", main="Plot of the second sine term")
plot(f[, 4], type="l", main="Plot of the second cosine term")



fit1 <- Arima(window_ts, order = c(3, 1, 3), 
              seasonal = list(order = c(0, 0, 1), period = 52), 
              xreg = fourier_terms)

# Display the summary of the fitted model
summary(fit1)
coeftest(fit1)
Acf(fit1$residuals)
pacf(fit1$residuals)

h <- 52  
fourier_terms_forecast <- fourier(window_ts, K = 5, h = h)
forecast_values <- forecast(fit1, xreg = fourier_terms_forecast)

autoplot(forecast_values)







################ Harmonic Regression k =2 ######################


library(forecast)

# Define Fourier terms
fourier_terms <- fourier(window_ts, K = 2)

# Fit the model using tslm(time series liner model function)
# trend will automatically include the time trend here
fit0 <- tslm(window_ts ~ trend + fourier_terms)
summary(fit0)

# Check residuals
#checkresiduals(fit0)
Acf(fit0$residuals, lag.max=24)
pacf(fit0$residuals, lag.max=24)


# Forecast future values using the fittedmodel
future_fourier <- fourier(window_ts, K = 2, h = 52) # Forecast for the next 52 periods
forecasted_values <- forecast(fit0, newdata = data.frame(fourier_terms = future_fourier))
plot(forecasted_values, main = "Forecasted Weekly Sales with Fourier Terms")



f = fourier(window_ts, K=2)
autoplot(f)
head(f)

plot(f[, 1], type="l", main="Plot of the first sine term")
plot(f[, 2], type="l", main="Plot of the first cosine term")
plot(f[, 3], type="l", main="Plot of the second sine term")
plot(f[, 4], type="l", main="Plot of the second cosine term")

# include Fit the ARIMA Model with Fourier Terms


fit1 <- Arima(window_ts, order = c(3, 1, 3), 
              seasonal = list(order = c(0, 0, 1), period = 52), 
              xreg = fourier_terms)

# Display the summary of the fitted model
summary(fit1)
coeftest(fit1)
Acf(fit1$residuals)
pacf(fit1$residuals)

h <- 52  # Forecast horizon (e.g., 52 weeks)
fourier_terms_forecast <- fourier(window_ts, K = 2, h = h)
forecast_values <- forecast(fit1, xreg = fourier_terms_forecast)

# Plot the forecast
autoplot(forecast_values)



##################################################################
spectrum(test, log="no", spans=c(2, 2), plot=T, xlab="Frequency (Cycles/Year)")

# multi linear  seriws 12 52
test <- msts(window_ts, seasonal.periods = c(2*6, 2*26), start = start_window, end = end_window)
head(test)
plot(test, type = "l", col = "blue", lwd = 2, 
     main = "Weekly Sales of GROCERY I",
     xlab = "Year", ylab = "Weekly Sales")
#autoplot(test)
Acf(test, lag.max = 20)
pacf(test, lag.max = 20)
pacf(diff(test))
eacf(test)
Box.test(test, lag = 3, type = "L")


adf.test(test)
kpss.test(test)


library(forecast)

# Define Fourier terms
fourier_terms <- fourier(test, K = c(5,5))

fit0 <- tslm(test ~ trend + fourier_terms)
summary(fit0)

# Check residuals
#checkresiduals(fit0)
Acf(fit0$residuals, lag.max=24)
pacf(fit0$residuals, lag.max=24)
Box.test(fit0$residuals, type = "L")

future_fourier <- fourier(test, K = c(5,5), h = 52) # Forecast for the next 52 periods
forecasted_values <- forecast(fit0, newdata = data.frame(fourier_terms = future_fourier))
plot(forecasted_values, main = "Forecasted Weekly Sales with Fourier Terms")



f = fourier(test, K=2)
autoplot(f)
head(f)

plot(f[, 1], type="l", main="Plot of the first sine term")
plot(f[, 2], type="l", main="Plot of the first cosine term")
plot(f[, 3], type="l", main="Plot of the second sine term")
plot(f[, 4], type="l", main="Plot of the second cosine term")


fit1 <- Arima(test, order = c(1, 0, 4),  
              seasonal = list(order = c(0, 0, 1), period = 52),
              xreg = fourier_terms)

# Display the summary of the fitted model
summary(fit1)
coeftest(fit1)
Acf(fit1$residuals, lag.max = 20)
pacf(fit1$residuals, lag.max = 20)
Box.test(fit1$residuals, type = "L")

h <- 52  
fourier_terms_forecast <- fourier(test, K = c(5,5), h = h)
forecast_values <- forecast(fit1, xreg = fourier_terms_forecast)

plot(forecast_values)






####################Identify outliers:
library(forecast)
library(ggplot2)
library(dplyr)

gasoline_msts <-msts(window_ts, seasonal.periods = c(2*6, 2*26), start = start_window, end = end_window)
head(gasoline_msts)

autoplot(gasoline_msts) +
  ggtitle("Original Gasoline Sales Data") +
  xlab("Year") +
  ylab("Sales")

outliers <- tsoutliers(gasoline_msts)
cleaned_sales <- tsclean(gasoline_msts, )

autoplot(log(cleaned_sales)) +
  ggtitle("Cleaned Gasoline Sales Data") +
  xlab("Year") +
  ylab("Sales")






######################## 
gasoline_df <- data.frame(date = time(gasoline_msts), sales = as.numeric(gasoline_msts))

# Define a threshold for identifying outliers
threshold <- 2500000

# Replace outliers with the average of the previous and next values
gasoline_df$sales <- ifelse(gasoline_df$sales > threshold,
                            (dplyr::lag(gasoline_df$sales) + dplyr::lead(gasoline_df$sales)) / 2,
                            gasoline_df$sales)

gasoline_df <- gasoline_df %>%
  mutate(sales = ifelse(row_number() == 1 | row_number() == n(),
                        sales,
                        ifelse(sales > threshold,
                               (dplyr::lag(sales, default = first(sales)) + dplyr::lead(sales, default = last(sales))) / 2,
                               sales)))
# Create a cleaned msts object
cleaned_sales <- msts(gasoline_df$sales,seasonal.periods = c(2*6, 2*26), start = start_window, end = end_window)

head(cleaned_sales)
# Plot the cleaned time series
autoplot(cleaned_sales) +
  ggtitle("Cleaned Gasoline Sales Data") +
  xlab("Year") +
  ylab("Sales")










#############################


plot(cleaned_sales, type = "l", col = "blue", lwd = 2, 
     main = "Weekly Sales of GROCERY I",
     xlab = "Year", ylab = "Weekly Sales")
#autoplot(test)
Acf(cleaned_sales, lag.max = 20)
pacf(cleaned_sales, lag.max = 20)
pacf(diff(cleaned_sales))
eacf(cleaned_sales)
Box.test(cleaned_sales, lag = 3, type = "L")


adf.test(cleaned_sales)
kpss.test(cleaned_sales)


library(forecast)

spectrum(cleaned_sales, log="no", spans=c(2,2), plot=T, xlab="Frequency (Cycles/Year)")


# Define Fourier term4
fourier_terms <- fourier(cleaned_sales, K = c(5,5))

fit0 <- tslm(cleaned_sales ~ trend + fourier_terms)
summary(fit0)

# Check residuals
#checkresiduals(fit0)
Acf(fit0$residuals, lag.max=20)
pacf(fit0$residuals, lag.max=20)
Box.test(fit0$residuals, type = "L")

future_fourier <- fourier(cleaned_sales, K = c(5,5), h = 52) # Forecast for the next 52 periods
forecasted_values <- forecast(fit0, newdata = data.frame(fourier_terms = future_fourier))
plot(forecasted_values, main = "Forecasted Weekly Sales with Fourier Terms")



f = fourier(test, K=2)
autoplot(f)
head(f)

plot(f[, 1], type="l", main="Plot of the first sine term")
plot(f[, 2], type="l", main="Plot of the first cosine term")
plot(f[, 3], type="l", main="Plot of the second sine term")
plot(f[, 4], type="l", main="Plot of the second cosine term")

library(lmtest)
fit1 <- Arima(cleaned_sales, order = c(1, 0, 4),  
              seasonal = list(order = c(0, 0, 1), period = 52),
              xreg = fourier_terms)

# Display the summary of the fitted model
summary(fit1)
coeftest(fit1)
Acf(fit1$residuals, lag.max = 20)
pacf(fit1$residuals, lag.max = 20)
Box.test(fit1$residuals, type = "L")

h <- 52  
fourier_terms_forecast <- fourier(cleaned_sales, K = c(5,5), h = h)
forecast_values <- forecast(fit1, xreg = fourier_terms_forecast)

plot(forecast_values)




#################### HR with ARIMA MODEL 2 #######
fit2 <- Arima(cleaned_sales, order = c(2, 0, 5),  
              seasonal = list(order = c(0, 0, 1), period = 52),
              xreg = fourier_terms)

# Display the summary of the fitted model
summary(fit2)
coeftest(fit2)
Acf(fit2$residuals, lag.max = 20)
pacf(fit2$residuals, lag.max = 20)
Box.test(fit2$residuals, type = "L")

h <- 52  
fourier_terms_forecast <- fourier(cleaned_sales, K = c(5,5), h = h)
forecast_values <- forecast(fit1, xreg = fourier_terms_forecast)

plot(forecast_values)



#################### HR with ARIMA MODEL 3 #######

fit2 <- Arima(cleaned_sales, order = c(4, 0, 4),  
              seasonal = list(order = c(1, 0, 0), period = 52),
              xreg = fourier_terms)

# Display the summary of the fitted model
summary(fit2)
coeftest(fit2)
Acf(fit2$residuals, lag.max = 20)
pacf(fit2$residuals, lag.max = 20)
Box.test(fit2$residuals, type = "L")

h <- 52  
fourier_terms_forecast <- fourier(cleaned_sales, K = c(5,5), h = h)
forecast_values <- forecast(fit1, xreg = fourier_terms_forecast)

plot(forecast_values)




#################### HR with ARIMA MODEL 4 #######



fit2 <- Arima(cleaned_sales, order = c(1, 0, 5),  
              seasonal = list(order = c(0, 0, 1), period = 52),
              xreg = fourier_terms)

# Display the summary of the fitted model
summary(fit2)
coeftest(fit2)
Acf(fit2$residuals, lag.max = 20)
pacf(fit2$residuals, lag.max = 20)
Box.test(fit2$residuals, type = "L")

h <- 52  
fourier_terms_forecast <- fourier(cleaned_sales, K = c(5,5), h = h)
forecast_values <- forecast(fit1, xreg = fourier_terms_forecast)

plot(forecast_values)



