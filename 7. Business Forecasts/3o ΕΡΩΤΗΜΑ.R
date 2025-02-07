dat <- read.csv("C:\\ntua\\4ο εξάμηνο\\Προβλέψεις\\EP\\ft_project_insample_24.csv", header = TRUE)

third_row <- dat[dat$id == 3, ]

values <- as.numeric(third_row[6:ncol(dat)])

values <- values[1:(which(is.na(values))[1] - 1)]
# Extract metadata
start_year <- third_row$start_year
start_period <- third_row$start_period
frequency <- third_row$frequency
description <- third_row$description

#A

# Generate time axis as Date objects
start_date <- as.Date(paste0(start_year, "-", start_period, "-01"))
time_axis <- seq.Date(from = start_date, by = paste(12/frequency, "months"), length.out = length(values))

# Plot the data
plot(
  time_axis, values, type = "l", 
  xlab = " ", ylab = description,
  main = paste(description, " Quarterly"),
  xaxt = "n" # Suppress default x-axis
)

# Add custom x-axis labels with months and years
axis(1, at = time_axis, labels = format(time_axis, "%b %Y"), las = 2, cex.axis = 0.7)

ts_data <- ts(values, start = c(start_year, start_period), frequency = frequency)

# Decompose the time series
fit <- decompose(ts_data, type="additive")

# Plot the decomposition
plot(fit)

#B
growth_rate <- ((sum(ts_data[25:28])/4-sum(ts_data[1:24])/24)/(sum(ts_data[1:24])/24))*100
print(paste(round(growth_rate, 3), "%"))

#C

y <-ts_data
#Create train and test set
insample <-window(y, start=1980, end= c(1984,10))
outsample <-window(y, start=1985)

seasonal_indices<-decompose(y, type="multiplicative")$seasonal
ydes<-y/seasonal_indices
x <-c(1:length(y))
trainset <-data.frame(ydes,x)
plot(x, ydes)
fit <-lm(ydes~x, data=trainset)
abline(fit, col="red")

lm(formula = ydes~ x, data = trainset)

summary(fit)

plot(residuals(fit))
abline(h = 0, col = "red")

library(forecast)
fit_arima <- auto.arima(ydes)
forecast_arima <- forecast(fit_arima, h = 24)  # 2 years ahead
plot(forecast_arima)

plot(residuals(fit_arima))
abline(h = 0, col = "red")

forecast_periods <- 24  # forecast for 24 quarters (6 years)
forecast_time_axis <- seq.Date(from = as.Date("1987-01-01"), by = paste(12/frequency, "months"), length.out = forecast_periods)

forecast_arima <- forecast(fit_arima, h = forecast_periods)
predictions_arima <- forecast_arima$mean * seasonal_indices[1:forecast_periods]

x_out_linear <- seq(length(y) + 1, length(y) + forecast_periods)
testset_linear <- data.frame(x = x_out_linear)
predictions_linear <- predict(fit, newdata = testset_linear) * seasonal_indices[1:forecast_periods]

plot(time_axis, values, type = "l", xlab = "Date", ylab = description, main = "Quarterly Forecast", xlim = c(min(time_axis), max(forecast_time_axis)))
lines(forecast_time_axis, predictions_arima, col = "red", lty = 2)
lines(forecast_time_axis, predictions_linear, col = "green", lty = 2)

# Add a legend
legend("topright", legend = c("ARIMA", "Linear"), col = c("red",  "green"), lty = 2)

#D

linear_predictions_with_ci <- predict(fit, newdata = testset_linear, interval = "confidence", level = 0.80)

# Extract the lower and upper bounds of the confidence intervals
lower_bound_linear <- linear_predictions_with_ci[, 2]*seasonal_indices[1:forecast_periods]
upper_bound_linear <- linear_predictions_with_ci[, 3]*seasonal_indices[1:forecast_periods]

forecast_arima_with_ci <- forecast(fit_arima, h = 24, level = 90)

# Extract the lower and upper bounds of the confidence intervals
lower_bound_arima <- forecast_arima_with_ci$lower[, 1]*seasonal_indices[1:forecast_periods]
upper_bound_arima <- forecast_arima_with_ci$upper[, 1]*seasonal_indices[1:forecast_periods]



# Plot the original data and linear regression predictions
plot(time_axis, values, type = "l", xlab = "Date", ylab = description, 
     main = "Linear Regression with 80% Confidence Interval", 
     xlim = c(min(time_axis), max(forecast_time_axis)), 
     ylim = c(3000, 5000))

# Plot the original and predicted values with the confidence intervals
lines(forecast_time_axis, predictions_linear, col = "green", lty = 1)
lines(forecast_time_axis, lower_bound_linear, col = "blue", lty = 2)
lines(forecast_time_axis, upper_bound_linear, col = "blue", lty = 2)
lines(forecast_time_axis, predictions_arima, col = "red", lty = 1) 
lines(forecast_time_axis, lower_bound_arima, col = "purple", lty = 2)
lines(forecast_time_axis, upper_bound_arima, col = "purple", lty = 2) 

legend("bottomleft", legend = c("Linear prediction with seasonality", "Upper bound/Lower bound"), col = c("green",  "blue"), lty = c(1,2))

#E

market_share_loss_factor <- 0.9  # Reducing by 10%

# Adjust all forecast values (ARIMA, Polynomial, Linear)
adjusted_predictions_arima <- predictions_arima * market_share_loss_factor
adjusted_predictions_linear <- predictions_linear * market_share_loss_factor

# Step 3: Plot the original data and the adjusted forecasts
plot(time_axis, values, type = "l", xlab = "Date", ylab = description, 
     main = "Adjusted Quarterly Forecast", 
     xlim = c(min(time_axis), max(forecast_time_axis)), 
     ylim = c(3000, 5000), xaxt = "n")

# Plot the original forecasts (ARIMA and Linear)
lines(forecast_time_axis, predictions_arima, col = "red", lty = 1)  # Original ARIMA (solid red line)
lines(forecast_time_axis, predictions_linear, col = "green", lty = 1)  # Original Linear (solid green line)

# Plot the adjusted forecasts (ARIMA and Linear with 10% market share reduction)
lines(forecast_time_axis, adjusted_predictions_arima, col = "red", lty = 2)  # Adjusted ARIMA (dashed red line)
lines(forecast_time_axis, adjusted_predictions_linear, col = "green", lty = 2)  # Adjusted Linear (dashed green line)

# Add a legend to differentiate between original and adjusted forecasts
legend("bottomleft", legend = c("Original ARIMA", "Original Linear", "Adjusted ARIMA", "Adjusted Linear"), 
       col = c("red", "green", "red", "green"), lty = c(1, 1, 2, 2))

axis(1, at = c(time_axis, forecast_time_axis), labels = format(c(time_axis, forecast_time_axis), "%b %Y"), las = 2, cex.axis = 0.7)

print(data.frame(forecast_time_axis, predictions_linear, upper_bound_linear, lower_bound_linear))
