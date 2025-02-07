dat <- read.csv("C:\\ntua\\4ο εξάμηνο\\Προβλέψεις\\EP\\ft_project_insample_24.csv", header = TRUE)

first_row <- dat[dat$id == 1, ]

values <- as.numeric(first_row[6:ncol(dat)])

# Extract metadata
start_year <- first_row$start_year
start_period <- first_row$start_period
frequency <- first_row$frequency
description <- first_row$description

#A

# Generate time axis as Date objects
start_date <- as.Date(paste0(start_year, "-", start_period, "-01"))
time_axis <- seq.Date(from = start_date, by = paste(12/frequency, "months"), length.out = length(values))

# Plot the data
plot(
  time_axis, values, type = "l", 
  xlab = "Time", ylab = "Shipments Count",
  main = paste("Shipments Count Monthly"),
  xaxt = "n" # Suppress default x-axis
)

# Add custom x-axis labels with months and years
axis(1, at = time_axis, labels = format(time_axis, "%b %Y"), las = 2, cex.axis = 0.7)

#B

ts_data <- ts(values, start = c(start_year, start_period), frequency = frequency)

# Decompose the time series
fit <- decompose(ts_data, type="multiplicative")

# Plot the decomposition
plot(fit)

seasonal_indices<-fit$seasonal
deseasonalized_ts<-ts_data/seasonal_indices
plot(ts_data)
lines(deseasonalized_ts, col="red")
legend("topleft", legend=c("Data","Seasonally adjusted data"), col=c("black","red"), lty=1)

print(fit$seasonal)

plot(seasonal_indices)

#C

library(forecast)

# Fit Holt's Linear Trend model to the deseasonalized data
holt_model <- holt(deseasonalized_ts, h=12)

# Forecast for the next 12 months
holt_forecast <- holt_model$mean

# Reseasonalize the forecast
next_seasonal_indices <- fit$seasonal[5:16]  # Seasonal indices for the next 12 months
reseasonalized_forecast <- holt_forecast * next_seasonal_indices

# Combine time points for plotting
last_observed_date <- max(time_axis)
forecast_dates <- seq.Date(from = last_observed_date + 31, by = "month", length.out = 12)

# Plot observed, deseasonalized, and reseasonalized forecast
plot(
  c(time_axis, forecast_dates), 
  c(ts_data, rep(NA, length(forecast_dates))), 
  type = "l", col = "black", lwd = 2,
  xlab = "Time", ylab = "Shipments Count",
  main = "Holt's Linear Trend with Reseasonalized Forecast",
  xlim =  range(c(time_axis, forecast_dates)), 
  ylim = c(min(ts_data, reseasonalized_forecast), max(ts_data, reseasonalized_forecast)),
  xaxt = "n" # Suppress default x-axis
)

lines(time_axis, deseasonalized_ts, col = "red", lty = 2, lwd = 2)
lines(forecast_dates, reseasonalized_forecast, col = "blue", lty = 2, lwd = 2)

legend(
  "topleft", legend = c("Observed Data", "Deseasonalized Data", "Forecast"),
  col = c("black", "red", "blue"), lty = c(1, 2, 2), lwd = 2
)
par(xpd = TRUE)
axis(1, 
     at = c(time_axis, forecast_dates), 
     labels = format(c(time_axis, forecast_dates), "%b %Y"), 
     las = 2, 
     cex.axis = 0.7)
# Print the forecast values
print(data.frame(Forecast_Date = forecast_dates, Reseasonalized_Forecast = reseasonalized_forecast))

#D
x <- ts_data

# Estimate KMO12 and LE
KMO_12 <- ma(x,12)
LE <- x/KMO_12

# Estimate SE and deseasonalized series

DE <- seasonal_indices
des_x <- deseasonalized_ts

plot(x) 
lines(des_x, col="red", ylab="",xlab="time")
legend("topleft", legend=c("Data", "Deseasonalized"),
       col=c("black", "red"), lty=1, cex=0.8)

# Estimate KMO3
KMO_3 <- ma(des_x,3)
KKMO_3 <- ma(KMO_3,3)
KKMO_3[2] <- KMO_3[2] #Fill missing values
KKMO_3[length(KKMO_3)-1] <- KMO_3[length(KKMO_3)-1]
KKMO_3[1] <- (des_x[1]+des_x[2]+KMO_3[2]-KMO_3[3])/2
KKMO_3[length(KKMO_3)] <- (des_x[length(KKMO_3)]+des_x[length(KKMO_3)-1]+KMO_3[length(KKMO_3)-1]-KMO_3[length(KKMO_3)-2])/2

# Estimate trend
xx <- c(1:length(x))
yy <- KKMO_3
lrl <- lm(yy~xx)
summary(lrl)
Trend <- ts(as.numeric(predict(lrl)), frequency = 12, start = c(1984,10))

plot(x) 
lines(des_x, col="red", ylab="",xlab="time")
lines(Trend, col="green", ylab="",xlab="time")
lines(KKMO_3, col="purple", ylab="",xlab="time")
legend("topleft", legend=c("Data", "Deseasonalized", "T", "TxC"),
       col=c("black", "red", "green", "purple"), lty=1, cex=0.8)



# Method 2 - Forecasts based on decomposition
FM <- Trend*DE
plot(x, ylab="",xlab="time") 
lines(FM, col="red")
legend("topleft", legend=c("Data", "Forecast"),
       col=c("black", "red"), lty=1, cex=0.8)
stD <- sd(FM)
Dmean <- mean(des_x)

# Method 1
ratio_m1_1 <- des_x/KKMO_3
ratio_m1_2 <- des_x/FM

# Method 3 - KMO5 and KMO7
KMO5 <- ma(des_x,5)
KMO7 <- ma(des_x,7)
ratio_m3 <- KMO7/KMO5

# Method 4
ratio_m4 <- des_x/KMO_12


# Apply method 1
criteria_1 <- data.frame(ratio_m1_1, ratio_m1_2)
ta = 2 ; tb = 5
criteria_1$is_r1 = criteria_1$is_r2 <- 0
criteria_1[criteria_1$ratio_m1_1 >= 1.1-ta/100,]$is_r1 <- 1
criteria_1[criteria_1$ratio_m1_1 <= 0.9+ta/100,]$is_r1 <- 1
criteria_1[criteria_1$ratio_m1_2 >= 1.25-tb/100,]$is_r2 <- 1
criteria_1[criteria_1$ratio_m1_2 <= 0.75+tb/100,]$is_r2 <- 1
criteria_1$is <- criteria_1$is_r1 * criteria_1$is_r2
criteria_1$data <- x
criteria_1[criteria_1$is==0,]$data <- NA
plot(x, ylab="",xlab="time") 
points(ts(criteria_1$data, frequency = 12, start = c(1984,10)), col="red", pch = 5)

# Apply method 2
t = 0.6
upper <- Dmean + (3-t)*stD
lower <- Dmean - (3-t)*stD
criteria_2 <- data.frame(des_x, x,upper, lower)
criteria_2$is_r <- 0
criteria_2[(criteria_2$des_x >= upper)|(criteria_2$des_x <= lower),]$is_r <- 1
criteria_2[criteria_2$is_r==0,]$x <- NA
points(ts(criteria_2$x, frequency = 12, start = c(1984,10)), col="blue", pch = 19)

# Apply method 3
t = 1
criteria_3 <- data.frame(des_x, x, ratio_m3)
criteria_3$is_r <- 0
criteria_3[is.na(criteria_3$ratio_m3),]$ratio_m3 <- 1
criteria_3[(criteria_3$ratio_m3 >= (1.05-t/100))|(criteria_3$ratio_m3 <= (0.95+t/100)),]$is_r <- 1
criteria_3[criteria_3$is_r==0,]$x <- NA
points(ts(criteria_3$x, frequency = 12, start = c(1984,10)), col="green", pch = 10)

# Apply method 4
t = 2
criteria_4 <- data.frame(des_x, x, ratio_m4)
criteria_4$is_r <- 0
criteria_4[is.na(criteria_4$ratio_m4),]$ratio_m4 <- 1
criteria_4[(criteria_4$ratio_m4 >= (1.1-t/100))|(criteria_4$ratio_m4 <= (0.9+t/100)),]$is_r <- 1
criteria_4[criteria_4$is_r==0,]$x <- NA
points(ts(criteria_4$x, frequency = 12, start = c(1984,10)), col="orange", pch = 20)

legend("topleft", legend=c("Method 1", "Method 1","Method 3","Method 4"),
       col=c("red", "blue", "green", "orange"), pch=c(5, 19,10,20), cex=0.8)

#COmbine results
criteria <- data.frame(criteria_1$is, criteria_2$is_r, criteria_3$is_r, criteria_4$is)
colnames(criteria) <- c("m1", "m2", "m3", "m4")
criteria$SE <- criteria$m4*2
criteria$data <- x
criteria$data_adj <- des_x
criteria[criteria$SE<2,]$data <- NA
plot(x, ylab="",xlab="time") 
points(ts(criteria$data, frequency = 12, start = c(1984,10)), col="red", pch = 5)

for (i in 2:nrow(criteria)){
  if (is.na(criteria$data[i])==F){
    criteria$data_adj[i] <- (criteria$data_adj[i-1] + criteria$data_adj[i+1])/2
  }
}
Adjusted <- ts(criteria$data_adj*DE, frequency = 12, start = c(1984,10))
Impact <- (des_x-criteria$data_adj)*100/criteria$data_adj
Impact[Impact!=0]

plot(x, ylab="",xlab="time") 
lines(Adjusted, col="red")
legend("topleft", legend=c("Data", "Adjusted"),
       col=c("black", "red"), lty=1, cex=0.8)

plot(
  c(time_axis, forecast_dates), 
  c(Adjusted, rep(NA, length(forecast_dates))), 
  type = "l", col = "black", lwd = 2,
  xlab = "Time", ylab = "Shipments Count",
  main = "Holt's Linear Trend with Reseasonalized Forecast",
  xlim =  range(c(time_axis, forecast_dates)), 
  ylim = c(min(ts_data, reseasonalized_forecast), max(ts_data, reseasonalized_forecast)),
  xaxt = "n" # Suppress default x-axis
)

lines(time_axis, deseasonalized_ts, col = "red", lty = 2, lwd = 2)
lines(forecast_dates, reseasonalized_forecast, col = "blue", lty = 2, lwd = 2)
lines(forecast_dates, holt_forecast, col = "blue", lty = 2, lwd = 2)
legend(
  "topleft", legend = c("Observed Data", "Deseasonalized Data", "Forecast"),
  col = c("black", "red", "blue"), lty = c(1, 2, 2), lwd = 2
)
axis(1, 
     at = c(time_axis, forecast_dates), 
     labels = format(c(time_axis, forecast_dates), "%b %Y"), 
     las = 2, 
     cex.axis = 0.7)

holt_model2 <- holt(Adjusted, h=12)

# Forecast for the next 12 months
holt_forecast2 <- holt_model2$mean

# Reseasonalize the forecast

reseasonalized_forecast2 <- holt_forecast2 * next_seasonal_indices

