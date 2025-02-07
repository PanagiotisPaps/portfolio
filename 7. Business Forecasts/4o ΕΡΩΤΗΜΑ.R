dat2 <- read.csv("C:\\ntua\\4ο εξάμηνο\\Προβλέψεις\\EP\\ft_project_outsample_24.csv", header = TRUE)

first_row2 <- dat2[dat2$id == 1, ]

real_values <- as.numeric(first_row2[2:13])


# Ensure forecast and real data are of the same length
if (length(real_values) != length(reseasonalized_forecast2)) {
  stop("Mismatch in length of real vs forecast data.")
}

# Compute error metrics
mae <- mean(abs(real_values - reseasonalized_forecast2))  # Mean Absolute Error
mape <- mean(abs((real_values - reseasonalized_forecast2) / real_values)) * 100  # Mean Absolute Percentage Error
rmse <- sqrt(mean((real_values - reseasonalized_forecast2)^2))  # Root Mean Squared Error

# Print the error metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Plot forecast vs actual values
plot(
  forecast_dates, real_values, type = "l", col = "black", lwd = 2, ylim = range(c(real_values, reseasonalized_forecast)),
  xlab = "Time", ylab = "Shipments Count", main = "Forecast vs Actual Shipments"
)
lines(forecast_dates, reseasonalized_forecast2, col = "blue", lty = 2, lwd = 2)

legend("topleft", legend = c("Actual Data", "Forecast from adjusted_time_series"), col = c("black", "blue"), lty = c(1, 2), lwd = 2)

# Print comparison table
comparison <- data.frame(
  Date = forecast_dates,
  Actual = real_values,
  Forecast = reseasonalized_forecast,
  Error = real_values - reseasonalized_forecast
)
print(comparison)

#B
# Define actual values
real <- c(6, 17, 34, 38, 0)

# Define forecast values
naive <- c(40, 40, 40, 40, 40)
naive_reseasonalized <- c(5.6, 15.6, 26.7, 3.3, 28.9)
adida <- c(18.67, 18.67, 18.67, 18.67, 18.67)
adida_reseasonalized <- c(2.6, 7.3, 12.5, 1.6, 13.5)

# Function to calculate error metrics
error_metrics <- function(real, forecast) {
  mae <- mean(abs(real - forecast))
  mape <- mean(abs((real - forecast) / (real + 1e-10))) * 100  # Avoid division by zero
  rmse <- sqrt(mean((real - forecast)^2))
  return(c(MAE = mae, MAPE = mape, RMSE = rmse))
}

# Compute metrics for each method
metrics_naive <- error_metrics(real, naive)
metrics_naive_reseasonalized <- error_metrics(real, naive_reseasonalized)
metrics_adida <- error_metrics(real, adida)
metrics_adida_reseasonalized <- error_metrics(real, adida_reseasonalized)

# Print results
cat("🔹 Naïve Model:\n", metrics_naive, "\n\n")
cat("🔹 Naïve Reseasonalized Model:\n", metrics_naive_reseasonalized, "\n\n")
cat("🔹 ADIDA Model:\n", metrics_adida, "\n\n")
cat("🔹 ADIDA Reseasonalized Model:\n", metrics_adida_reseasonalized, "\n")

error_metrics <- function(real, forecast) {
  mae <- mean(abs(real - forecast))
  rmse <- sqrt(mean((real - forecast)^2))
  return(c(MAE = mae, RMSE = rmse))
}

# Create a dataframe with actual and forecasted values
comparison_df <- data.frame(
  Month = c("Feb", "Mar", "Apr", "May", "Jun"),
  Real = real,
  Naive = naive,
  Naive_Reseasonalized = naive_reseasonalized,
  ADIDA = adida,
  ADIDA_Reseasonalized = adida_reseasonalized
)

# Compute metrics for each method
metrics_df <- data.frame(
  Model = c("Naive", "Naive Reseasonalized", "ADIDA", "ADIDA Reseasonalized"),
  rbind(
    error_metrics(real, naive),
    error_metrics(real, naive_reseasonalized),
    error_metrics(real, adida),
    error_metrics(real, adida_reseasonalized)
  )
)

# Print the dataframes
print(comparison_df)
print(metrics_df)

#C
third_row2 <- dat2[dat2$id == 3, ]

actual_values <- as.numeric(third_row2[2:25])

error_metrics <- function(actual, forecast) {
  mae <- mean(abs(actual - forecast))
  mape <- mean(abs((actual - forecast) / (actual + 1e-10))) * 100  
  rmse <- sqrt(mean((actual - forecast)^2))
  return(c(MAE = mae, MAPE = formatC(mape, format = "f", digits = 2), RMSE = rmse))
}

Prediction_errors <- error_metrics(actual_values, predictions_linear)
Prediction_90percent_errors <- error_metrics(actual_values, predictions_arima)

# Create a dataframe with the comparison
comparison_df <- data.frame(
  Model = c("Forecasts", "Forecasts 90%"),
  MAE = c(Prediction_errors["MAE"], Prediction_90percent_errors["MAE"]),
  MAPE = c(Prediction_errors["MAPE"], Prediction_90percent_errors["MAPE"]),
  RMSE = c(Prediction_errors["RMSE"], Prediction_90percent_errors["RMSE"])
)

# Print the results
print(comparison_df)
