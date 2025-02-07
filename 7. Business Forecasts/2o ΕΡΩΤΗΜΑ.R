dat <- read.csv("C:\\ntua\\4ο εξάμηνο\\Προβλέψεις\\EP\\ft_project_insample_24.csv", header = TRUE)

second_row <- dat[dat$id == 2, ]

values <- as.numeric(second_row[6:ncol(dat)])

values <- values[1:(which(is.na(values))[1] - 1)]
# Extract metadata
start_year <- second_row$start_year
start_period <- second_row$start_period
frequency <- second_row$frequency
description <- second_row$description

#A

# Generate time axis as Date objects
start_date <- as.Date(paste0(start_year, "-", start_period, "-01"))
time_axis <- seq.Date(from = start_date, by = paste(12/frequency, "months"), length.out = length(values))

# Plot the data
plot(
  time_axis, values, type = "l", 
  xlab = "Time", ylab = "Toothpaste stock",
  main = paste("Toothpaste stock Monthly"),
  xaxt = "n" # Suppress default x-axis
)

# Add custom x-axis labels with months and years
axis(1, at = time_axis, labels = format(time_axis, "%b %Y"), las = 2, cex.axis = 0.7)

#B 
ts_data <- ts(values, start = c(start_year, start_period), frequency = frequency)

# Decompose the time series
fit <- decompose(ts_data, type="additive")

# Plot the decomposition
plot(fit)

#C
library(forecast)
library(zoo)

ADIDA_avg <- function(x, h, al) {
  AS <- as.numeric(na.omit(as.numeric(rollapply(tail(x, (length(x) %/% al) * al), al, FUN=mean, by=al))))
  forecast <- rep(as.numeric(ses(AS, h=1)$mean), h)  
  return(forecast)
}

ADIDA_frc <- ts(ADIDA_avg(ts_data, h=5, al=3), start = end(ts_data) + c(0,1), frequency = 12)


# Generate time labels for both historical and forecast periods
time_axis <- seq.Date(from = as.Date(paste0(start_year, "-", start_period, "-01")), 
                      by = "month", length.out = length(values) + 5)  # Extra 5 months for forecast

# Convert ADIDA_frc to a vector (avoid ts object misalignment)
forecast_values <- as.numeric(ADIDA_frc)

# Set x-axis range to include forecast period
xlim_range <- range(time_axis)

# Plot the original data
plot(time_axis[1:length(values)], values, type = "l", 
     xlab = "Time", ylab = "Toothpaste stock",
     main = "Toothpaste Stock Forecast (ADIDA)", xaxt = "n",
     ylim = range(c(values, forecast_values)), xlim = xlim_range)

# Add forecasted values
lines(time_axis[(length(values) + 1):(length(values) + 5)], forecast_values, col = "purple", lwd = 2, lty = 2)

# Add custom x-axis with month names and years
axis(1, at = time_axis, labels = format(time_axis, "%b %Y"), las = 2, cex.axis = 0.7)

# Add legend
legend("topright", legend = c("Historical Data", "ADIDA Forecast"), col = c("black", "purple"), lty = c(1,2))
