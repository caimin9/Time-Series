# Load the dataset
load("sharedbikes.RData")
# Check the structure of the data
str(dat)

####################################################################################################################
##2 
####################################################################################################################
# Convert date to proper date format if needed
dat$dteday <- as.Date(dat$dteday)

# Create a scatter plot with lowess smoother
plot(dat$dteday, dat$casual, 
     type = "p", 
     xlab = "Date", 
     ylab = "Number of Casual Bike Hires",
     main = "Daily Casual Bike Hires Over Time",
     col = "blue",
     pch = 20)

# Add lowess smoother
lines(lowess(dat$dteday, dat$casual, f = 0.1), 
      col = "red", 
      lwd = 2)

# Add legend
legend("topleft", 
       legend = c("Casual Bike Hires", "Lowess Smoother"),
       col = c("blue", "red"),
       pch = c(20, NA),
       lty = c(NA, 1),
       lwd = c(NA, 2))

mod = lm(casual~t+s1+c1+s2+c2+s7_1+c7_1+s7_2+c7_2,data=dat)
# The linear model removes most seasonality at annual
# and weekly periodicity, but does not do a perfect job.
# => Use time-series models to imporve fit.
dat$res = ts(residuals(mod),start=2018,frequency=365)

####################################################################################################################
##3
####################################################################################################################
dat$res
# Plot the residual time series
plot(dat$res, 
     main = "Residuals of Linear Model for Casual Bike Hires",
     xlab = "Time", 
     ylab = "Residual")

# Plot the ACF of residuals
acf(dat$res, 
    main = "Autocorrelation Function of Residuals")

# Plot the PACF of residuals
pacf(dat$res, 
     main = "Partial Autocorrelation Function of Residuals")

####################################################################################################################
##4
####################################################################################################################

# Decompose the residual series
res_decomp <- decompose(dat$res)

# Plot the decomposition
plot(res_decomp, 
     col = "blue", 
     xlab = "Time")


####################################################################################################################
##5
####################################################################################################################
# Calculate first order differences
res_diff <- diff(dat$res)

# Convert to time series
res_diff_ts <- ts(res_diff, start=2018, frequency=365)

# Decompose the differenced series
res_diff_decomp <- decompose(res_diff_ts)

# Plot the decomposition
plot(res_diff_decomp, 
     col = "blue", 
     xlab = "Time")


####################################################################################################################
##6
####################################################################################################################

# (a) Cumulative periodogram of residual series
par(mfrow=c(1,2))
cpgram(dat$res, 
       main = "Cumulative Periodogram of Residuals")

# (b) Cumulative periodogram of differenced residual series
cpgram(res_diff, 
       main = "Cumulative Periodogram of Differenced Residuals")
par(mfrow=c(1,1))



####################################################################################################################
##7
####################################################################################################################
# Define a range of p and q values to test
p_values <- 0:6
q_values <- 0:6


# Initialize a matrix to store AIC values
aic_matrix <- matrix(NA, nrow = length(p_values), ncol = length(q_values))
rownames(aic_matrix) <- paste("p =", p_values)
colnames(aic_matrix) <- paste("q =", q_values)

# Fit ARIMA(p,1,q) models and store AIC values
for (i in 1:length(p_values)) {
  for (j in 1:length(q_values)) {
    p <- p_values[i]
    q <- q_values[j]
    
    tryCatch({
      model <- arima0(dat$res, order = c(p, 1, q))
      aic_matrix[i, j] <- model$aic
    }, error = function(e) {
      aic_matrix[i, j] <- NA
    })
  }
}

# Print the AIC matrix
print(aic_matrix)

# Find the model with the lowest AIC
min_aic <- min(aic_matrix, na.rm = TRUE)
min_indices <- which(aic_matrix == min_aic, arr.ind = TRUE)
best_p <- p_values[min_indices[1]]
best_q <- q_values[min_indices[2]]

cat("Best model: ARIMA(", best_p, ",1,", best_q, ") with AIC =", min_aic, "\n")

####################################################################################################################
##8
####################################################################################################################
# Fit the best model using arima
best_model <- arima(dat$res, order = c(best_p, 1, best_q))
print(best_model)

# Extract residuals from the best model
model_residuals <- residuals(best_model)

# Plot cumulative periodogram of model residuals
cpgram(model_residuals, 
       main = "Cumulative Periodogram of ARIMA Model Residuals")



####################################################################################################################
##9
####################################################################################################################

# Load the forecast data
load("sharedbikes_forecast.RData")

# Predict from the linear model for the forecast period
linear_forecast <- predict(mod, newdata = newdat)

# Forecast from the ARIMA model
arima_forecast <- predict(best_model, n.ahead = 60)

# Combine forecasts
total_forecast <- linear_forecast + arima_forecast$pred

# Calculate prediction intervals (95%)
upper_ci <- total_forecast + 1.96 * arima_forecast$se
lower_ci <- total_forecast - 1.96 * arima_forecast$se



#forecast dates for plotting with newdata
forecast_dates <- seq.Date(from = max(dat$dteday) + 1, 
                           by = "day", 
                           length.out = length(total_forecast))

# Plot original data and forecasts
plot(dat$dteday, dat$casual, 
     type = "l", 
     xlim = c(min(dat$dteday), max(forecast_dates)),
     ylim = c(min(c(dat$casual, lower_ci)), max(c(dat$casual, upper_ci))),
     xlab = "Date", 
     ylab = "Number of Casual Bike Hires",
     main = "Casual Bike Rentals with 60-day Forecast")

# Add the forecast
lines(forecast_dates, total_forecast, col = "blue", lwd = 2)

# Add prediction intervals
lines(forecast_dates, upper_ci, col = "red", lty = 2)
lines(forecast_dates, lower_ci, col = "red", lty = 2)

# Add legend
legend("topleft", 
       legend = c("Observed", "Forecast", "95% Prediction Interval"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = c(1, 2, 1))




####################################################################################################################
#10
####################################################################################################################
mod1 = lm(registered~t+s1+c1+s2+c2+s7_1+c7_1+s7_2+c7_2,data=dat)
dat$res1 = ts(residuals(mod1),start=2018,frequency=365)
p = plot(registered~dteday,data=dat)
lines(lowess(dat$dteday,dat$registered,f=0.1),col="red",lwd=2)
lines(dat$t,fitted(mod1),col="green",lwd=2)
# Load the vars package
library(vars)
diff_casual <- diff(dat$res)
diff_registered <- diff(dat$res1)
diff_data <- cbind(diff_casual, diff_registered)
# First differences

# Use VARselect to choose p-lags by SC
lag_sel <- VARselect(diff_data, lag.max = 5, type = "const")
lag_sel
chosen_lag <- lag_sel$selection["SC(n)"]
cat("SC-based chosen lag for VAR:", chosen_lag, "\n")

final_var <- VAR(diff_data, p = chosen_lag, type = "const")
summary(final_var)

