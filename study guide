##############################################################################
# TIME SERIES STUDY GUIDE (Labs 1–7 + Extras)
# ----------------------------------------------------------------------------
# This single R script demonstrates code snippets for key time-series methods:
#   - Lowess smoothing
#   - Basic plotting of time-series objects
#   - Decomposition (seasonality, trend)
#   - Autocorrelation (ACF), Partial Autocorrelation (PACF)
#   - Differencing
#   - Cumulative periodograms / Periodogram
#   - ARIMA modelling
#   - Forecasting
#   - Vector Autoregression (VAR)
#   - Spectral analysis
#
# PLUS some additional ideas:
#   - Stationarity testing (adf.test)
#   - Box-Cox / log transformations
#   - Residual diagnostics
#   - Cross-correlation (CCF)
##############################################################################

###############################
# 0. PRELIMINARIES / PACKAGES
###############################

# You might need the following packages installed:
# install.packages("vars")
# install.packages("tseries")   # for adf.test
# install.packages("forecast")  # for BoxCox or auto.arima, if desired

library(vars)       # for VAR, VARselect, etc.
library(tseries)    # for adf.test (stationarity)
library(forecast)   # for BoxCox transformations, auto.arima, etc.


###############################
# SECTION 1: LOWESS SMOOTHING
###############################
# Lowess is a locally weighted polynomial regression method for smoothing.
# Example with built-in co2 data:

plot(co2, ylab = expression(CO[2]), main = "CO2 Series with Lowess")
co2.lowess <- lowess(x = time(co2), y = as.numeric(co2), f = 0.1) 
lines(co2.lowess$x, co2.lowess$y, col = "red", lwd = 2)

# Another quick example with random data:
set.seed(101)
x <- 1:100
y <- sin(x/10) + rnorm(100, 0, 0.2)
plot(x, y, main = "Simple Sine + Noise with Lowess Smoother")
lines(lowess(x, y, f = 0.2), col = "blue", lwd = 2)


#########################################
# SECTION 2: DECOMPOSE INTO SEASONALITY
#########################################
# Using 'nottem' (monthly average temperatures at Nottingham):

plot(nottem, main = "Nottem (Monthly Temperatures)")
nottem.decomp <- decompose(nottem)  # classical approach
plot(nottem.decomp, main = "Classical Decomposition of Nottem")

# Using STL:
stl.fit <- stl(nottem, s.window = "periodic")
plot(stl.fit, main = "STL Decomposition of Nottem")


############################################
# SECTION 3: ACF, PACF (Autocorrelation)
############################################
# Lab 2 style

plot(lh, type = "o", main = "Luteinizing Hormone Data")
acf(lh, main = "ACF of LH")
pacf(lh, main = "PACF of LH")

# Another example with 'co2':
acf(co2, main = "ACF of CO2 Data")
pacf(co2, main = "PACF of CO2 Data")


############################
# SECTION 4: DIFFERENCING
############################
plot(AirPassengers, main = "AirPassengers (Original)")

# First difference:
AP_diff <- diff(AirPassengers, lag = 1)
plot(AP_diff, main = "AirPassengers - First Difference")

# Seasonal difference at lag=12:
AP_diff12 <- diff(AirPassengers, lag = 12)
plot(AP_diff12, main = "AirPassengers - Seasonal Difference at lag 12")

# Double difference:
AP_diff12_1 <- diff(AP_diff12, lag = 1)
plot(AP_diff12_1, main = "AirPassengers - Double Differenced")


######################################
# SECTION 5: CUMULATIVE PERIODOGRAM
######################################
noise <- ts(rnorm(200))
cpgram(noise, main = "Cumulative Periodogram of White Noise")

cpgram(lh, main = "Cumulative Periodogram of LH Data")


#############################
# SECTION 6: PERIODOGRAM
#############################
# 'spectrum()' for frequency-domain analysis

spectrum(nottem, main = "Periodogram of Nottem")

# Simulated sinusoid:
t <- 1:500
y2 <- sin(2 * pi * t / 50) + 0.5*rnorm(500)
spectrum(ts(y2), main = "Periodogram of Simulated 50-day Cycle")


##################################
# SECTION 7: ARIMA MODELLING
##################################
# Example with 'nottem' for a S-ARIMA:

fit_arima <- arima(
  nottem,
  order = c(1, 0, 0),  # ARIMA(p,d,q)
  seasonal = list(order = c(2,1,0), period = 12)
)
fit_arima

tsdiag(fit_arima)
cpgram(residuals(fit_arima), main = "CPgram of ARIMA Residuals")

# Alternatively, 'auto.arima()' from the forecast package:
# (Uncomment if you want to see how auto.arima might pick a model)
# auto_arima_model <- auto.arima(nottem)
# auto_arima_model


###############################
# SECTION 8: FORECASTING
###############################
n.ahead <- 12
preds <- predict(fit_arima, n.ahead = n.ahead)
ts.plot(
  nottem,
  preds$pred,
  preds$pred + 2*preds$se,
  preds$pred - 2*preds$se,
  col = c(1, 2, 4, 4),
  main = "Nottem + ARIMA Forecast"
)


################################################
# SECTION 9: MULTIVARIATE ANALYSIS (VAR Model)
################################################
# Quick example with two correlated time series:
set.seed(123)
x1 <- arima.sim(list(ar = 0.5), n = 200)
x2 <- arima.sim(list(ar = c(0.6, -0.3)), n = 200) + 0.3*x1
multi_data <- cbind(x1, x2)
plot.ts(multi_data, main = "Two Series for VAR Demo")

VARselect(multi_data, lag.max = 5, type = "const")
var_fit <- VAR(multi_data, p = 1, type = "const")
summary(var_fit)

serial.test(var_fit, lags.pt = 10, type = "PT.asymptotic")


################################
# SECTION 10: SPECTRAL ANALYSIS
################################
set.seed(2)
arma_data <- arima.sim(model = list(ar = 0.6, ma = -0.4), n = 300)
spectrum(arma_data, spans = c(3,3), main = "Spectrum of ARMA(1,1)-type data")


################################################
# EXTRA 1: STATIONARITY TESTS (adf.test)
################################################
# If we suspect a series is non-stationary, we can run the Augmented Dickey-Fuller test.
# For a random walk type data:
rw_data <- cumsum(rnorm(200))
adf_test_rw <- adf.test(rw_data)  # typically fails to reject non-stationary
adf_test_rw

# Compare to a stationary AR(1):
ar_data <- arima.sim(list(ar = 0.5), n = 200)
adf_test_ar <- adf.test(ar_data)
adf_test_ar


################################################
# EXTRA 2: BOX-COX / LOG TRANSFORM
################################################
# Stabilise variance or approach normality. 
# 'forecast' package provides BoxCox and its inverse, etc.

AP_log <- log(AirPassengers)  # simple log transform
plot(AP_log, main = "AirPassengers (Log Transformed)")

# Or the more general BoxCox approach:
lambda <- BoxCox.lambda(AirPassengers)  # finds suitable lambda
AP_bc <- BoxCox(AirPassengers, lambda)
plot(AP_bc, main = paste("AirPassengers BoxCox, lambda =", round(lambda, 3)))


################################################
# EXTRA 3: RESIDUAL DIAGNOSTICS
################################################
# After fitting an ARIMA or linear model, always check residuals for correlation,
# normality, etc. For ARIMA:
res_arima <- residuals(fit_arima)
acf(res_arima, main = "ACF of ARIMA residuals")
pacf(res_arima, main = "PACF of ARIMA residuals")
qqnorm(res_arima); qqline(res_arima, col = "red")


################################################
# EXTRA 4: CROSS-CORRELATION (CCF)
################################################
# If you want to see if one series leads/lags another, use ccf().
# For the multi_data from the VAR example:

ccf(x1, x2, main = "Cross-correlation between x1 and x2")


##############################################################################
# END OF STUDY GUIDE
##############################################################################
