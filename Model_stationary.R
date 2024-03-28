# =========================================================================================================== #
#                       
#                                     This Tool executes the SEA_Forecast
#
#               
# =========================================================================================================== #

# ===================================== Load data and packages ============================================== #


cat("\014") 
graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace

pacman::p_load(pacman, urca, vars, tsDyn,forecast,readxl,xts, ggfortify,stargazer, tseries, ggplot2, tidyr, caret,  dplyr, foreach, randomForest, xts()) #installing neccesary packages 

data <-read_excel("C:\\Users\\mariu\\OneDrive\\Skrivebord\\UNI\\8. semester\\Economic forecasting\\Project\\Data\\Processed\\Final.xlsx") #Change to your own directory


n <- nrow(data)


# ===================================== declare variables as time series ===================================== #

SIE <-ts(data$SIE, freq=12, start=1993)
CO2 <-ts(data$CO2, freq=12, start=1993)
AT <-ts(data$AT, freq=12, start=1993)
SL <-ts(data$GMSL, freq=12, start=1993)


#We use the variables in levels 



# ===================================== Test for seasonal behavior ===================================== #

#including trend to avoid omitted variable bias
fit.SIE <- tslm(SIE ~ season + trend)
summary(fit.SIE)

fit.CO2 <- tslm(CO2 ~ season + trend)
summary(fit.CO2)

fit.AT <- tslm(AT ~ season + trend)
summary(fit.AT)

fit.SL <- tslm(SL ~ season + trend)
summary(fit.SL)

#All variables have significant seasonal behavior, so we deseasonalise the data


# ===================================== Stationarity ===================================== #



fit.C02 <- tslm(CO2 ~ season)
residuals <- resid(fit.C02)
CO2_ds <- residuals

diff_CO2<-diff(CO2_ds)
kpss.test(diff_CO2)
adf.test(diff_CO2)
#both tests support that CO2 is a difference stationary variable

fit.AT <- tslm(AT ~ season)
residuals <- resid(fit.AT)
AT_ds <- residuals

kpss.test(AT_ds, null=c("T"))
#AT is not trend stationary 


diff_AT<-diff(AT_ds)
kpss.test(diff_AT)
adf.test(diff_AT)
#Both tests support that AT is a difference stationary variable



fit.SL <- tslm(SL ~ season)
residuals <- resid(fit.SL)
SL_ds <- residuals


diff_SL<-diff(SL_ds)
kpss.test(diff_SL)
adf.test(diff_SL)
#Both tests support that SL is a difference stationary variable




# ===================================== Data preperation and plotting ===================================== #

#deseasonalised data
ds_data <- data.frame(
  Month = 1:length(CO2_ds),  # Assuming the time series is monthly
  CO2_ds = CO2_ds,
  AT_ds = AT_ds,
  SL_ds = SL_ds
)

#stationary data 

stationary_data <- data.frame(
  Month = 1:length(diff_CO2),  # Assuming the time series is monthly
  CO2 = diff_CO2,
  AT= diff_AT,
  SL = diff_SL
)



# Reshape the dataframe for easier plotting
melted_data <- pivot_longer(ds_data, cols = -Month, names_to = "Variable", values_to = "Deseasonalized_Value")

# Plot using ggplot2
ggplot(melted_data, aes(x = Month, y = Deseasonalized_Value, color = Variable)) +
  geom_line() +
  labs(x = "Month", y = "Deseasonalized Value", color = "Variable") +
  ggtitle("Deseasonalized Time Series Plot") +
  theme_minimal() +
  facet_wrap(~ Variable, nrow = 4, scales = "free_y")  # Stacking plots vertically with free y-axis scales


# ===================================== Creating test and training data ===================================== #

#Splitting up into a training, validation and test-set 
train_prop <-0.8 
test_prop <- 0.2

# Calculate the number of observations for each set
n <- length(diff_SL)
n_train <- round(train_prop * n)
n_test <- n - n_train

# Create training, validation, and testing datasets
set.seed(123)  # for reproducibility
train_data <- stationary_data[1:n_train,]
test_data <- stationary_data[(n_train+1):n,]



# ===================================== Rolling window forecast ===================================== #

#Parameters for Random Forest 
lag_order <- 6
#fully  embedded data
SL_ts_full <- embed(stationary_data$SL, lag_order+1)
n_rf_train <- n_train-lag_order*(ncol(SL_ts_full)/(lag_order+1))

# = Number of windows and window size
w_size = n_train
n_windows = n_test

# = Rolling Window Loop = #
forecasts = foreach(i=1:n_windows, .combine = rbind) %do%{
  
  # = Select data for the window (in and out-of-sample) = #
  X_in = stationary_data[i:(w_size + i - 1), ] # = change to X[1:(w_size + i - 1), ] for expanding window
  X_out = stationary_data[w_size + i, ]
  
  # ARIMA model # 
  arima_model <- auto.arima(X_in$SL)
  f1 <- forecast(arima_model, h = 1)

  
  # Random Forest 
  
  set.seed(2019)
  y_train <- SL_ts_full[i:(n_rf_train+i-1), 1]
  X_train <- SL_ts_full[i:(n_rf_train+i-1), -1]
  X_test <- SL_ts_full[n_rf_train+i-1, c(1:lag_order)]
  
  fit_rf <- randomForest(X_train, y_train, ntree=1000)
  
  # predict using the test set
  f2 <- predict(fit_rf, X_test)
  
  
  # Random Walk #
  f3 = tail(X_in$SL, 1)
  
  return(c(f1$mean, f2, f3))
}

result_matrix<-data.frame(cbind(forecasts, test_data$SL, test_data$Month))

colnames(result_matrix) <- c("ARIMA", "RF", "RW", "Actual", "Month")


# ===================================== Performance meassures ===================================== #

# Define the predictors (forecasts) and the outcome (actual)
predictors <- cbind(result_matrix[, 1:3])
outcome <- result_matrix$Actual


for (i in 1:length(predictors)){
  
  performance <- postResample(predictors[i], outcome)
  print(names(predictors)[i])
  print(performance)
  
}


# ===================================== Plotting data ===================================== #


# Reshape the data to long format
forecast_df_long <- pivot_longer(result_matrix, 
                                 #LM1, LM2, ARIMA, VECM,
                                 cols = c(ARIMA, RW, RF, Actual), 
                                 names_to = "Variable", 
                                 values_to = "Value")

# Plot
ggplot(forecast_df_long, aes(x = Month, y = Value, color = Variable)) +
  geom_line() +
  labs(title = "Forecast Comparison",
       x = "Month",
       y = "Value") +
  theme_minimal()


# Forecasting Sea Level from regression model on AT, CO2, SIE
model_combined <- lm(SL_ds ~ AT_ds + CO2_ds + SIE_ds)
summary(model_combined)

#cointegration between CO2 and SL

residuals <- resid(model_CO2)

#testing for cointegration
plot(residuals)
adf_test<-ur.df(residuals, type="none", selectlags = "AIC")
summary(adf_test)


















