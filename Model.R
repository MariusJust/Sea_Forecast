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


# ===================================== deseasonalising the data ===================================== #


fit.SIE <- tslm(SIE ~ season)
residuals <- resid(fit.SIE)
SIE_ds <- residuals

fit.C02 <- tslm(CO2 ~ season)
residuals <- resid(fit.C02)
CO2_ds <- residuals


fit.AT <- tslm(AT ~ season)
residuals <- resid(fit.AT)
AT_ds <- residuals

fit.SL <- tslm(SL ~ season)
residuals <- resid(fit.SL)
SL_ds <- residuals


# ===================================== Plotting deseasonalised data ===================================== #

ds_data <- data.frame(
  Month = 1:length(SIE_ds),  # Assuming the time series is monthly
  SIE_ds = SIE_ds,
  CO2_ds = CO2_ds,
  AT_ds = AT_ds,
  SL_ds = SL_ds
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



# testing for unit root 

unit_root <- adf.test(SL_ds)
unit_root1 <- adf.test(CO2_ds)

#The null hypothesis can't be rejected, therefore both time series are non-stationary


# ===================================== Creating test and training data ===================================== #

#Splitting up into a training, validation and test-set 
train_prop <-0.8 
test_prop <- 0.2

# Calculate the number of observations for each set
n <- length(SL_ds)
n_train <- round(train_prop * n)
n_test <- n - n_train

# Create training, validation, and testing datasets
set.seed(123)  # for reproducibility
train_data <- ds_data[1:n_train,]
test_data <- ds_data[(n_train+1):n,]



# ===================================== Random Forrest ===================================== #

# Create lagged data

lag_order <- 2

SL_ts_full <- embed(ds_data$SL_ds, lag_order+1)
#embedding the training data 
SL_ts_mbd <- embed(train_data$SL_ds, lag_order+1)

#The target column
y_train <- SL_ts_mbd[, 1] # the target

#Containing the 6 lags of SL as it is the target variable
X_train <- SL_ts_mbd[, -1] # everything but the target

# There is one overlapping value between the test and training set but that should not be a major concern 
y_test <- window(test_data$SL_ds) 
X_test <- SL_ts_full[nrow(SL_ts_mbd)+1, c(1:lag_order)] # the test set consisting # of the six most recent values (we have six lags) of the training set. 



for (i in 1:2){
  # set seed
  set.seed(2019)
  # fit the model

  fit_rf <- randomForest(X_train, y_train)
  
  # predict using the test set
  forecasts_rf[i] <- predict(fit_rf, X_test)
  
  
  # here is where we repeatedly reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train <- y_train[-1] 
  X_train <- X_train[-nrow(X_train), ] 
  
  print(tail(y_train))
  print(tail(X_train))
  X_test <- SL_ts_full[nrow(SL_ts_mbd)+i, c(1:lag_order)]
  print(X_test)
  
}


# ===================================== Rolling window forecast ===================================== #


# = Number of windows and window size
w_size = length(train_data$SL_ds)
n_windows = length(test_data$SL_ds)

# = Rolling Window Loop = #
forecasts = foreach(i=1:n_windows, .combine = rbind) %do%{
  
  # = Select data for the window (in and out-of-sample) = #
  X_in = ds_data[i:(w_size + i - 1), ] # = change to X[1:(w_size + i - 1), ] for expanding window
  X_out = ds_data[w_size + i, ]
  
  #  Simple Regression Model #
  m1 = lm(SL_ds ~ CO2_ds, data = X_in)
  f1 = predict(m1, X_out)
  
  # Combined model # 
  m2 <- lm(SL_ds ~ AT_ds + CO2_ds + SIE_ds, data=X_in)
  f2 <- predict(m2, X_out)
  
  # ARIMA model # 
  
  arima_model <- auto.arima(X_in$SL_ds)
  
  f3 <- forecast(arima_model, h = 1)
  
  # VECM model #
  
  VECM<-VECM(X_in[,c("SL_ds", "CO2_ds")], lag=6, r=1)
  f4 <- data.frame(predict(VECM, n.ahead=1))
  
  # Random Forest 

  
  # Random Walk #
  f5 = tail(X_in$SL_ds, 1)
  
  return(c(f1, f2, f3$mean, f4$SL_ds, f5))
}

result_matrix<-data.frame(cbind(forecasts, test_data$SL_ds, test_data$Month))

colnames(result_matrix) <- c("LM1", "LM2", "ARIMA", "VECM", "RW", "Actual", "Month")


# ===================================== Performance meassures ===================================== #

# Define the predictors (forecasts) and the outcome (actual)
predictors <- result_matrix[, 1:5]
outcome <- result_matrix$Actual

# Calculate performance metrics

for (i in 1:length(predictors)){
  
  performance <- postResample(predictors[i], outcome)
  print(names(predictors)[i])
  print(performance)

}


# ===================================== Plotting data ===================================== #


# Reshape the data to long format
forecast_df_long <- pivot_longer(result_matrix, 
                                 cols = c(LM1, LM2, ARIMA, VECM, RW, Actual), 
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



# ===================================== VECM model ===================================== #

#Binding CO2 and SL 
dset <- cbind(SL, CO2)

#lag selection Criteria 

lagselect <- VARselect(dset, lag.max = 7)
lagselect$selection

#Model suggests using 7 lags, so we use 7-1=6 lags

#Johansen testing (Trace)

ctest <- ca.jo(dset, type="trace", ecdet="const", K=6, season=12)
summary(ctest)

#rejecting nullhypothesis, so we see that we have 1 cointegrating relationship 

# building VECM model - we only use the training observations for this


VECM<-VECM(ds_data[1:n_train, c("SL_ds", "CO2_ds")], lag=6, r=1)
summary(VECM)

forecast_VECM <- data.frame(predict(VECM, n.ahead=1))

forecast_VECM$SL_ds

# ===================================== Random Forrest ===================================== #







# ===================================== Plotting our models ===================================== #

# Combine forecasts into a dataframe
forecast_df <- data.frame(Actual = test_data$SL_ds, 
                          Random_walk = forecast_RW,
                          Forecast_CO2 = forecast_CO2, 
                          #forecast_VECM = forecast_VECM$SL,
                          #Forecast_Combined = forecast_combined,
                          time = test_data$Month)
















#

#we hav



acf(SL_ds_diff)
pacf(train_data)



# Forecast using the ARIMA model on test data
forecast_test <- forecast(arima_model, h = length(test_data))



test_ts <- ts(test_data, start = start(test_data))

# Create a data frame containing actual and forecasted values
plot_data <- data.frame(
  Date = time(test_ts),
  Actual = as.numeric(test_ts),
  Forecast = forecast_test$mean
)

# Plot the real data vs the forecast data
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Forecast, color = "Forecast"), size = 1, linetype = "dashed") +
  labs(y = "SL Deseasonalized", color = "Data", title = "Forecast vs Actual Data") +
  theme_minimal() +
  theme(legend.position = "bottom")












accuracy_test <- accuracy(forecast_test, x = test_data)
accuracy_test