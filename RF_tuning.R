# =========================================================================================================== #
#                       
#                                     This Tool does parameter tuning of RF
#
#               
# =========================================================================================================== #

# ===================================== Load data and packages ============================================== #


cat("\014") 
graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace

pacman::p_load(pacman, urca, vars, tsDyn,forecast,readxl,xts, ggfortify,stargazer, tseries, ggplot2, tidyr, caret,  dplyr, foreach, randomForest, doParallel) #installing neccesary packages 

data <-read_excel("C:\\Users\\mariu\\OneDrive\\Skrivebord\\UNI\\8. semester\\Economic forecasting\\Project\\Data\\Processed\\Final.xlsx") #Change to your own directory


n <- nrow(data)


# ===================================== declare variables as time series ===================================== #

SIE <-ts(data$SIE, freq=12, start=1993)
CO2 <-ts(data$CO2, freq=12, start=1993)
AT <-ts(data$AT, freq=12, start=1993)
SL <-ts(data$GMSL, freq=12, start=1993)




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
  CO2 = CO2_ds,
  AT = AT_ds,
  SL= SL_ds
)

#stationary data 

stationary_data <- data.frame(
  Month = 1:length(diff_CO2),  # Assuming the time series is monthly
  CO2 = diff_CO2,
  AT= diff_AT,
  SL = diff_SL
)



# ===================================== Creating test and training data - stationary data ===================================== #

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

# ===================================== Parameter tuning - Random Forest ===================================== #
# Parameters for grid search
lag_orders <- c(3, 4, 5, 6, 7, 8, 9)  # Example lag orders to search over
ntrees <- c(500, 1000, 1500, 2000)  # Example number of trees to search over

# Initialize a dataframe to store results
results <- data.frame(lag_order = numeric(), ntree = numeric(), RMSE = numeric())

#Parameters
w_size = n_train
n_windows = n_test

actual_values <- test_data$SL
# Rolling Window Loop
for (lag_order in lag_orders) {
  SL_ts_full <- embed(stationary_data$SL, lag_order+1)
  n_rf_train <- n_train-lag_order*(ncol(SL_ts_full)/(lag_order+1))
  for (ntree in ntrees) {
    # Initialize a vector to store forecasts
    forecasts <- numeric(n_windows)
    
    # Iterate over each window
    for (i in 1:n_windows) {
      # Random Forest
      set.seed(123)
      #Select data for 
      y_train <- SL_ts_full[i:(n_rf_train + i - 1), 1]
      X_train <- SL_ts_full[i:(n_rf_train + i - 1), -1]
      X_test <- SL_ts_full[n_rf_train + i - 1, c(1:lag_order)]
      
      fit_rf <- randomForest(X_train, y_train, ntree = ntree)
      
      # Predict using the test set
      f3 <- predict(fit_rf, X_test)
      
      # Store forecasts
      forecasts[i] <- f3
    }
    
    # Calculate RMSE
    RMSE <- sqrt(mean((forecasts - actual_values)^2))
    
    # Store results
    results <- rbind(results, data.frame(lag_order = lag_order, ntree = ntree, RMSE = RMSE))
  }
  cat("Finished with", lag_order, "-lag model with", ntree, "trees\n")
}

# Print results
print(results)



#min value is obtained with 500 trees and 7 lags

