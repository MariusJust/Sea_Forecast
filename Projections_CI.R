#=========================================================================================================== #
#                       
#                                     This Tool makes proejections based on RCP
#
#               
# =========================================================================================================== #

# ===================================== Load data and packages ============================================== #


cat("\014") 
graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace

pacman::p_load(pacman, urca, vars, tsDyn,forecast,readxl,xts, ggfortify, stargazer, tseries, ggplot2, tidyr, caret,  dplyr, foreach, randomForest, kableExtra, keras, ModelMetrics, xlsx,KFAS) #installing neccesary packages 

data <-read_excel("C:\\Users\\mariu\\OneDrive\\Skrivebord\\UNI\\8. semester\\Economic forecasting\\Project\\Code\\Final.xlsx") #Change to your own directory

projections <-read_excel("C:\\Users\\mariu\\OneDrive\\Skrivebord\\UNI\\8. semester\\Economic forecasting\\Project\\Code\\RCP1.xlsx") #Change to your own directory

tail(projections)

CO2 <-ts(data$CO2, freq=12, start=1993)
SL <-ts(data$GMSL, freq=12, start=1993)

conditional <- function (case){
  if (case == "WC") {
    case <- projections[,3]
  } else if (case =="BC")  {
    case <- projections[,4]
  } else {
    case <- projections[,2]
  }
  
  
  # ===================================== Linear interpolation ===================================== #
  case <- ts(case, start=1993)
  
  # Create a new time series with monthly frequency for the same period
  new_time <- seq(from=start(case)[1], to=end(case)[1], by=1/12)
  
  # Use the approx function to interpolate the data
  case_monthly <- approx(x=time(case), y=case, xout=new_time)$y
  
  # Convert the result to a time series object
  case <- ts(case_monthly, start=1993, freq=12)
  
  # ===================================== Stationarity ===================================== #
  
  fit.C02 <- tslm(CO2 ~ season)
  residuals <- resid(fit.C02)
  CO2_ds <- residuals
  
  fit.SL <- tslm(SL ~ season)
  residuals <- resid(fit.SL)
  SL_ds <- residuals
  
  
  # ===================================== Data preperation and plotting ===================================== #
  
  #deseasonalised data
  ds_data <- data.frame(
    Month = 1:length(CO2_ds),  # Assuming the time series is monthly
    CO2 = CO2_ds,
    SL= SL_ds
  )
  
  # ===================================== Estimating VECM model on full sample ===================================== #
  
  VECM<-VECM(ds_data[,c("SL", "CO2")], lag=6, r=1)
  
  VAR<- VARrep(VECM)
  
  
  # ===================================== projections ===================================== #
  
  
  case <- ts(case, freq=12, start=1993)
  
  #deseasonalising the data 
  fit.case <- tslm(case ~ season)
  residuals <- resid(fit.case)
  case_ds <- residuals
  
  #Differencing for stationarity
  case_diff = diff(case_ds)
  SL = diff(SL_ds)
  
  
  
  #creating matrix to store SL values and the Combined_Worst_case
  
  storage <- as.data.frame(matrix(data=NA, 
                                  nrow=1284, ncol=2))
  
  storage[1:371,1] <- ts(SL)
  storage[1:1284,2] <- ts(case_diff)[1:1284]
  
  colnames(storage) <- c("SL", "CO2")
  
  
  # ===================================== Setting up State Space model ===================================== #
  
  #extract intercept from VAR
  intercept <- VARrep(VECM)[,1]
  
  # observation is just x,y
  Ft <- cbind(diag(1,2),diag(0,2),diag(0,2),diag(0,2),diag(0,2),diag(0,2),diag(0,2))
  
  # Extracting the vectors
  vec1 <- VARrep(VECM)[1,-1]
  vec2 <- VARrep(VECM)[2,-1]
  
  # Filling the matrix with repetitions
  Tt <- matrix(c(vec1, 
                 vec2,
                 c(1,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                 c(0,1,0,0,0,0,0,0,0,0,0,0,0,0),
                 c(0,0,1,0,0,0,0,0,0,0,0,0,0,0),
                 c(0,0,0,1,0,0,0,0,0,0,0,0,0,0),
                 c(0,0,0,0,1,0,0,0,0,0,0,0,0,0),
                 c(0,0,0,0,0,1,0,0,0,0,0,0,0,0),
                 c(0,0,0,0,0,0,1,0,0,0,0,0,0,0),
                 c(0,0,0,0,0,0,0,1,0,0,0,0,0,0),
                 c(0,0,0,0,0,0,0,0,1,0,0,0,0,0),
                 c(0,0,0,0,0,0,0,0,0,1,0,0,0,0),
                 c(0,0,0,0,0,0,0,0,0,0,1,0,0,0),
                 c(0,0,0,0,0,0,0,0,0,0,0,1,0,0)
  ),
  nrow=14, byrow=TRUE)
  
  # residuals variance matrix from VECM
  
  Qt <- cov(residuals(VECM))
  
  ts_data <- ts(storage[, c("SL", "CO2")])
  
  # set up statespace model
  
  K2 <- SSModel(ts_data ~
                  # add the intercept 
                  SSMcustom(Z = diag(0,2), T = diag(2), Q = diag(0,2), 
                            a1 = matrix(intercept,2,1), P1inf = diag(0,2),index=c(1,2),
                            state_names=c("mu_SL","mu_CO2")) +
                  
                  # add the VAR part (excluding intercept)
                  
                  SSMcustom(Z=Ft,  # observations
                            
                            T=Tt, # state transtion (from VECM)
                            
                            Q=Qt,  # state innovation variance (from VECM)
                            
                            index=c(1,2), # we observe variables 1 & 2 from statespace (without noise)
                            
                            
                            state_names=c("SL","CO2","SLlag1","CO2lag1", "SLlag2","CO2lag2","SLlag3","CO2lag3", "SLlag4","CO2lag4","SLlag5","CO2lag5","SLlag6","CO2lag6"),  # name variables
                            
                            a1=c(ts_data[7,"SL"], ts_data[7,"CO2"], ts_data[6,"SL"], ts_data[6,"CO2"],ts_data[5,"SL"], ts_data[5,"CO2"], ts_data[4,"SL"], ts_data[4,"CO2"], ts_data[3,"SL"], ts_data[3,"CO2"],ts_data[2,"SL"], ts_data[2,"CO2"],ts_data[1,"SL"], ts_data[1,"CO2"]),   #initialize values
                            
                            P1=1*diag(1,14)),  # make initial variance very large
                
                
                H=diag(0,2) # no noise in observation equation
  )
  
  
  
  
  
  # Predict values for our statespace model- sea level values
  # Predict values for our statespace model
  SL<-predict(K2,interval = "confidence", level = 0.95 )[[1]]
  predict(K2,interval = "confidence", level = 0.6 )[[1]] %>% 
    #convert to time series
    ts %>% 
    # plot
    autoplot()+autolayer(ts_data[,1],color="black")+
    scale_color_manual(values=c("red","blue","blue"))+
    labs(x="periods",y="y",
         title="Forecasts for y conditional on x",
         subtitle="y and x are bivariate VECM, x observed after period 40, y is not")+
    geom_vline(xintercept=40,linetype=2)+
    annotate(geom="text",x=40.5,y=0, label="Forecasts for X available, \nY unobserved",hjust=0)
  
  SL_fit<-cumsum(SL[,1])
  lwr_diff <- cumsum(SL[,2] - SL[,1])
  upr_diff <- cumsum(SL[,3] - SL[,1])
  
  # Add the differences to the fitted values in the original series
  SL_lwr <- SL_fit + lwr_diff
  SL_upr <- SL_fit + upr_diff
  
  plot_data <- data.frame(Time = time(SL_fit), Fit = SL_fit, Lwr = SL_lwr, Upr = SL_upr)
  
  
  
  
  return(plot_data)
  
}



Worst_case <- ts(conditional("WC"), start=1993, freq=12)
Best_case <- ts(conditional("BC"), start=1993, freq=12)
Bau <- ts(conditional(""), start=1993, freq=12)


worst_case_df <- data.frame(Time = time(Worst_case), Fitted = Worst_case[,2], Lwr = Worst_case[,3], Upr = Worst_case[,4])
best_case_df <- data.frame(Time = time(Best_case), Fitted = Best_case[,2], Lwr = Best_case[,3], Upr = Best_case[,4])
bau_df <- data.frame(Time = time(Bau), Fitted = Bau[,2], Lwr = Bau[,3], Upr = Bau[,4])

worst_case_df$is_observation <- ifelse(worst_case_df$Time <= 372, "Observations", "Projections")
best_case_df$is_observation <- ifelse(best_case_df$Time <= 372, "Observations", "Projections")
bau_df$is_observation <- ifelse(bau_df$Time <= 372, "Observations", "Projections")

# Plot the data
ggplot() +
  # Worst case
  geom_line(data = worst_case_df, aes(x = Time, y = Fitted, color = "Worst Case"), size = 0.2) +
  geom_ribbon(data = worst_case_df, aes(x = Time, ymin = Lwr, ymax = Upr, fill = "Worst Case"), alpha = 0.2) +
  # Best case
  geom_line(data = best_case_df, aes(x = Time, y = Fitted, color = "Best Case"), size = 0.2) +
  geom_ribbon(data = best_case_df, aes(x = Time, ymin = Lwr, ymax = Upr, fill = "Best Case"), alpha = 0.2) +
  # BAU case
  geom_line(data = bau_df, aes(x = Time, y = Fitted, color = "Bau"), size = 0.2) +
  geom_ribbon(data = bau_df, aes(x = Time, ymin = Lwr, ymax = Upr, fill = "Bau"), alpha = 0.2) +
  labs(x = "Time", y = "GMSL") +
  scale_color_manual(values = c("Worst Case" = "red", "Best Case" = "blue", "Bau" = "dark green", "black"), name="Scenarios") +
  scale_fill_manual(values = c("Worst Case" = "red", "Best Case" = "blue", "Bau" = "green")) +
  theme_minimal()+
  guides(fill = FALSE) +
  geom_vline(xintercept = 2024, linetype = "dotted")
  
  




