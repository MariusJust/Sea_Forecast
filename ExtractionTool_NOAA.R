# =========================================================================================================== #
#                       
#                               This Tool extracts data on 
#   
#           I)      Air Temperature (https://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.surface.html)
#           
#   Note:
#           You are asked to define 'degrees.n', which determines the southern boundary of the domain
#           over which monthly averages will be calculated.
# 
#   Credit: Original code provided by Maximilien Göbel. Revised by Marius Just the 02.03.2024
# =========================================================================================================== #

if (!require(ncdf4)) {
  install.packages("ncdf4")             
  library(ncdf4)
}

rm(list=ls())
# ================================================ USER INTERACTION ========================================= #

# --- Where is your data stored?
directory <- "C:\\Users\\mariu\\OneDrive\\Skrivebord\\UNI\\8. semester\\Economic forecasting\\Project\\Data\\Raw"

#   Which variable do you want to extract? TCC == Total Cloud Cover; AT == Air Temp; Precip == Precipitation
I.want <- "AT"

#   Which region (latitude-wise) do you want to average over? (default = 30), Pan-Arctic = 60
#   ----> define it as "degrees north", i.e. if you want the whole grid/globe: degrees.n <- -90
degrees.n <- -90

# ================================================ USER INTERACTION ========================================= #



# =========================================================================================================== #
#                                      Import your Data
# =========================================================================================================== #

nc_file <- nc_open(file.path(directory,"air.mon.mean.nc"))

# --- Have a look at the file-structure ---- no User-Interaction required
print(nc_file)
data <- ncvar_get(nc_file, verbose = F)

nc_file$dim$leve$vals
# --- If you want to look at the values/grid, uncomment the following line
#View(nc_file[["dim"]][["lat"]][["vals"]])


# =========================================================================================================== #
#           Create a data frame where to store your data ---- no User-Interaction required
# =========================================================================================================== #

# --- Set your Grid
grid_lon <- dim(data)[1]
grid_lat <- dim(data)[2]

# --- Determine start and end of your time-series
start_year <- as.integer(nc_file[["dim"]][["time"]][["vals"]][[1]]/(24*365)+1800)
end_year <- as.integer(nc_file[["dim"]][["time"]][["vals"]][[dim(data)[4]]]/(24*365)+1800)
length_sequence <- dim(data)[4] 

length(nc_file[["dim"]][["time"]][["vals"]])
yearly_sequence <- seq(start_year,end_year,1)



# --- Create monthly- and annual-sequences (and yes, there are easier/more efficient ways to do it)
yearly_repetition <- NA
for (y_rep in yearly_sequence){
  reps_12 <- rep(y_rep, times = 12)
  yearly_repetition <- c(yearly_repetition, reps_12)
}
yearly_repetition <- yearly_repetition[2:length(yearly_repetition)]
monthly_repetition <- rep(1:12,times=length(yearly_sequence))

# --- Create your storage data frame ('out_df'): Column1: Year; Column2: Month
out_df <- matrix(NA, nrow = length(yearly_repetition), ncol = 3)
out_df[,1] <- t(yearly_repetition)
out_df[,2] <- t(monthly_repetition)
colnames(out_df) <- c("year", "month", I.want)

out_df <- out_df[1:length_sequence,]


# =========================================================================================================== #
#   Extract the Monthly Averages over the domain that stretches: 'degree.n' - 90 Degrees Northern Latitude
#                                   ---- no User-Interaction required ----
# =========================================================================================================== #


for (r in 1:length_sequence) {
  # --- Take the average over all grid-points in your domain  
  data_month_mean <- mean(data[1:grid_lon, which(nc_file$dim$lat$vals > degrees.n), 1, r], na.rm = TRUE)
  
  # --- Assign it to your storage data-frame
  out_df[r, 3] <- data_month_mean
}

# Plotting the time series
plot(out_df[, 1] + (out_df[, 2] - 1)/12, out_df[, 3], type = "l",
     xlab = "Year", ylab = "Monthly Average",
     main = "Monthly Average Over Specified Domain")

# =========================================================================================================== #
#                                     Export your data as a csv file
# =========================================================================================================== #

write.csv(out_df, 
          file = file.path(directory, paste0(colnames(out_df)[3],"_monthlyavg_",start_year,
                                             end_year,"_",degrees.n,"Deg90.csv")), 
          row.names = FALSE)
