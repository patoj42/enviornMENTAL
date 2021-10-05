# # Predicting Mental Health Outcomes from Environmental Factors in NZ

# # Structure:
  # 1. Packages & Libraries
  # 2. Data Import
  # 3. Data Wrangling & Cleansing
  # 4. Exploratory Analysis
  # 5. Model Exploration: Random Forest
  # 6. Time Series & ARIMA Predictions
    #6.1 Rainfall in Hawke's Bay
    #6.2 Air quality in Wellington
    #6.3 Temperature in Canterbury
  # 7. Ridge Regression & Mental Health Predictions

# 1. Packages & Libraries

library(dplyr)
library(revgeo)
library(lubridate)
library(zoo)
library(ggplot2)
library(randomForest)
library(sarima)
library(astsa)
library(forecast)
library(caret)
library(repr)
library(genridge)
library(glmnet)
library(lambda.r)

# 2. DATA IMPORT
# 2.1 IMPORT DATA: for earthquakes, temperature, air quality, and rainfall

earthquakes <-read.csv('earthquakes.csv')

temperature <- read.csv('average annual temperature by state.csv')

airquality <- read.csv('lawa-air-quality-dataset_july-2021.csv')

rainfall_full <- read.csv('Rainfall daily_data.csv')

mental_health <- read.csv('mh_regional.csv')

# 2.2 IMPORT DATA: for time series (see section 5)

# 3. DATA WRANGLING

rainfall <- rainfall_full %>% filter(date >= '2016-01-01') # adjust data range for rainfall to fit 2016-2021

rainfall$date<- as.Date(rainfall$date)

remove(rainfall_full) #remove raw rainfall data set

airquality <- airquality[c(1:134066),] #remove empty row from air quality data

airquality<- airquality %>% filter(Indicator != 'PM25') #remove PM25 data

airquality$Date <- dmy(airquality$Date) #change Date to date format

temperature <- temperature %>% filter(statistic == 'Average', season == 'Annual') #reduce temperature data to usable entries

mental_health <- mental_health %>% filter(Region != "Total") #remove totals from mental_health data

#Adjust spelling of Regions in mental_health to fit uniform standard
mental_health <- mental_health %>%
  mutate(Region = case_when(
    Region == "Hawkes Bay" ~ "Hawke's Bay",
    Region == "Manawatu-Whanganui" ~ "Manawatū-Whanganui",
    Region == "Southern " ~ "Southern",
    TRUE ~ Region
  ))

#Adjust regions in air quality to match selection predicted by health data:
unique(airquality[c("Region")])

airquality <- airquality %>%
  mutate(Region = case_when(
    Region == "Auckland region" ~ "Auckland",
    Region == "Bay of Plenty region" ~ "Central North",
    Region == "Canterbury region" ~ "Canterbury",
    Region == "Gisborne region" ~ "Gisborne",
    Region == "Hawke's Bay region" ~ "Hawke's Bay",
    Region == "Manawatū-Whanganui region" ~ "Manawatū-Whanganui",
    Region == "Marlborough region" ~ "Upper South",
    Region == "Nelson region" ~ "Upper South",
    Region == "Northland region" ~ "Northland",
    Region == "Otago region" ~ "Southern",
    Region == "Southland region" ~ "Southern",
    Region == "Tasman region" ~ "Upper South",
    Region == "Waikato region" ~ "Central North",
    Region == "Wellington Region" ~ "Wellington",
    Region == "West Coast region" ~"West Coast",
  ))

#Assign regions in rainfall based on location name to match selection predicted by health data:

unique(rainfall[c("location_name")])

rainfall <- rainfall %>%
  mutate(Region = case_when(
    location_name == "Auckland" ~ "Auckland",
    location_name == "Blenheim" ~ "Upper South",
    location_name == "Christchurch" ~ "Canterbury",
    location_name == "Dannevirke" ~ "Manawatū-Whanganui",
    location_name == "Dunedin" ~ "Southern",
    location_name == "Gisborne" ~ "Gisborne",
    location_name == "Gore" ~ "Southern",
    location_name == "Hamilton" ~ "Central North",
    location_name == "Hokitika" ~ "West Coast",
    location_name == "Invercargill" ~ "Southern",
    location_name == "Kerikeri" ~ "Northland",
    location_name == "Lake Tekapo" ~ "Canterbury",
    location_name == "Masterton" ~ "Wellington",
    location_name == "Milford Sound" ~ "Southern",
    location_name == "Napier" ~ "Hawke's Bay",
    location_name == "Nelson" ~ "Upper South",
    location_name == "New Plymouth" ~ "Taranaki",
    location_name == "Queenstown" ~ "Southern",
    location_name == "Reefton" ~ "West Coast",
    location_name == "Rotorua" ~ "Central North",
    location_name == "Tara Hills" ~ "Southern",
    location_name == "Taumarunui" ~ "Manawatū-Whanganui",
    location_name == "Taupo" ~ "Central North",
    location_name == "Tauranga" ~ "Central North",
    location_name == "Timaru" ~ "Canterbury",
    location_name == "Waiouru" ~ "Manawatū-Whanganui",
    location_name == "Wellington" ~ "Wellington",
    location_name == "Whanganui" ~ "Manawatū-Whanganui",
    location_name == "Whangaparaoa" ~ "Auckland",
    location_name == "Whangarei" ~ "Northland",
  ))

#Assign regions in temperature based on site name to match selection predicted by health data:
unique(temperature[c("site")])

temperature <- temperature %>%
  mutate(Region = case_when(
    site == "Auckland" ~ "Auckland",
    site == "Auckland" ~ "Auckland",
    site == "Blenheim" ~ "Upper South",
    site == "Christchurch" ~ "Canterbury",
    site == "Dannevirke" ~ "Manawatū-Whanganui",
    site == "Dunedin" ~ "Southern",
    site == "Gisborne" ~ "Gisborne",
    site == "Gore" ~ "Southern",
    site == "Hamilton" ~ "Central North",
    site == "Hokitika" ~ "West Coast",
    site == "Invercargill" ~ "Southern",
    site == "Kerikeri" ~ "Northland",
    site == "Lake Tekapo" ~ "Canterbury",
    site == "Masterton" ~ "Wellington",
    site == "Milford Sound" ~ "Southern",
    site == "Napier" ~ "Hawke's Bay",
    site == "Nelson" ~ "Upper South",
    site == "New Plymouth" ~ "Taranaki",
    site == "Queenstown" ~ "Southern",
    site == "Reefton" ~ "West Coast",
    site == "Rotorua" ~ "Central North",
    site == "Tara Hills" ~ "Southern",
    site == "Taumarunui" ~ "Manawatū-Whanganui",
    site == "Taupo" ~ "Central North",
    site == "Tauranga" ~ "Central North",
    site == "Timaru" ~ "Canterbury",
    site == "Waiouru" ~ "Manawatū-Whanganui",
    site == "Wellington" ~ "Wellington",
    site == "Whanganui" ~ "Manawatū-Whanganui",
    site == "Whangaparaoa" ~ "Auckland",
    site == "Whangarei" ~ "Northland",
    TRUE ~ site))


#Adjust earthquakes to magnitude >= 3.5 & eliminating earthquakes out of NZ interest area

earthquakes<- earthquakes %>% filter(magnitude >= '3.5', area_interest == "Yes")

#Assigning regions to earthquakes via reverse geo-coding:

earthquakes$Region = NA #Add column for Region

#Using Google Maps API to reverse geocode the state of the earthquakes using longitude and latitude
# NOTE: This code does not work for the assignment hand in, as I restricted the Google API call limit.
earthquakes[,c(25)] <- revgeo(longitude = earthquakes[,c(7)],
                              latitude = earthquakes[,c(8)], 
                              provider = "google",
                              API = "AIzaSyCp1eOkxM9VG3YI4ZVovr0RaVcGLVSEr14",
                              output = "hash",
                              item = "state"
)

#USE THIS CODE AS PROOF OF REVERSE GEOCODING FUNCTION

earthquakes[45,c(25)] <- revgeo(longitude = earthquakes[45,c(7)],
                              latitude = earthquakes[45,c(8)], 
                              provider = "google",
                              API = "AIzaSyCp1eOkxM9VG3YI4ZVovr0RaVcGLVSEr14",
                              output = "hash",
                              item = "state"
)

View(earthquakes[c(45),]) #Check Region (column 25) to see that the geocoding returned Canterbury for the example

#Adjust Regions for selection set by mental health data
earthquakes <- earthquakes %>%
  mutate(Region = case_when(
    Region == "Southland" ~ "Southern",
    Region == "Southland Region" ~ "Southern",
    Region == "Otago Region" ~ "Southern",
    Region == "Otago" ~ "Southern",
    Region == "West Coast Region" ~ "West Coast",
    Region == "Canterbury Region" ~ "Canterbury",
    Region == "Tasman Region" ~ "Upper South",
    Region == "Tasman" ~ "Upper South",
    Region == "Marlborough" ~ "Upper South",
    Region == "Marlborough Region" ~ "Upper South",
    Region == "Nelson Region" ~ "Upper South",
    Region == "Nelson" ~ "Upper South",
    Region == "Taranaki Region" ~ "Taranaki",
    Region == "Northland Region" ~ "Northland",
    Region == "Waikato" ~ "Central North",
    Region == "Wellington Region" ~ "Wellington",
    Region == "Waikato Region" ~ "Central North",
    Region == "Manawatu-Wanganui Region" ~ "Manawatū-Whanganui",
    Region == "Bay of Plenty" ~ "Central North",
    Region == "Hawke's Bay Region" ~ "Hawke's Bay",
    Region == "Gisborne Region" ~ "Gisborne",
    Region == "Westcoast" ~ "West Coast",
    Region == "Manawatu-Wanganui" ~ "Manawatū-Whanganui",
    TRUE ~ Region))

#NOTE FOR GRADER: As the API is limited we just wanted to demonstrate how we reverse-geocoded the earthquakes. 
#NOTE FOR GRADER: We will now import the wrangled earthquake data to work with from here on.

earthquakes <- read.csv('earthquakes_wrg.csv')

earthquakes$Date <- as.Date(earthquakes$origintime) #change origin time to Date

earthquakes <- earthquakes %>% filter(Date >= '2016-01-01')

#Reducing the data frames to the necessary columns only:
airquality <- airquality[,c(1,11,12)]
earthquakes <- earthquakes[,c(8,11,12)]
rainfall <- rainfall[,c(2,3,6)]
temperature <- temperature[,c(4,6,14)]

#Combining mental health data and environmental averages into one data frame for exploratory analysis
#Air quality & mental health
airquality$Value <- as.numeric(airquality$Value) #setting Value to numeric for averaging

airquality_avg <- aggregate(airquality$Value, 
                            by = list(Date = year(airquality$Date),
                                      Region = airquality$Region),
                            data = airquality,
                            FUN = mean)
#merge mental health data with average air quality on Region and Year without dropping any mental health data
mh_environment <- merge(mental_health, 
                        airquality_avg, 
                        by.x = c("Region", "Year"), 
                        by.y = c("Region", "Date"), 
                        all.x = TRUE)

remove(airquality_avg) #drop the average air quality data frame as no longer needed

#Earthquakes Maximums & Mental Health
earthquakes$magnitude <- as.numeric(earthquakes$magnitude) #setting Value to numeric for averaging

earthquakes_max <- aggregate(earthquakes$magnitude, 
                            by = list(Date = year(earthquakes$Date),
                                      Region = earthquakes$Region),
                            FUN = max)

#merge mental health data with max earthquakes on Region and Year without dropping any mental health data
mh_environment <- merge(mh_environment, 
                        earthquakes_max, 
                        by.x = c("Region", "Year"), 
                        by.y = c("Region", "Date"), 
                        all.x = TRUE)

remove(earthquakes_max) #drop the max earthquake data frame as no longer needed

#Average Rainfall & Mental Health
rainfall_avg <- aggregate(rainfall$rainfall, 
                            by = list(Date = year(rainfall$date),
                                      Region = rainfall$Region),
                            data = rainfall,
                            FUN = mean)

#merge mental health data with average rainfall on Region and Year without dropping any mental health data
mh_environment <- merge(mh_environment, 
                        rainfall_avg, 
                        by.x = c("Region", "Year"), 
                        by.y = c("Region", "Date"), 
                        all.x = TRUE)

remove(rainfall_avg)

#Average Temperature & Mental Health
temperature_avg <- aggregate(temperature$temperature, 
                          by = list(Date = temperature$year,
                                    Region = temperature$Region),
                          data = temperature,
                          FUN = mean)

#merge mental health data with average rainfall on Region and Year without dropping any mental health data
mh_environment <- merge(mh_environment, 
                        temperature_avg, 
                        by.x = c("Region", "Year"), 
                        by.y = c("Region", "Date"), 
                        all.x = TRUE)

remove(temperature_avg)

#Rename columns in mh_environment to reflect the values
colnames(mh_environment) <- c("Region", 
                              "Year",
                              "nmb_seen_DHB",
                              "suicide_deaths", 
                              "Population",
                              "rate_seen_DHB",
                              "rate_suicide",
                              "airquality_avg",
                              "earthquake_max",
                              "rainfall_avg",
                              "temperature_avg")

#4 EXPLORATORY ANALYSIS
#4.1 EXPLORE correlation between mental health & environment
#build linear model to test for correlations between rate of population seen by DHB and our environmental factors
lm_rate_seen_DHB <- lm(rate_seen_DHB~ airquality_avg+earthquake_max+rainfall_avg+temperature_avg,
                       data = mh_environment)

summary(lm_rate_seen_DHB) #Air quality, rainfall and temperature show correlation; earthquake doesn't

#build linear model to test for correlations between suicide rate and our environmental factors
lm_suicide_rate <- lm(rate_suicide~ airquality_avg+earthquake_max+rainfall_avg+temperature_avg,
                      data = mh_environment)

summary(lm_suicide_rate) #Only Rainfall shows the weakest of correlations -> let's explore further

#Plot exploratory analysis graphs:
#AIRQUALITY & Rate seen by DHB
mental_airquality <- ggplot(data = mh_environment)+
  geom_line(aes(x=Year, y= rate_seen_DHB*10, colour = Region, linetype = "Percentage of population seen by DHB"))+
  geom_line(aes(x=Year, y=airquality_avg, colour = Region, linetype = "Average Airpollution by PM10"))+
  scale_linetype_manual(NULL, values = c("Percentage of population seen by DHB" = "solid", "Average Airpollution by PM10" = "dashed"))+
  scale_shape_manual(NULL, values =16)+
  scale_y_continuous(name = "Percentage of population seen by DHB",
                     breaks = c(10,20,30,40,50),
                     labels=c("10" = "1",  
                              "20" = "2",
                              "30" = "3",
                              "40" = "4",
                              "50" = "5"),
                     sec.axis = sec_axis (trans=~., name = "Average Airquality by PM10"))+
  facet_wrap(Region~., scales = "free_y")

print(mental_airquality) 

#4.2 RAINFALL & Suicide Rate to further explore the weak correlation shown by the linear model
#Plot (Double y-axis plot) average rainfall against population rate of suicide deaths, faceted by Region:
suicide_rainfall <- ggplot(data = mh_environment)+
  geom_line(aes(x=Year, y= rate_suicide, colour = Region, linetype = "Percentage of suicide deaths by Population"))+
  geom_line(aes(x=Year, y=rainfall_avg/100, colour = Region, linetype = "Average Daily Rainfall"))+
  scale_linetype_manual(NULL, values = c("Percentage of suicide deaths by Population" = "solid", "Average Daily Rainfall" = "dashed"))+
  scale_shape_manual(NULL, values =16)+
  scale_y_continuous(name = "Percentage of suicide deaths by Population", 
                     sec.axis = sec_axis (trans=~., 
                                          name = "Average Daily Rainfall",
                                          breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08),
                                          labels=c("0" = "0",  
                                                   "0.01" = "1",
                                                   "0.02" = "2",
                                                   "0.03" = "3",
                                                   "0.04" = "4",
                                                   "0.05" = "5",
                                                   "0.06" = "6",
                                                   "0.07" = "7",
                                                   "0.08" = "8")))+
  facet_wrap(Region~., scales = "free_y")

print(suicide_rainfall)

#4.3 TEMPERARTURE & DHB rate seen
#Plot (Double y-axis plot) average annual temperature against population rate seen by DHB for mental health reasons, faceted by Region:
mental_temperature <- ggplot(data = mh_environment)+
  geom_line(aes(x=Year, y= rate_seen_DHB, colour = Region, linetype = "Percentage of Population seen by DHB"))+
  geom_line(aes(x=Year, y=temperature_avg/4, colour = Region, linetype = "Average Annual Temperature"))+
  scale_linetype_manual(NULL, values = c("Percentage of Population seen by DHB" = "solid", "Average Annual Temperature" = "dashed"))+
  scale_shape_manual(NULL, values =16)+
  scale_y_continuous(name = "Percentage of population seen by DHB service",
                     sec.axis = sec_axis (trans=~., 
                                          name = "Average Annual Temperature",
                                          breaks = c(2,2.2,2.4,2.6,2.8,3,3.2,3.4,3.6,3.8,4,4.2),
                                          labels=c("2" = "8", 
                                                   "2.2" = "8.8", 
                                                   "2.4" = "9.6",
                                                   "2.6" = "10.4",
                                                   "2.8" = "11.2",
                                                   "3" = "12",
                                                   "3.2" = "12.8",
                                                   "3.4" = "13.6",
                                                   "3.6" = "14.4",
                                                   "3.8" = "15.2",
                                                   "4" = "16",
                                                   "4.2" = "16.8")))+
  facet_wrap(Region~., scales = "free_y")

print(mental_temperature)

#EARTHQUAKES & rate seen by DHB to further test the correlation of earthquakes
mental_earthquakes <- ggplot(data = mh_environment)+
  geom_line(aes(x=Year, y= rate_seen_DHB, colour = Region, linetype = "Percentage of Population seen by DHB"))+
  geom_line(aes(x=Year, y=earthquake_max, colour = Region, linetype = "Maximum Earthquake Magnitude"))+
  scale_linetype_manual(NULL, values = c("Percentage of Population seen by DHB" = "solid", "Maximum Earthquake Magnitude" = "dashed"))+
  scale_shape_manual(NULL, values =16)+
  scale_y_continuous(name = "Percentage of population seen by DHB service",
                     sec.axis = sec_axis (trans=~., 
                                          name = "Maximum Earthquake Magnitude"))+
  facet_wrap(Region~., scales = "free_y")

print(mental_earthquakes)

#5. MODEL EXPLORATION: RANDOM FOREST

set.seed(1234) #set seed to reproduce results

mh_environment_rf <- mh_environment #create duplicate mh_environment for random forest experiments

mh_environment_rf$Region<- as.factor(mh_environment_rf$Region)

# Reduce attributes to necessary only for Random forest between numbers seen rate and environmental factors. 
mh_environment_rf <- mh_environment_rf %>%
  select(Region, rate_seen_DHB, airquality_avg, earthquake_max, rainfall_avg, temperature_avg)

# Split the data set into train data (70%) and test data (30%)
rf_train = sample (1:nrow(mh_environment_rf), round(0.7*nrow(mh_environment_rf)))
rf_test = mh_environment_rf[-rf_train, ]

# impute values for the NAs in the dataset with rfImpute
rf.mhr1<- rfImpute(rate_seen_DHB~.,
                   data = mh_environment_rf,
                   iter = 4)
# -> low overall MSE value; however most likely over fitted due to small data set

#test estimated errors for out of bag
bag.mhr1 = randomForest(rate_seen_DHB~.,
                        data = rf.mhr1,
                        proximity=T,
                        importance=T)

#Determine best number of trees for forest
plot(bag.mhr1,
     main = "Bagging: OOB Estimate of Error Rate")

which.min(bag.mhr1$mse) #determine best tree count (changes every time run)

# return n trees 
rf.mhr.ntree = randomForest(rate_seen_DHB~.,
                            data = mh_environment_rf,
                            subset = rf_train,
                            proximity=T,
                            importance=T,
                            na.action=na.roughfix,
                            ntree = 496,
                            mtry = ncol(mh_environment_rf)-1)

#Plot variable importance for random forest (mean decrease accuracy when variable removed)
varImpPlot(rf.mhr.ntree, main = "Variable Importance - Clients Seen By DHB Service Rate and Environmental Factors")

#RANDOM FOREST between suicide rate and environment:

# Reduce attributes to necessary only for Random forest between suicide rate and environmental factors. 
mh_environment_rf2 <- mh_environment %>%
  select(Region, rate_suicide, airquality_avg, earthquake_max, rainfall_avg, temperature_avg)

mh_environment_rf2$Region<- as.factor(mh_environment_rf2$Region)

# Split the data set into train data (70%) and test data (30%)
rf2_train = sample (1:nrow(mh_environment_rf2), round(0.7*nrow(mh_environment_rf2)))
rf2_test = mh_environment_rf2[-rf2_train, ]

# OOB Estimate of Error
rf2.sr1<- rfImpute(rate_suicide~.,
                   data = mh_environment_rf2,
                   iter = 4)
# -> low overall MSE value; however most likely over fitted due to small data set

#test estimated errors for out of bag
bag.sr1 = randomForest(rate_suicide~.,
                        data = rf2.sr1,
                        proximity=T,
                        importance=T)

#Determine best number of trees for forest
plot(bag.sr1,
     main = "Bagging: OOB Estimate of Error Rate")

which.min(bag.mhr1$mse) #determine best tree count (changes every time run)

# return n trees 
rf.sr1.ntree = randomForest(rate_suicide~.,
                            data = mh_environment_rf2,
                            subset = rf2_train,
                            proximity=T,
                            importance=T,
                            na.action=na.roughfix,
                            ntree = 496,
                            mtry = ncol(mh_environment_rf2)-1)

#Plot variable importance for random forest (mean decrease accuracy when variable removed)
varImpPlot(rf.sr1.ntree, main = "Variable Importance - Suicide Rate and Environmental Factors")

#RANDOM FOREST NOTE: While the errors seem to be small there is a high chance of over fitting due to small data set
# We decided to abandon random forests here and focus on other methods which are better suited for small data sets

#6. TIME SERIES & ARIMA PREDICTION
#NOTE: Due to the exploratory research we were able to determine specific regions to predict:
#This way we could go out and search for more data, allowing us to use more detailed data per region for these predictions, hence we import more detailed data sets.

#6.1 Hawke's Bay Rainfall
rain_hawk_full <- read.csv("Rain_Hawkes.csv")

rain_hawk_full$Date <- rain_hawk_full$Date.NZST. #rename date column to simplify
rain_hawk_full$Rain <- rain_hawk_full$Amount.mm. #rename temperature column to simplify

rain_hawk_full <- rain_hawk_full[,c(1,10,11)] #select variables of interest

rain_hawk_full$Date <- ymd(rain_hawk_full$Date) #Convert date attribute to date format

#combines station data across the whole region for every day
rain_hawk <- aggregate(rain_hawk_full$Rain, 
                       by = list(Date = rain_hawk_full$Date,
                                 Region = rain_hawk_full$Region),
                       data = rain_hawk_full,
                       FUN = mean)

rain_hawk$Date <- ymd(rain_hawk$Date) #set Date to date format

remove(rain_hawk_full) #remove raw data set

#TIMESERIES 
rain_hawk_ts <- ts(rain_hawk[,3], start = c(2016,1), frequency = 365.25) #creates time series

plot(rain_hawk_ts,
     ylab = 'Daily Rain in mm',
     main = "Time series for daily rain amount in Hawke's Bay 2016-2020")

rain_hawk_ts_components <- decompose(rain_hawk_ts)

plot(rain_hawk_ts_components)

#Box-Pierce or Ljung-Box test statistic for Auto-correlation
Box.test(rain_hawk_ts, lag=10, type = 'Ljung-Box')

#ARIMA Prediction for Rainfall in Hawke's Bay

rain_hawk_forecast <- auto.arima(rain_hawk_ts) #create  ARIMA model on time series

checkresiduals(rain_hawk_forecast) #checks for residuals

summary(rain_hawk_forecast) #check summary for evaluation 

rain_hawk_forecast_2y <- forecast(rain_hawk_forecast, h = 730) #predict average rainfall for the following two years

rain_hawk_forecast_plot <- plot(forecast(rain_hawk_forecast),
                                xlab = 'Year',
                                ylab = 'Daily Rainfall in mm',
                                main = "Average Daily Rainfall in mm for Hawke's Bay 2016 - 2022")

#Extract annual mean for predicted data
print(paste('The Predicted 2022 Value is', mean(rain_hawk_forecast_2y$mean[1:365]))) 
print(paste('The Predicted 2021 Value is', mean(rain_hawk_forecast_2y$mean[366:730])))

#Extract annual mean for observed data
print(paste('The Observed 2016 Value is', mean(rain_hawk_forecast_2y$fitted[1:365]))) 
print(paste('The Observed 2017 Value is', mean(rain_hawk_forecast_2y$fitted[366:730])))
print(paste('The Observed 2018 Value is', mean(rain_hawk_forecast_2y$fitted[731:1096]))) 
print(paste('The Observed 2019 Value is', mean(rain_hawk_forecast_2y$fitted[1097:1462])))
print(paste('The Observed 2020 Value is', mean(rain_hawk_forecast_2y$fitted[1463:1827]))) 

#6.2 ARIMA Prediction for Air Quality in Wellington
#Combine air quality and finding the average as there are more than 2 values on the same date
airquality_well <- aggregate(airquality$Value,
                        by=list(Date = airquality$Date, 
                                Region=airquality$Region),
                        data=airquality,FUN=mean)

airquality_well <- airquality_well %>% filter(Region == "Wellington") #Only air quality for Wellington 

#time series air quality
airquality_well_ts = ts(airquality_well[,3],
                                   start = c(2016,1),
                                   frequency = 365.25)

plot(airquality_well_ts,
     ylab = 'PM10 Air Pollution',
     main = "Time Series plot for PM10 airquality in Wellington from 2016 to 2021")

#Box-Pierce or Ljung-Box test statistic for Auto-correlation
Box.test(airquality_well_ts, lag=10, type = 'Ljung-Box')

#Forecast Wellington air quality
airquality_well_forecast <- auto.arima(airquality_well_ts) #build ARIMA model

airquality_well_forecast2y <- forecast (airquality_well_forecast, h=730) #Forecast 2 years

plot(airquality_well_forecast2y,
     ylab = "Pollutant Count by PM10",
     xlab = "Year",
     main = "Average Predicted Air Pollution (PM10) for Wellington 2016-2022")

#Check residuals
checkresiduals(airquality_well_forecast)

#Summary of data
summary(airquality_well_forecast)

#Extract mean values for 2 y
print(paste('The Predicted 2021 Value is', mean(airquality_well_forecast2y$mean[1:365])))
print(paste('The Predicted 2022 Value is', mean(airquality_well_forecast2y$mean[366:730])))

#Extract annual mean for observed data
print(paste('The Observed 2016 Value is', mean(airquality_well_forecast2y$fitted[1:365]))) 
print(paste('The Observed 2017 Value is', mean(airquality_well_forecast2y$fitted[366:730])))
print(paste('The Observed 2018 Value is', mean(airquality_well_forecast2y$fitted[731:1096]))) 
print(paste('The Observed 2019 Value is', mean(airquality_well_forecast2y$fitted[1097:1462])))
print(paste('The Observed 2020 Value is', mean(airquality_well_forecast2y$fitted[1463:1826]))) 

#6.3 ARIMA Prediction for Temperature in Canterbury
temp_cant_full <- read.csv("Temp_Canter.csv") #import the detailed temperature data for Canterbury

temp_cant_full$Date <- temp_cant_full$Date.NZST.#adjust column name
temp_cant_full$Temp <- temp_cant_full$Tair.C.#adjust columns date

temp_cant_full <- temp_cant_full[,c(1,9,10)] #selects columns of interest

temp_cant_full$Date <- ymd(temp_cant_full$Date) #sets date format for time series

temp_cant_full$Date <- as.yearmon(temp_cant_full$Date, "%Y %m") #reduces date to month and year, as daily date didn't work with the ARIMA (kept freezing)

#Combine data across all measuring stations for a unified average temperature per day.
temp_cant <- aggregate(temp_cant_full$Temp,
                       by = list(Date = temp_cant_full$Date,
                                 Region = temp_cant_full$Region),
                       data = temp_cant_full,
                       FUN = mean)

remove(temp_cant_full) #remove raq data as no longer needed

temp_cant_ts <- ts(temp_cant[,3], start = c(2016,1), frequency = 12) #create time series for Canterbury

#Box-Pierce or Ljung-Box test statistic for Auto-correlation
Box.test(temp_cant_ts, lag=10, type = 'Ljung-Box')

temp_cant_forecast <- auto.arima(temp_cant_ts) #create ARIMA for prediction

temp_cant_forecast2y <- forecast(temp_cant_forecast, h = 24) #predict next two years (24 months)

#Check residuals
checkresiduals(temp_cant_forecast)

#check summary of models
summary(temp_cant_forecast)

#Plot predicted temperature graph
plot(temp_cant_forecast2y,
     ylab = "Average Temperature in C",
     xlab = "Year",
     main = "Average Predicted Temperature for Canterbury 2016-2022")

#Extract annual mean for predicted data
print(paste('The Predicted 2021 Value is', mean(temp_cant_forecast2y$mean[1:12]))) 
print(paste('The Predicted 2022 Value is', mean(temp_cant_forecast2y$mean[13:24])))

#Extract annual mean for observed data
print(paste('The Observed 2016 Value is', mean(temp_cant_fit_2y$fitted[1:12]))) 
print(paste('The Observed 2017 Value is', mean(temp_cant_fit_2y$fitted[13:24])))
print(paste('The Observed 2018 Value is', mean(temp_cant_fit_2y$fitted[25:36]))) 
print(paste('The Observed 2019 Value is', mean(temp_cant_fit_2y$fitted[37:48])))
print(paste('The Observed 2020 Value is', mean(temp_cant_fit_2y$fitted[49:60])))

#7.PREDICTIONS with RIDGE REGRESSION
#Note: After research we decided to use ridge regression instead of the previously trialed random forests, as it is better suited for small data sets.

#duplicate mh_environmental to prepare for ridge regression analysis
mh_environment_rr <- mh_environment %>% select(rate_seen_DHB, airquality_avg, earthquake_max, rainfall_avg, temperature_avg)
mh_environment_rr2 <- mh_environment %>% select(rate_suicide, airquality_avg, earthquake_max, rainfall_avg, temperature_avg)

mh_environment_rr = na.omit(mh_environment_rr) #omitting empty values
mh_environment_rr2 = na.omit(mh_environment_rr2) #omitting empty values

set.seed(250)

#RIDGE REGRESSION & Rate of Population seen by DHB:

# splitting data into train and test:
dt = sort(sample(nrow(mh_environment_rr), nrow(mh_environment_rr)*.7))
rr_train<-mh_environment_rr[dt,]
rr_test<-mh_environment_rr[-dt,]

remove(dt)

#setting independent and dependent variables (test/train):
x_rr_train<-data.matrix(rr_train[, c(2:5)])
x_rr_test<-data.matrix(rr_test[, c(2:5)])
y_rr_train<-rr_train[,1]
y_rr_test<-rr_test[,1]

#fitting the model via penalised maximum likelihood (lambda):
rr_lambda_seq <-10^seq(2, -2, by = -.1)
rr_fit <-glmnet(x_rr_train, y_rr_train, alpha = 0, lambda = rr_lambda_seq)

summary(rr_fit)

#Finding the minimum lambda (best fit) and turning it into a variable for later use:
rr_ridge <-cv.glmnet(x_rr_train, y_rr_train, alpha = 0, lambda = rr_lambda_seq)
rr_best_lambda <- rr_ridge$lambda.min

rr_best_fit <-rr_ridge$glmnet.fit
head(rr_best_fit)

#Using the training data to create the best ridge for the model:
rr_best_ridge<-glmnet(x_rr_train, y_rr_train, alpha = 0, lambda = rr_lambda_seq)

#coefficients:
coef(rr_best_ridge)

#creating evaluation function which measures RMSE and R squared for prediction results:
rr_eval_results<-function(true, predicted, mh_environment_rr) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(mh_environment_rr))
  data.frame(
    RMSE=RMSE,
    Rsquare=R_square
  )
}

#Creating linear model and applying it to a graphable ridge:
rr_amod0 <- lm(rate_seen_DHB~ airquality_avg + earthquake_max + rainfall_avg +temperature_avg, data=mh_environment_rr)
rr_y <- mh_environment_rr[,1]
rr_X0 <- model.matrix(rr_amod0)[,-1]
rr_alambda <- c(0, 0.0005, 0.001, 0.002, 0.005, 0.01)
rr_aridge0 <- ridge(rr_y, rr_X0, lambda=rr_alambda)

#Pair plot built from the ridge regression model, which shows correlation between variables:
pairs(rr_aridge0, radius=0.2)

#Creating a biplot which shows the variables from a PCA perspective to visualise correlation to the output:
rr_lridge <- ridge(y_rr_train, x_rr_train, lambda=rr_alambda)
rr_plridge <- pca.ridge(rr_lridge)
biplot(rr_plridge)

#PREDICTIONS FOR WELLINGTON, CANTERBURY, HAWKES BAY & POPULATION RATE SEEN BY DHB
#NOTE: We created the three following csv files in Excel with the data provided from our time series predictions.
#NOTE: They are regionalised copies of the mh_environment.csv with the added environmental predictions.

mental1 <-read.csv("mh_wellington.csv")
mental2 <-read.csv("mh_canterbury.csv")
mental3 <-read.csv("mh_hawkesbay.csv")

#Dropping columns (optional):
mental1 <- mental1 %>% select(3:6,12)

#WELLINGTON: Setting independent variables:
x_mental1<-data.matrix(mental1[,c(1,4)])
y_mental1<-mental1[,2]

#CANTERBURY: Setting independent variables:
x_mental2<-data.matrix(mental2[,c(1,4)])
y_mental2<-mental2[,2]

#Hawke's Bay: Setting independent variables:
x_mental3<-data.matrix(mental3[,c(1,4)])
y_mental3<-mental3[,2]


#Fitting the models:
lambda_seq<-10^seq(2, -2, by = -.1)

fit_mental1 <- glmnet(x_mental1, y_mental1, alpha = 0, lambda = lambda_seq)
fit_mental2 <- glmnet(x_mental2, y_mental2, alpha = 0, lambda = lambda_seq)
fit_mental3 <- glmnet(x_mental3, y_mental3, alpha = 0, lambda = lambda_seq)


#Best model for WELLINGTON:
ridge_mental1 <- cv.glmnet(x_mental1, y_mental1, alpha = 0, lambda = lambda_seq)
best_lambda_mental1 <- ridge_mental1$lambda.min
best_fit_mental1 <-ridge_mental1$glmnet.fit
head(best_fit_mental1)

best_ridge_mental1 <- glmnet(x_mental1, y_mental1, alpha = 0, lambda = best_lambda_mental1)

#Best model for CANTERBURY:
ridge_mental2 <- cv.glmnet(x_mental2, y_mental2, alpha = 0, lambda = lambda_seq)
best_lambda_mental2 <- ridge_mental2$lambda.min
best_fit_mental2 <-ridge_mental2$glmnet.fit
head(best_fit_mental2)

best_ridge_mental2 <- glmnet(x_mental2, y_mental2, alpha = 0, lambda = best_lambda_mental2)

#Best model for HAWKE's BAY:
ridge_mental3 <- cv.glmnet(x_mental3, y_mental3, alpha = 0, lambda = lambda_seq)
best_lambda_mental3 <- ridge_mental3$lambda.min
best_fit_mental3 <-ridge_mental3$glmnet.fit
head(best_fit_mental3)

best_ridge_mental3 <- glmnet(x_mental3, y_mental3, alpha = 0, lambda = best_lambda_mental3)

#coefficients:
coef(best_ridge_mental1)
coef(best_ridge_mental2)
coef(best_ridge_mental3)

#creating regionally-specific evaluation functions:
eval_results_mental1 <-function(true, predicted, mental1) {
  SSE<- sum((predicted - true)^2)
  SST<- sum((true - mean(true))^2)
  R_square<- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(mental1))
  
  data.frame(
    RMSE=RMSE,
    Rsquare=R_square
  )
}

eval_results_mental2 <-function(true, predicted, mental2) {
  SSE<- sum((predicted - true)^2)
  SST<- sum((true - mean(true))^2)
  R_square<- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(mental2))
  
  data.frame(
    RMSE=RMSE,
    Rsquare=R_square
  )
}

eval_results_mental3 <-function(true, predicted, mental3) {
  SSE<- sum((predicted - true)^2)
  SST<- sum((true - mean(true))^2)
  R_square<- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(mental3))
  
  data.frame(
    RMSE=RMSE,
    Rsquare=R_square
  )
}

#predictions for Wellington:
predictions_mental1 <- predict(best_ridge_mental1, s=best_lambda_mental1, newx=x_mental1)
eval_results_mental1(y_mental1, predictions_mental1, x_mental1)

predictions_mental1 #shows results for DHB visits by population rate in Wellington for 2016:2022

print(paste('The Predicted rate of Wellington population being seen by DHB in 2021 Value is', predictions_mental1[6])) #prints 2021 prediction
print(paste('The Predicted rate of Wellington population being seen by DHB in 2022 Value is', predictions_mental1[7])) #prints 2022 prediction

#predictions for Canterbury:
predictions_mental2 <- predict(best_ridge_mental2, s=best_lambda_mental2, newx=x_mental2)
eval_results_mental2(y_mental2, predictions_mental2, x_mental2)

predictions_mental2 #shows results for DHB visits by population rate in Canterbury for 2016:2022

print(paste('The Predicted rate of Canterbury population being seen by DHB in 2021 Value is', predictions_mental2[6])) #prints 2021 prediction
print(paste('The Predicted rate of Canterbury population being seen by DHB in 2022 Value is', predictions_mental2[7])) #prints 2022 prediction


#predictions for Hawke's Bay:
predictions_mental3 <- predict(best_ridge_mental3, s=best_lambda_mental3, newx=x_mental3)
eval_results_mental3(y_mental3, predictions_mental3, x_mental3)

predictions_mental3 #shows results for DHB visits by population rate in Hawke's Bay for 2016:2022

print(paste('The Predicted rate of Hawkes Bay population being seen by DHB in 2021 Value is', predictions_mental3[6])) #prints 2021 prediction
print(paste('The Predicted rate of Hawkes Bay population being seen by DHB in 2022 Value is', predictions_mental3[7])) #prints 2022 prediction

#NOTE: To predict suicide rate we only have to change the predictor variable from DHB visits to suicide rate and rerun the models. 

#END OF CODE 
