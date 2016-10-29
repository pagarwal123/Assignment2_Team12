library(weatherData)
library(dplyr)
library(tidyr)
library(chron)
rawData1 <- read.csv("CopyOfrawData2.csv")
#Filtering only the first kwh entries for each date
rawData1 <- filter(rawData1, grepl('kWh', Channel))
j<-0
last <- ncol(rawData1)
i<-5

#Adding hourly data of each date i.e. Kwh is aggregated hourly. Sum of 12 observations (5 min intervals rolled up to hourly)
library(lazyeval)
f = function(df, n, new_col_name) {
  mutate_call = lazyeval::interp(~ rowSums(df[, c(n:(n+11))]) )
  rawData1 %>% mutate_(.dots = setNames(list(mutate_call), new_col_name))
}

while(i <= (last-11)){
  newname <- j
  rawData1 <- f(df = rawData1, n = i,new_col_name = newname)
  i <- i+12
  j<-j+1
}
rawData1<-rawData1[-(5:last)]
rawData1<-rawData1[-(3:4)]

#stack data
rawData1 <- rawData1 %>% gather(hour, kWh, -(1:2))

#Convert to date format
rawData1 <- rawData1 %>% mutate(Date = as.Date(rawData1$Date , "%m/%d/%Y"))

#ordering the data with respect to date
rawData1 <- arrange(rawData1, Date)

#creating year, month, day, day of week, weekday columns
#month 1-12 => Jan-Dec – Derived from dates
#day 1-31 – Derived from dates
#Dayof Week 0-6 –Sun-Sat – Derived from dates
#Weekday 1- Yes 0- No – Derived from dates
rawData1 <- rawData1 %>% mutate(year = as.numeric(format(rawData1$Date, format = "%Y")))
rawData1 <- rawData1 %>% mutate(month = as.numeric(format(rawData1$Date, format = "%m")))
rawData1 <- rawData1 %>% mutate(day = as.numeric(format(rawData1$Date, format = "%d")))
rawData1 <- rawData1 %>% mutate(dayOfWeek = as.numeric(format(rawData1$Date, format = "%w")))
rawData1 <- rawData1 %>% mutate(Weekday = ifelse(dayOfWeek %in% 1:5 , 1, 0))
rawData1$hour <- as.numeric(rawData1$hour)
distinct_df = rawData1 %>% distinct(Date)
XX <- NULL
g<-1

#Extracting weather data using weatherdata package
while(g<= nrow(distinct_df)){
  XX_temp <- getDetailedWeather(station_id = "KBOS", distinct_df[g, ] , opt_custom_columns = T, custom_columns = c(2:8, 12:13))
  XX_temp <- separate(XX_temp, Time, into = c("Date", "Time"), sep=" ") 
  XX_temp$Date <- as.Date(XX_temp$Date, "%Y-%m-%d")
  XX_temp <- filter(XX_temp, grepl('.*:54:00', Time ))
  XX_temp$Wind_SpeedMPH <- as.factor(XX_temp$Wind_SpeedMPH)
  XX_temp$Humidity <- as.factor(XX_temp$Humidity)
  XX <- bind_rows(XX, XX_temp)
  g <- g+1
}

#creating hour column: 
XX <- separate(XX, Time, into = c("hour"), sep=":", remove = FALSE)
XX <- transform(XX, hour = as.numeric(hour))

#Joining rawdata and weatherdata by date and then hour
fulldata1 <- full_join(rawData1, XX, by = c("Date", "hour"))

library(chron)

#Converting to time format
fulldata1$Time <- chron::times(fulldata1$Time)

#creating peakhour column: 7AM-7PM – 1 ; 7PM-7AM – 0
fulldata1 <- mutate(fulldata1, Peakhour = ifelse(hour %in% 7:18 , 1, 0))
fulldata1$Time <- NULL
write.csv(XX, "rawdata2_clean.csv")
write.csv(fulldata1, "fulldata1.csv")

#Combining rawdat1, weatherdata1, rawdata2 and weatherdata2 package
fulldata_all <- bind_rows(fulldata,fulldata1)
write.csv(fulldata_all, "fulldata_all.csv")

#Cleaning 
fulldata_test <- fulldata_all
fulldata_test <- na.locf(fulldata_test)

#Rename all the wind direction with "Calm" and "Variable" as "north" as the wind direction degrees for north is 0
fulldata_test$Wind_Direction[fulldata_test$Wind_Direction == "Calm"] = "North"
fulldata_test$Wind_Direction[fulldata_test$Wind_Direction == "Variable"] = "North"

#Replace unwnted values as NA
fulldata_test$VisibilityMPH[fulldata_test$VisibilityMPH == "-9999.0"] = NA

#Using locf package to fill in the missing values
fulldata_test <- na.locf(fulldata_test)
fulldata_test$Wind_SpeedMPH[fulldata_test$Wind_SpeedMPH == "Calm"] = NA
fulldata_test$Wind_SpeedMPH[fulldata_test$Wind_SpeedMPH == "-9999"] = NA
fulldata_test <- na.locf(fulldata_test)
fulldata_test$Account <- as.numeric(fulldata_test$Account)
fulldata_test <- fulldata_test %>% mutate(Date = as.Date(fulldata_test$Date , "%Y-%m-%d"))
fulldata_test$hour <- as.numeric(fulldata_test$hour)
fulldata_test$kWh <- as.numeric(fulldata_test$kWh)
fulldata_test$year <- as.numeric(fulldata_test$year)
fulldata_test$month <- as.numeric(fulldata_test$month)
fulldata_test$dayOfWeek <- as.numeric(fulldata_test$dayOfWeek)
fulldata_test$Weekday <- as.numeric(fulldata_test$Weekday)
fulldata_test$TemperatureF <- as.numeric(fulldata_test$TemperatureF)
fulldata_test$Dew_PointF <- as.numeric(fulldata_test$Dew_PointF)
fulldata_test$Humidity <- as.numeric(fulldata_test$Humidity)
fulldata_test$Sea_Level_PressureIn <- as.numeric(fulldata_test$Sea_Level_PressureIn)
fulldata_test$VisibilityMPH <- as.numeric(fulldata_test$VisibilityMPH)
fulldata_test$Wind_SpeedMPH <- as.numeric(fulldata_test$Wind_SpeedMPH)
fulldata_test$WindDirDegrees <- as.numeric(fulldata_test$WindDirDegrees)
fulldata_test$Peakhour <- as.numeric(fulldata_test$Peakhour)
fulldata_test$day <- as.numeric(fulldata_test$day)

#feature transformation
fulldata_test$TemperatureF <- log(fulldata_test$TemperatureF)
fulldata_test$WindDirecSin <- sin(fulldata_test$WindDirDegrees)
fulldata_test$WindDireccos <- cos(fulldata_test$WindDirDegrees)

fulldata_test <- na.omit(fulldata_test)

write.csv(fulldata_test, "fulldata_test_test1.csv")
