ForeCastData <- read.csv("forecastData.csv")

#Seperate column into date and time
ForeCastData <- separate(ForeCastData, Time, into = c("Date", "Time"), sep=" ")
library(chron)

#Converting to time format
ForeCastData$Time <- times(ForeCastData$Time)

#Converting to date format
ForeCastData <- mutate(ForeCastData, Date = as.Date(ForeCastData$Date , "%Y-%m-%d"))

#creating hour column: 
ForeCastData <- separate(ForeCastData, Time, into = c("hour"), sep=":", remove = FALSE) 
ForeCastData <- transform(ForeCastData, hour = as.numeric(hour))

#Select only feature variables
ForeCastData <- dplyr::select(ForeCastData,TemperatureF,Dew_PointF, Humidity, hour, Date)
hghghgj <- ForeCastData

#Mean of data of same hour and date
ForeCastData <- ForeCastData %>% group_by(Date, hour) %>% summarise(TemperatureF = mean(TemperatureF),
                Dew_PointF = mean(Dew_PointF), Humidity = mean(Humidity))

#creating month,day of week, weekday columns
#month 1-12 => Jan-Dec – Derived from dates
#Dayof Week 0-6 –Sun-Sat – Derived from dates
#Weekday 1- Yes 0- No – Derived from dates
ForeCastData$month <- as.numeric(format(ForeCastData$Date, format = "%m"))
ForeCastData$DayOfWeek <- as.numeric(format(ForeCastData$Date, format = "%w"))
ForeCastData <- mutate(ForeCastData, Weekday = ifelse(DayOfWeek %in% 1:5 , 1, 0))
ForeCastData <- mutate(ForeCastData, Peak_hour = ifelse(hour %in% 7:18 , 1, 0))
write.csv(ForeCastData, "Forecast_Input.csv")

#Forecast data
ForecastDataOutput <- ForeCastData
tidy_lmfit$estimate <- as.numeric(tidy_lmfit$estimate)
ForecastDataOutput$kWh <- tidy_lmfit[2,2] + (tidy_lmfit[3,2]*ForeCastData$month) + 
  (tidy_lmfit[4,2]*ForeCastData$DayOfWeek)+ (tidy_lmfit[5,2]*ForeCastData$Weekday) +
  (tidy_lmfit[6,2]*log(ForeCastData$TemperatureF)) + (tidy_lmfit[7,2]*ForeCastData$Dew_PointF) + 
  (tidy_lmfit[8,2]*ForeCastData$Humidity)+ (tidy_lmfit[9,2]*ForeCastData$Peak_hour)

write.csv(ForecastDataOutput, sprintf("ForecastData_Output_Account_%s.csv",tidy_lmfit[1,2]))
