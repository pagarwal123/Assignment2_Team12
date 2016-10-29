library(weatherData)
library(dplyr)
library(tidyr)
library(chron)
rawData1 <- read.csv("CopyOfrawData1.csv")
#REmoving unwanted rows and keeping row for only kwh value

#Return rows with matching conditions in package dplyr
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
XX_temp <- filter(XX_temp, grepl('.*:54:00', Time ))
XX_temp$Wind_SpeedMPH <- as.factor(XX_temp$Wind_SpeedMPH)
XX_temp$Humidity <- as.factor(XX_temp$Humidity)
XX <- bind_rows(XX, XX_temp)
g <- g+1
}

#sepearte column into date and time
XX <- separate(XX, Time, into = c("Date","Time"), sep=" ", remove = TRUE)

#creating hour column: 
XX <- separate(XX, Time, into = c("hour"), sep=":", remove = FALSE)
XX <- transform(XX, hour = as.numeric(hour))
XX$Date <- as.Date(XX$Date, "%Y-%m-%d")

#Joining rawdata and weatherdata by date and then hour
fulldata <- full_join(rawData1, XX, by = c("Date", "hour"))
library(chron)

#Converting to time format
fulldata$Time <- chron::times(fulldata$Time)

#creating peakhour column: 7AM-7PM – 1 ; 7PM-7AM – 0
fulldata <- mutate(fulldata, Peakhour = ifelse(hour %in% 7:18 , 1, 0))
fulldata$Time <- NULL
write.csv(XX, "rawdata1_clean.csv")
write.csv(fulldata, "fulldata.csv")





