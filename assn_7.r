# Homework 7
# Hint


##-----Load Libraries-----
library(dplyr)
library(data.table)

##-----Load Data-----
setwd("/Users/Magic/Documents/UW/Methods/Week 1")
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
setwd("/Users/Magic/Documents/UW/Methods/Week 7")
weather = read.csv('las_vegas_hourly_weather.csv', stringsAsFactors = FALSE)


##-----Format Data----
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
names(weather) = c('time','temp','dew_pt','humidity','pressure',
                   'visibility','wind_dir','wind_speed','gust_speed',
                   'precipitation','events','conditions',
                   'wind_dir_deg','date')

weather$datetime = paste(weather$date,weather$time)
weather$datetime = strptime(weather$datetime, format="%Y-%m-%d %I:%M %p")
weather$Hour = as.numeric(format(round(weather$datetime, units="hours"), format="%H"))

##----Drop Duplicates----
weather = weather[!duplicated(weather[c("date", 'Hour')]),]


##----Merge Data-----
weather$DateFormat = weather$date
weather$date = NULL
weather$DateFormat = as.Date(weather$DateFormat, format="%Y-%m-%d")

headcount = merge(headcount, weather, all.x=TRUE, by=c("DateFormat","Hour"))

##----Imputation for NAs in weather-----
numeric_cols = c(11:15, 17:19, 22)
# Linear Interpolation:
headcount[,numeric_cols] = apply(headcount[,numeric_cols], 2, function(x) approx(x, xout=1:length(x), rule=2)$y)

##---Drop character columns----
headcount$wind_dir = NULL
headcount$time = NULL
headcount$datetime = NULL

##-----Deal with events/conditions----
headcount$events[headcount$events == ""] = "None"
headcount$events[is.na(headcount$events)] = "None"
headcount$conditions[is.na(headcount$conditions)] = "None"

##----Format Data for Time Series Exploration-----

?floor()
headcount$month_count = floor(headcount$DayNumber/30.5) #month number since epoch

headcount$year_count = as.numeric(format(headcount$DateFormat, format="%Y")) - 1990 #year number

#create a flag that indicates if the headcount occured on a weekday (1 - yes, 0 - no)
headcount$Weekday_Flag = headcount [, Weekday_Flag := ifelse(DayOfWeek %in% c(1,7),0,1)]
headcount$Weekday_Flag = headcount [DayOfWeek %in% c(1,7)] = 1

hc_model = lm(HeadCount ~ DayNumber, data = headcount_daily)
summary(hc_model)

headcount = data.table(headcount)
headcount_daily = aggregate(HeadCount ~ DayNumber, data = headcount, sum)

plot(headcount_daily$DayNumber
     , headcount_daily$HeadCount, type="l", lwd=2, main="Headcount",
     xlab="Time", ylab="Headcount")
lines(headcount_daily$DayNumber, hc_model$fitted.values, lwd=2, lty=8, col="red")



headcount_weekly = aggregate(HeadCount ~ week_count + GameCode, data = headcount, sum)

# Not a full last week, drop the last data point
headcount_weekly = headcount_weekly[1:52,]

                                 
                                 ##NEW CODE FOR DAILY REGRESSION
require(data.table)
headcount = data.table(headcount)
headcount_daily = headcount[, list(sum_headcount = sum(HeadCount),
                      temp_high = max(temp),
                     temp_avg = mean(temp),
                      humidity_high = max(humidity),
                      humidity_avg = mean(humidity),
                      sum_precip = sum(precipitation)), by = list(DateFormat,GameCode)]


headcount_daily$rain_ind = ifelse(headcount_daily$sum_precip > 0, 1, 0)
headcount_daily$weekday_flag = ifelse(as.numeric(format(headcount_daily$DateFormat, format = "%w")) %in% c(1,7),0,1)


hc_model = lm(sum_headcount ~ . - DateFormat - sum_precip, data = headcount_daily)
summary(hc_model)
