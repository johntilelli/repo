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

dj$month = as.numeric(format(dj$Date, format="%m")) #month number
dj$season = floor((dj$month-1) / 3) 


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

# Closer view:
plot(dj$Date, dj$DJIA, type="l", lwd=2, main="DJIA",
     xlab="Time", ylab="DJIA",
     xlim=c(as.Date("2009-01-01"), as.Date("2009-12-31")),
     ylim=c(6500,11000))
lines(dj$Date, dj_model$fitted.values, lwd=2, lty=8, col="red")
# Slightly lagging by a bit?
# Makes sense because of the "most important" variable.

# More autoregressive than 1 day?  Let's check:
DJIA_2_periods_ago = sapply(1:nrow(dj), function(x){
  if(x <= 2){
    return(dj$DJIA[1])
  }else{
    return(dj$DJIA[x-2])
  }
}
dj$two_days_ago = DJIA_2_periods_ago

dj_model_AR2 = lm(DJIA ~ . - Date, data = dj)
summary(dj_model_AR2)



headcount_weekly = aggregate(HeadCount ~ week_count + GameCode, data = headcount, sum)

# Not a full last week, drop the last data point
headcount_weekly = headcount_weekly[1:52,]

# Fit time series:
headcount_arima = arima(headcount_weekly$HeadCount, order = c(1,1,1), seasonal = list(order=c(1,0,1)))
# What are the fitted coefficients?
headcount_arima$coef

arima_fitted = headcount_weekly$HeadCount - headcount_arima$residuals

arima_predictions = predict(headcount_arima, n.ahead=25)
x_pred = nrow(headcount_weekly) : (nrow(headcount_weekly) + 25)

plot(headcount_weekly$HeadCount, type="l", lwd = 2, col="black", xlim = c(0,80))
lines(arima_fitted, lwd = 2, col="red")
lines(x_pred, c(arima_fitted[length(arima_fitted)],arima_predictions$pred),
      lwd=2, col="red", lty=8)

