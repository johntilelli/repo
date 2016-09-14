require(dplyr) #to group
require(ggplot2) #to graph
require(beanplot) #for beanplots
#data comes from https://www.prontocycleshare.com/datachallenge
setwd("/Users/Magic/Documents/R/Bike Share Seattle/open_data_year_one")
#load all trip data
tripdata = read.table("2015_trip_data.csv", header=T, quote="\"",sep=",")
#convert tripdata$stoptime to ymd date
tripdata$date = as.Date(tripdata$stoptime, format="%m/%d/%Y")
#create table with frequency by day
daily_trips = count(tripdata, date)
#plot
total_use_plot = ggplot(daily_trips, aes(date,n))+
  geom_line(col="blue", lwd=1, alpha = .3)+
  labs(x="Date",y="Number of Rides")+
  ggtitle("Daily Usage of Pronto Cycle Share")+
#  geom_point(col="gray50") +
  geom_smooth(method="loess") 
#view
total_use_plot

#adding weekday num and workday y/n
daily_trips$weekday = as.POSIXlt(daily_trips$date)$wday 
daily_trips$workday = factor(daily_trips$weekday)
levels(daily_trips$workday) <- list("1" = c("1","2","3","4","5"),"0" = c("0","6"))

#double density plot - workday
ddensity_workday = ggplot(daily_trips, aes(n, fill=workday, color=workday)) +
  geom_density(alpha=0.4) +
  theme_minimal() +
  xlab("Daily Bike Use Count") +
  ylab("Density") +
  scale_fill_discrete(name="Work Day?") +
  scale_color_discrete(name="Work Day?") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="top")
ddensity_workday


#create month field
daily_trips$month = as.POSIXlt(daily_trips$date)$mon+1
daily_trips$month = ordered(daily_trips$month, 1:12) 
levels(daily_trips$month) = c(month.abb)

#Boxplot
boxplot_daily = ggplot(daily_trips, aes(month, n, fill=workday)) +
  xlab("Month") +
  ylab("Daily Cycle Use Count") +
 geom_boxplot() +
  theme_minimal() +
  scale_fill_discrete(name="Work Day?") +
  scale_color_discrete(name="Work Day?") +
  theme(legend.position="bottom")
boxplot_daily

#beanplot
beanplot_daily = beanplot(n ~ month
         , data = daily_trips, side="second"
         , overallline="median", what=c(1,1,1,0)
         , col=c("gray70", "transparent", "transparent", "blue")
         , ylab = "Month",
         xlab = "Daily Cycle Use Count", horizontal=TRUE)
beanplot_daily
