##--------------------------------------------
##
## Chicago Diabetes Homework (Lecture 4)
##
## Class: PCE Data Science Methods Class
## Don't forget to functionalize, create unit test, and log!!!!
##--------------------------------------------

getwd()
setwd('/Users/Magic/Documents/UW/Methods for Data Analysis/Week 4')
list.files()

data = read.csv('ChicagoDiabetesData.csv', stringsAsFactors = FALSE)

data_means = apply(data[-1],2,mean) #average across all zipcodes
#they will be rates (numbers per 10,000 people, same denom)

#-------------------1--------------------#
hospitalizations = data_means[grepl('Hospitalizations', names(data[-1]))] #pull out hospitalizations
admit_rate = data_means[grepl('Crude.Rate.[0-9]+$', names(data[-1]), perl = TRUE)] #pull out crude rate

plot(hospitalizations, admit_rate)

#order is important
hospitalizations = hospitalizations[order(admit_rate)]
admit_rate = admit_rate[order(admit_rate)] #gives index numbers ordered smallest to highest temp


slope_guess = (hospitalizations[1] - hospitalizations[9])/(admit_rate[1] - admit_rate[9])

# Pick a point, and calculate the y-intercept:
# y = mx + b,   or   b = y - mx
y_int_guess = hospitalizations[1] - slope_guess * admit_rate[1]

plot(admit_rate, hospitalizations)
abline(y_int_guess, slope_guess)

fit = lm(hospitalizations ~ admit_rate)

abline(fit, col = "red")
#fit a line here and interpret output


#-------------------2--------------------#
hospitalizations_diff = diff(hospitalizations)
admit_rate_diff = diff(admit_rate)
#this should be much more linear, but we sacrifice a data point

plot(hospitalizations_diff, admit_rate_diff)

lm(hospitalizations_diff ~ admit_rate_diff)
