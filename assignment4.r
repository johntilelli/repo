##--------------------------------------------
##
## Assignment 4
## Date: 5/3/2017
## Class: PCE Data Science Methods Class
## Name: John Tilelli
##--------------------------------------------
library(logging)


# Get the log file name that has a date-time in the name
get_log_filename = function(){
  log_file_name = format(Sys.time(), format="HW4_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}  

#function to pull out hospitalizations
pull_values = function(input, expression){
  mean_input = apply(input[-1],2,mean)
  mean_input[grepl(paste(expression,'.[0-9]+$', sep = ''), names(input[-1]), perl = TRUE)]
}

#unit test here

if (interactive()){
  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
  # Setup working directory
  working_directory = '/Users/Magic/Documents/UW/Methods for Data Analysis/Week 4'
  setwd(working_directory)
  loginfo(paste('Working directory set to ', working_directory))
  
  #load data
  file = 'ChicagoDiabetesData.csv'
  data = read.csv(file, stringsAsFactors = FALSE)
  loginfo(paste('Load ', file))

  # Perform unit test
  hospitalizations = pull_values(data, 'Hospitalizations')
  admit_rate = pull_values(data, 'Crude.Rate')
  
  #plot
  plot(admit_rate, hospitalizations)
  
  #fit model
  fit = lm(hospitalizations ~ admit_rate)
  loginfo(paste('The R-squared is ',summary(fit)$r.squared))
  coef(fit)
  abline(fit)

  #get differences for new model
  hospitalizations_diff = diff(hospitalizations)
  admit_rate_diff = diff(admit_rate)
 
  #fit new model
  plot(hospitalizations_diff,admit_rate_diff)
  fit_diff = lm(admit_rate_diff~ hospitalizations_diff)
  abline(fit_diff)
  loginfo(paste('The R-squared is ',summary(fit_diff)$r.squared))
  coef(fit_diff)
}
