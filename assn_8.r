library(logging)
library(boot)

logistic_boot = function(data, indices){
  data_new = data[indices, ]
  fit_new = glm(Buy ~ ., family=binomial, data = data_new)
  return(coef(fit_new))
}

if(interactive()){
  #set logging
  logReset()
  file_log_handler = getLogger()
  setLevel("INFO", file_log_handler)
  basicConfig(level="INFO")
  addHandler(writeToFile, file = "test.log", level = "INFO", logger = file_log_handler)
  loginfo(paste("Logging loaded. Saving log file to", getwd(), "."))
  
  #set working directory and load data
  dir = "/Users/Magic/Documents/UW/Methods/Week 8"
  setwd(dir)
  loginfo(paste("Working directory set to", dir, "."))
  
  file = 'AdvertisingPrediction.csv'
  ad_data = read.csv(file)
  loginfo(paste(file, "data loaded."))
  ad_data$Obs.No. = NULL
  
  
  # Bootstrapping the residuals.  First we have to get the residuals from
  #  the logistic regression
  logistic_fit = glm(Buy ~ ., family=binomial, data = ad_data)
  
  #move this out when done.. add unit test
  logistic_boot = function(data, indices){
    data_new = data[indices, ]
    fit_new = glm(Buy ~ ., family=binomial, data = data_new)
    return(coef(fit_new))
  }
  
  N = 10000 # What to increase this to?
  boot_estimates = boot(data=ad_data, statistic=logistic_boot, R=N)
  #log this
  
  # Calculate Confidence intervals for everything with 'boot.ci()'
  
  boots = lapply(1:17, function(x){
    temp_boot_ci = boot.ci(boot_estimates, type="norm", index=x)
    return(c(attributes(temp_boot_ci$t0)[[1]],temp_boot_ci$normal[2],temp_boot_ci$normal[3]))
  })
  
  boot_conf_matrix = matrix(unlist(boots), ncol=3, byrow = TRUE)
  loginfo(boot_conf_matrix)
  
  
  #log this
  # Can also use 'type="bca"', for tighter approximations to the Conf. Interval.
  
  # You may get some warnings about the CI using extreme endpoint in the calculation
  #  If that bothers you, you can (1) increase N, or (2) decrease the confidence interval
  #  (2) can be done if you read the documentation of the function, type: '?boot.ci'
}
