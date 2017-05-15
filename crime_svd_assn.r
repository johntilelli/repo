

# SVD on features then regression on those features for crime prediction

library(logging)



load_data = function(input_file, header_file){
  # Load data
  crime_data = read.table(input_file, sep=",", header=FALSE, na.strings = c("NA","?"))
  # Load headers
  crime_headers = read.table(header_file)
  names(crime_data) = crime_headers$V1
  
  #drop features that are missing a majority of observations:
  crime_data = crime_data[colSums(is.na(crime_data)) < 100]
  
  # Drop missing rows
  crime_data = crime_data[complete.cases(crime_data),]
  
  return(crime_data)
}


# Unit test

if(interactive()){
  
  setwd("/Users/Magic/Documents/UW/Methods/Week 6")
  
  # Setup logger
  logReset()
  file_log_handler = getLogger()
  setLevel("INFO", file_log_handler)
  basicConfig(level="INFO")
  addHandler(writeToFile, file = "test.log", level = "INFO", logger = file_log_handler)
  
  # load data
  crime_df = load_data('communities.data', header_file = 'crime_headers.txt')
  
  #loginfo('info')
  #logdebug('debug')
  #logwarn('warn')
  
  # Disregard 'state' and 'communityname'.
  crime_df$state = NULL
  crime_df$communityname = NULL
  
  # Consider 'ViolentCrimesPerPop' as the y-dependent variable.
  summary(crime_df$ViolentCrimesPerPop)
  # Notice the y-values only vary between 0 and 1.  This is a logistic regression!
  
  # Regular logistic regression:
  regular_logistic = glm(ViolentCrimesPerPop ~ ., data = crime_df, family="binomial")
  #Warning message:
  #In eval(expr, envir, enclos) : non-integer #successes in a binomial glm!
  
  # Predictions (above or below 0.5) don't make sense since we are predicting a probability (not binary event)
  # So we use AIC as our measure
  regular_logistic_AIC = regular_logistic$aic
  
  
  
  
  # Histogram of fitted values
  hist(regular_logistic$fitted.values) #fitted values are probabilities
  hist(crime_df$ViolentCrimesPerPop)
  
  # Bonus!!!! Can you think of a way to check that the hypothesis that the two distributions:
  #  are similar?  If we were going to simulate the null, we would want to repeatedly generate
  #  the ViolentCrimesPerPop distribution.  We have 1993 observations of it.  Why not just look at
  #  repeated subsample of it?  Say we sample 500 observations randomly at a time (and repeat many times)...
  #  Then compare the null distribution to our sample statistic...
  #  (Not required for the homework... Just something to think about!)
  
  # Create SVD features and perform linear regression.
  
  # Full linear regression:
  
  # remove missing rows
  crime_df = crime_df[complete.cases(crime_df),]
  
  log_price_all_model = lm(log(ViolentCrimesPerPop) ~ ., data = crime_df)
  
  summary(log_price_all_model)
  
  # Step 1:  Create the data matrix:
  data_matrix = model.matrix(log(ViolentCrimesPerPop) ~. ,data = crime_df)  # modify this equation
  
  # Step 2: Calculate the principle components:
  pc_data_matrix = prcomp(data_matrix)
  
  # Step 3: Look at magnitude of the variances explained (These are the eigenvalues!)
  plot(pc_data_matrix$sdev)
  
  # Step 4: Perform linear regression on components:
  pc_all_components = glm(crime_df$ViolentCrimesPerPop ~ pc_data_matrix$x[,1:102], family="binomial")
  summary(pc_all_components)
  
  # Get aic
  pc_AIC = AIC(pc_all_components) # Slightly better...
  
  # What number of components minimizes the AIC?
  
  # Here, we use VERY similar code to the lines 181-192 from the "R_Examples_Lecture6.R"
  
  # Report your findings.
  
  #fill in missing data
  mean_norm_losses = mean(data$normalized_losses, na.rm=TRUE)
  data$normalized_losses[is.na(data$normalized_losses)] = mean_norm_losses
  
  
#error here... i think there are NAs somewhere... see 111
  #should pc_all_components_temp be a glm or a lm? I think it's glm. 
  aic_by_num_pc = sapply(2:102, function(x){
    formula_rhs_temp = paste(paste('pc_data_matrix$x[,',1:x,']'), collapse = ' + ')
    formula_temp = paste('log(crime_df$ViolentCrimesPerPop) ~',formula_rhs_temp)
    pc_all_components_temp = glm(eval(parse(text=formula_temp)))
    return(AIC(pc_all_components_temp))
  })
  plot(aic_by_num_pc, type='l', lwd=2,
       main='AIC of P.C. Linear Reg with X components',
       xlab="# of components", ylab='AIC')
  # add a horizontal line of where the all variable AIC is at
  abline(h=AIC(log_price_all_model), lwd=2, col='red')
  which.min(aic_by_num_pc) # 59 principal components!
  
}




