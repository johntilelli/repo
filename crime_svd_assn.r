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


log_reg_check = function(x){
  max_val = max(x)
  min_val = min(x)
  ifelse(max_val <= 1 && min_val >= 0, TRUE, FALSE)
  
}
# Unit test

log_reg_check_ut = function(){
  test_set_one = c(0,.25,.5,.75,1)
  test_set_two = c(0,1,0,1)
  test_set_three = c(-.1, 0, .25, .5)
  test_set_four = c(0,.25,.5,1.1)
  stopifnot(
    log_reg_check(test_set_one) == TRUE &&
      log_reg_check(test_set_two) == TRUE &&
      log_reg_check(test_set_three) == FALSE &&
      log_reg_check(test_set_four) == FALSE
  )
}

ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000) #create a standard x distribution
  y_cdf1 = sapply(x_seq, function(x){ #for each point
    sum(dist_a<x)/length(dist_a) #, how many are less than x divided by how long it is
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}


if(interactive()){
  dir = "/Users/Magic/Documents/UW/Methods/Week 6"
  setwd(dir)
  
  # Setup logger
  logReset()
  file_log_handler = getLogger()
  setLevel("INFO", file_log_handler)
  basicConfig(level="INFO")
  addHandler(writeToFile, file = "test.log", level = "INFO", logger = file_log_handler)
  
  loginfo(paste("Working directory set to", dir))
  
  file = 'communities.data'
  
  # load data
  crime_df = load_data(file, header_file = 'crime_headers.txt')
  
  loginfo(paste(file, 'was loaded.'))
  
  # Disregard 'state' and 'communityname'.
  crime_df$state = NULL
  crime_df$communityname = NULL
  
  # Consider 'ViolentCrimesPerPop' as the y-dependent variable.
  summary(crime_df$ViolentCrimesPerPop)
  
  check = log_reg_check(crime_df$ViolentCrimesPerPop)
  
  loginfo(paste('Data is suitable for logistic regression:', check))
  
  # Notice the y-values only vary between 0 and 1.  This is a logistic regression!
  
  # Regular logistic regression:
  regular_logistic = glm(ViolentCrimesPerPop ~ ., data = crime_df, family="binomial")
  
  # Predictions (above or below 0.5) don't make sense since we are predicted a probability (not binary event)
  # So we use AIC as our measure
  regular_logistic_AIC = regular_logistic$aic
  
  loginfo(paste('AIC of regular logistic regression:', regular_logistic_AIC))
  
  # Histogram of fitted values
  hist(regular_logistic$fitted.values)
  hist(crime_df$ViolentCrimesPerPop)
  
  function(x){
    sample = sample(crime_df$ViolentCrimesPerPop,500)
    hist(sample)
  }
  
  # Bonus!!!! Can you think of a way to check that the hypothesis that the two distributions:
  #  are similar?  If we were going to simulate the null, we would want to repeatedly generate
  #  the ViolentCrimesPerPop distribution.  We have 1993 observations of it.  Why not just look at
  #  repeated subsample of it?  Say we sample 500 observations randomly at a time (and repeat many times)...
  #  Then compare the null distribution to our sample statistic...
  #  (Not required for the homework... Just something to think about!)
  
 
  
  #ks stat of two distributions
  alt_ks = ks_stat(1, 1993, regular_logistic$fitted.values , crime_df$ViolentCrimesPerPop)
  
  
  #create null hypothesis, simulate 
  null_simulate = function(){
    dist1 = sample(crime_df$ViolentCrimesPerPop,500)
    dist2 = sample(crime_df$ViolentCrimesPerPop,500)
    return(ks_stat(1, 1993, dist1, dist2))
  }
  
  null_distribution = sapply(1:10000, function(x){
    return(null_simulate())
  })
  
  hist(null_distribution, breaks = 30, freq = FALSE)
  lines(density(null_distribution))
  
  empirical_p_value = sum(null_distribution>alt_ks)/10000
  
  loginfo(paste('Empirical p-value of simulation',empirical_p_value))
  #our p-value is less than 0.05 so we reject the null and conclue that these two distributions are not the same. 
  
  
  
  
  
  # Create SVD features and perform linear regression.
  # Step 1:  Create the data matrix:
  data_matrix = model.matrix(ViolentCrimesPerPop ~. ,data = crime_df)  # modify this equation
  summary(data_matrix)
  # Step 2: Calculate the principle components:
  pc_data_matrix = prcomp(data_matrix)
  
  # Step 3: Look at magnitude of the variances explained (These are the eigenvalues!)
  plot(pc_data_matrix$sdev)
  
  # Step 4: Perform linear regression on components:
  components_num = 10
  pc_all_components = glm(crime_df$ViolentCrimesPerPop ~ pc_data_matrix$x[,1:components_num], family="binomial")
  summary(pc_all_components)
  
  # Get aic
  pc_AIC = AIC(pc_all_components) 
  loginfo(paste('The AIC at', components_num, 'components is', pc_AIC))
  # What number of components minimizes the AIC?
  
  # Here, we use VERY similar code to the lines 181-192 from the "R_Examples_Lecture6.R"
  
  aic_by_num_pc = sapply(2:102, function(x){
    formula_rhs_temp = paste(paste0('pc_data_matrix$x[,',1:x,']'), collapse = ' + ')
    formula_temp = paste('crime_df$ViolentCrimesPerPop ~',formula_rhs_temp)
    pc_all_components_temp = glm(eval(parse(text=formula_temp)),family = "binomial")
    return(AIC(pc_all_components_temp))
  })
  plot(aic_by_num_pc, type='l', lwd=2,
       main='AIC of P.C. Linear Reg with X components',
       xlab="# of components", ylab='AIC')
  # add a horizontal line of where the all variable AIC is at
  abline(h=AIC(regular_logistic), lwd=2, col='red')
  abline(h=min(aic_by_num_pc), lwd=2, col='blue')
  lowest_components = which.min(aic_by_num_pc)
  lowest_aic = min(aic_by_num_pc)
  loginfo(paste('The regression has the lowest AIC of ',lowest_aic, 'at',lowest_components,'components'))
  
  # Report your findings.
  
}
