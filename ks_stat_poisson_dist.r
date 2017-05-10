#create one similuation of the k-s stat between our distribution and a poisson distribution
getwd()
setwd("/Users/Magic/Documents/UW/Methods/Week 5")

#load data
fb_data = read.csv('facebook_edge_list.csv', stringsAsFactors = FALSE)
fb_degree_list = as.numeric(table(fb_data$Source))

degree_mean = mean(fb_degree_list)
min_p = 1
max_p = 534

degree_poisson = dpois(fb_degree_list, degree_mean)

x_seq_fb = seq(min_p,max_p,len=1000) #create a standard x distribution
y_cdf_fb_degrees = sapply(x_seq_fb, function(x){ #for each point
  sum(fb_degree_list<x)/length(fb_degree_list) #, how many are less than x divided by how long it is
})
y_cdf_pois_degrees = sapply(x_seq_fb, function(x){
  sum(degree_poisson<x)/length(degree_poisson)
})
k_s_stat = max(abs(y_cdf_fb_degrees-y_cdf_pois_degrees))

k_s_stat

#plot our one k-s simulation
plot(x_seq_fb,y_cdf_fb_degrees, col='blue', pch=16) #blue is our distribution of degrees
points(x_seq_fb,y_cdf_pois_degrees,col='red', pch=16) #red is the poisson distribution
k_s_stat = max(abs(y_cdf_fb_degrees-y_cdf_pois_degrees)) #this is the max 
# where does it occur?
k_index = which.max(abs(y_cdf_fb_degrees-y_cdf_pois_degrees))
k_s_x = x_seq_fb[k_index] #this is where it occurs
# Add to plot
lines(c(k_s_x,k_s_x), c(y_cdf_fb_degrees[k_index],y_cdf_pois_degrees[k_index]),
      col='black', lwd=2)
