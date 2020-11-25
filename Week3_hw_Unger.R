# R Course homework week 3
# Silke Unger


# Exercise 1

# convert DMS to radians
# first create a function
dmstorad <- function(d,m,s) {
  rad <- pi * (d+m/60+s/3600)/180
  return(rad)}

# then put in the values for the angle and the uncertainty and assign them
angle <- dmstorad(1,21,0)
angle_err <- dmstorad(0,1,0)
print(angle)
print(angle_err)

# assign the distance and the respective precision
dis <- 2550
dis_err <- 25

# calculate the height including uncertainty
# first the height
h_building <- dis*tan(angle)
print(h_building)
# then the uncertainty of the height
h_err <- dis_err*tan(angle_err)
print(h_err)

# calculate the fractional uncertainty
frac_un_h <- h_err/abs(h_building)
print(frac_un_h)



# Exercise 2

# assign the values
d1 <- 25.53
d1_err <- 0.1
d2 <- 29.66
d2_err <- 0.2

# calculate the duration of volcanic activity including error
# minimum duration
min_vol <- d2-d1-d2_err-d1_err
# maximum duration
max_vol <- d2-d1+d2_err+d1_err
# show min and max duration
print(min_vol)
print(max_vol)



# Exercise 3

# set working directory
setwd("C:/Users/BESN/Desktop/Computing_and_Data_Analysis_in_Geosciences/Week3Homework")
# read in the data table
eqscals <- read.table("ex3_eqscals.txt", header = FALSE, sep="")
# watch the table
View(eqscals)
# give headers to the table
colnames(eqscals)<-c("X","r", "Mo")
View(eqscals)

# 3a
# mean, median, sd and median abs of r
print(mean_r <-mean(eqscals$r))
print(median_r<-median(eqscals$r))
print(sd_r<-sd(eqscals$r))
print(mad_r<-mad(eqscals$r))
# mean, median, sd and median abs of Mo
print(mean_Mo <-mean(eqscals$Mo))
print(median_Mo<-median(eqscals$Mo))
print(sd_Mo<-sd(eqscals$Mo))
print(mad_Mo<-mad(eqscals$Mo))

# 3b
# plots of Mo and r
boxplot(eqscals$Mo, main="Boxplot of Mo")
print(summary(eqscals$Mo))
print(summary(eqscals$r))
boxplot(eqscals$r, main="Boxplot of r")
hist(eqscals$r)
plot(eqscals$X, eqscals$r)
# The min and the max value of Mo are outliers. 
# The most values of r are over 1500m

# 3c
# trim the data 
trimmed_data <- data.frame(eqscals[eqscals$Mo<3*mad_Mo,])
View(trimmed_data)

# calculate mean, median and sd of Mo from the trimmed data
print(summary(trimmed_data))
print(median_Mo_t <- (median(trimmed_data$Mo)))
print(mean_Mo_t <- mean(trimmed_data$Mo))
print(sd_Mo_t<-sd(trimmed_data$Mo))
# calculate uncertainty in Mo
se <- sd_Mo_t/abs(median_Mo_t)    
se2 <- sd_Mo_t/abs(mean_Mo_t)    #didn't know if I should take mean or median
print(se)
print(se2)

# 3d
# calculate Mw
Mw_Mo <- log10(median_Mo_t)/1.5-6.0
print(Mw_Mo)
# calculate the Mw uncertainty
Mw_Mo_err <- log10(se)/1.5-6.0
print(abs(Mw_Mo_err))


