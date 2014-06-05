setwd('D:/coursera/reproducible//pa1/RepData_PeerAssessment1/')
#library(data.table)
library(lubridate)
#get data ready
data <- read.csv('data/activity.csv')
#data <- data.table(data)
data$date <- as.Date(as.character(data$date))

#Make a histogram of the total number of steps taken each day
data$day.number <-  as.factor(rep(1:61, each=288))
data$interval <- as.factor(data$interval)
temp.res <- unclass(by(data$steps, data$day.number, sum, na.rm = T))

#remove the NAs
tem.data <- temp.res[temp.res != 0]
hist(tem.data, breaks = 30, pro = T, 
     main = 'Histogram of steps taken each day',
     ylim = c(0,.00025))
mean.na.rm <- mean(tem.data)
median.na.rm <- median(tem.data)

#time serise plot
time <- as.integer(with(data, levels(interval)))
ave.steps <- unclass(by(data$steps, data$interval, sum, na.rm = T))/61
plot(time, ave.steps, type = 'l')
max.inter <- which.max(ave.steps) #8:35 180 steps

# NA numbers
na.num <- sum(is.na(data$steps)) #2304
na.pct <- na.num / 17568

# strategy for filling in all of the missing values
# use total steps taken per day 
