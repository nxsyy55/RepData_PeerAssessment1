rm(list = ls())
setwd('D:/coursera/reproducible//pa1/RepData_PeerAssessment1/')
library(lubridate)

#get data ready
data <- read.csv('data/activity.csv')
#data <- data.table(data)
data$date <- as.Date(as.character(data$date))

#Make a histogram of the total number of steps taken each day
data$day.number <-  as.factor(rep(1:61, each=288))
data$interval <- as.factor(data$interval)
temp.res <- aggregate(data$steps, list(data$day.number),
                      sum, na.rm = T)
names(temp.res) <- c('Day.Number', 'Steps')
hist(temp.res[,2], breaks = 15, xlim = c(0,23999),
     main = 'Histogram of steps taken each day',
     xlab = 'steps taken each day')
mean <- mean(temp.res[,2]) #9354.23
median <- median(temp.res[,2]) #10395

#time serise plot
time <- as.integer(with(data, levels(interval)))
ave.steps <- unclass(by(data$steps, data$interval, sum, na.rm = T))/61
plot(time, ave.steps, type = 'l')
max.inter <- which.max(ave.steps) #8:35 180 steps

# NA numbers
na.num <- sum(is.na(data$steps)) #2304

data.new <- data
# strategy for filling in all of the missing values
# replace the NAs by the mean of the interval
for (i in 1:17568) {
  if (is.na(data.new[i, 'steps'])) {
    data.new[i, 'steps'] <- ave.steps[as.character(data.new[i, 'interval'])]
  }
}

#Make a histogram of the total number of steps taken each day 
#Calculate and report the mean and median total number of steps taken per day.
#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
temp.res <- aggregate(data.new$steps, list(data.new$day.number),
                                 sum, na.rm = T)
names(temp.res) <- c('Day.Number', 'Steps')
hist(temp.res[,2],main = 'Histogram of steps taken each day',
                xlab = 'steps taken each day', breaks = 12)
mean.new <- mean(temp.res[,2]) #10581.01
median.new <- median(temp.res[,2]) #10395

