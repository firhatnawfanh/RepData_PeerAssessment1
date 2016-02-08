
# setwd("~/Downloads/RepData_PeerAssessment1")
# disabled because there are better alternative (download.file) which will easen for non-technical people
# and can be run from any working directory
# download file from repository link
download.file("https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip",destfile="activity.zip")

# unzip and read the downloaded file into R
raw.data<-read.csv(unz("activity.zip","activity.csv"),sep=",",na.strings="NA",stringsAsFactors=FALSE)

#################################################
# strptime date from date column
date<-strptime(raw.data$date,format="%Y-%m-%d")
# make the interval value more uniform in number format
interval<-formatC(raw.data$interval,width=4,flag="0")
# construct date.time which contain date and time interval value in POSIXlt and POSIXct
date.time<-strptime(paste(raw.data$date,interval),format="%Y-%m-%d %H%M")

#################################################
# calculate for total number of steps per day
tot.steps.date<-as.numeric(tapply(raw.data$steps,raw.data$date,sum,na.rm=TRUE))
# plot histogram of total number of steps oer day
hist(tot.steps.date,main="HIstogram of Total Number of Steps per Day",xlab="Total Steps in a Day",ylab="Frequency")

# calculate mean of total numbers of steps taken per days
mean.steps.date<-mean(tot.steps.date,na.rm=TRUE)
# calculate median of total numbers of steps taken per days
median.steps.date<-median(tot.steps.date,na.rm=TRUE)

#################################################
# averaging number of the 5 minute interval (x axis), and steps taken, averaged across all day(y axis)
interval.steps.date<-as.numeric(lapply(split(raw.data$steps,raw.data$interval),mean,na.rm=TRUE))
# construct a plot of interval and average steps taken across all days
plot(unique((interval)),interval.steps.date,type="l",main="Average Daily Number of Steps Taken in 5 Minutes Interval",xlab="Interval",ylab="Average of Number of Steps Taken")
# find 5 minute interval across all the days in the data sets  that contain the max number of steps
interval[which.max(interval.steps.date)]

#################################################
# calculate and report number of missing values in the data sets
colSums(is.na(raw.data))

# calculate filling values
# means of steps taken for the corresponding day
fill.mean.date<-(tapply(raw.data$steps,raw.data$date,mean,na.rm=TRUE))
# medians of steps taken for the corresponding day
fill.median.date<-(tapply(raw.data$steps,raw.data$date,median,na.rm=TRUE))
# means of steps taken for the corresponding interval
fill.mean.interval<-(tapply(raw.data$steps,raw.data$interval,mean,na.rm=TRUE))
# medians of steps taken for the corresponding interval
fill.median.interval<-(tapply(raw.data$steps,raw.data$interval,median,na.rm=TRUE))

# check for each filling strategy
fill.mean.date
fill.median.date
fill.mean.interval
fill.median.interval

# READ THE ASSUMPTION MADE: fill.mean.interval is used as filling value
# we chooses fill.mean.interval as filling value
fill.data<-raw.data
fill.data[is.na(fill.data)]<-round(fill.mean.interval)

# calculate for total number of steps per day of filled dataset
fill.tot.steps.date<-as.numeric(tapply(fill.data$steps,fill.data$date,sum,na.rm=TRUE))
#compare the value with the version where na is omitted
cbind(tot.steps.date,fill.tot.steps.date)
# construct histogram of total number of steps taken each day of filled dataset
hist(fill.tot.steps.date,main="Histogram of Total Number of Steps per Day - NAs Replaced'",xlab="Total Steps in a Day",ylab="Frequency")

# mean of total number of steps taken each of filled dataset
fill.mean.steps.date<-mean(fill.tot.steps.date,na.rm=TRUE)
#compare the value with the version where na is omitted
c(mean.steps.date,fill.mean.steps.date)
# median of total number of steps taken each of filled dataset
fill.median.steps.date<-median(fill.tot.steps.date,na.rm=TRUE)
#compare the value with the version where na is omitted
c(median.steps.date,fill.median.steps.date)
#################################################
# create a new factor in the filled dataset with two levels "weekday" and "weekends"
# create the factor object by assuming that "Saturday" and "Sunday" as weekend, and the rests as weekdays
days<-weekdays(date.time)
days.index<-(unique(weekdays(date.time)))
days[days %in% days.index[1:5]]<-"Weekdays"
days[days %in% days.index[5:7]]<-"Weekend"

# create new factor.data by append days to fill.dataset
factor.data<-cbind(fill.data,days)
# split into weekdats and weekend data frame
weekdays.data<-split(factor.data,factor.data$days)$Weekdays
weekend.data<-split(factor.data,factor.data$days)$Weekend

# average number of steps taken each day across all weekdays of filled weekdays dataset
weekdays.interval.steps.date<-as.numeric(lapply(split(weekdays.data$steps,weekdays.data$interval),mean,na.rm=TRUE))

# average number of steps taken each day across all weekend of filled weekend dataset
weekend.interval.steps.date<-as.numeric(lapply(split(weekend.data$steps,weekend.data$interval),mean,na.rm=TRUE))


# construct a layout for the plot1
par(mfrow=c(2,1))

# construct histogram of average number of steps taken each day across all weekdays of filled weekdays dataset
hist(weekdays.interval.steps.date,main="Average Daily Number of Steps Taken in 5 Minutes Interval-Weekdays",xlab="Interval",ylab="Steps")

# construct histogram of average number of steps taken each day across all weekend of filled weekend dataset
hist(weekend.interval.steps.date,main="Average Daily Number of Steps Taken in 5 Minutes Interval--Weekend",xlab="Interval",ylab="Steps")

#######################################

