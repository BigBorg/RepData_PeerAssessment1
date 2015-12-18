library(dplyr)
library(ggplot2)
library(lattice)
unzip("activity.zip")
activity<-read.csv("activity.csv")
mylocal<-Sys.getlocale("LC_ALL");
Sys.setlocale("LC_ALL", "English");

#What is mean total number of steps taken per day?
##1.Calculate the total number of steps taken per day.
totalstepsbyday<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
hist(totalstepsbyday)

##2.Make a histogram of the total number of steps taken each day.
hist(totalstepsbyday)

##3.Calculate and report the mean and median of the total number of steps taken per day.
summary(totalstepsbyday)

#What is the average daily activity pattern?
##1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avgstepsbyinterval<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
interval<-levels(factor(activity$interval))
interval<-as.numeric(interval)
qplot(interval,avgstepsbyinterval,geom = "line")
##2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
names(avgstepsbyinterval)[avgstepsbyinterval==max(avgstepsbyinterval)]

#Imputing missing values
##1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
summary(activity$steps)
##2-3.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.Create a new dataset that is equal to the original dataset but with the missing data filled in.
meandata<-data.frame(name=names(avgstepsbyinterval),mean=avgstepsbyinterval)
meansteps<-avgstepsbyinterval[as.character(activity$interval)]
activity<-mutate(activity,meansteps=meansteps)
activity$steps<-as.numeric(activity$steps)
activity[is.na(activity$steps),1]<-activity[is.na(activity$steps),4]
##4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalstepsbyday2<-tapply(activity$steps,activity$date,sum)
hist(totalstepsbyday2)
summary(totalstepsbyday2)

#Are there differences in activity patterns between weekdays and weekends?
##1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
weekday<-weekdays(date)
isweekend<-weekday %in% c("Sunday","Saturday")
weekday[isweekend]<-'weekend'
weekday[!isweekend]<-'weekday'
weekday<-as.factor(weekday)
##2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
activity<-mutate(activity,weekday=weekday)
gactivity<-group_by(activity,weekday,interval)
stepsbyweekdayandinterval<-summarize(gactivity,steps=mean(steps))
qplot(interval,steps,data = stepsbyweekdayandinterval,geom="line",facets = "weekday~.")

###Sys.setlocal("LC_ALL", mylocal) #ifnecessary