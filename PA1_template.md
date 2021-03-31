---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Reproducible Research: Project 1

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.



## Loading and preprocessing the data

##### 1. Load the data (i.e. read.csv())

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activity <- read.csv('activity.csv')
```
##### 2. Process/transform the data (if necessary) into a format suitable for your analysis.


```r
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

##### 1. Calculate the total number of steps taken per day


```r
StepsByDay<- aggregate(steps~date,activity,sum,na.rm=TRUE)
#head(StepsByDay)
```

##### 2. Make a histogram of the total number of steps taken each day. 


```r
ggplot(StepsByDay, aes(x = steps)) +
    geom_histogram(fill = "green", col= "red",binwidth = 1000) +
    labs(title = "Total steps per day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/ggplot1code-1.png)<!-- -->

##### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
StepsByDayMean <- mean(StepsByDay$steps,na.rm = TRUE)
StepsByDayMedian <- median(StepsByDay$steps,na.rm = TRUE)
```

###### Mean and Median values are:
* Mean : 1.0766189\times 10^{4}
* Median :  10765

## What is the average daily activity pattern?

##### 1. Make a time series plot (i.e.type = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
perdaySteps <- aggregate(steps~date,activity,sum)
#perdaySteps
stepsbyinterval <- activity %>% group_by(interval)%>% summarise(avgSteps= mean(steps,na.rm = TRUE))
#stepsbyinterval
```

##### 1. Make a time series plot


```r
ggplot(stepsbyinterval, aes(x = interval , y = avgSteps)) + geom_line(color="red", size=1) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/timeseries-1.png)<!-- -->
 
##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#maxsteps <- max(stepsbyinterval$avgSteps)
#filter(stepsbyinterval,avgSteps==maxsteps)
mostSteps <- which.max(stepsbyinterval$avgSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2",stepsbyinterval[mostSteps,'interval'])
```

* Most Steps taken  at: 8:35
Interval at "8:35"(8 hrs 35 min ) contains on average the maximum number of steps.

## Imputing missing values

##### 1. Calculate and report the total number of missing values in the dataset 


```r
numMissingValues <- length(which(is.na(activity$steps)))

#numMissingValues
```

* Number of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#impute() function simply imputes missing value using user defined statistical method (mean, max, median). 
#It’s default is median.
#install "Hmisc" package and load library

activityImputed <- activity
activityImputed$steps <- impute(activity$steps, fun=mean)
```

##### 4. Make a histogram of the total number of steps taken each day 


```r
stepsByDayImputed <- aggregate(activityImputed$steps~activityImputed$date,activityImputed, sum)
## Changing col names
names(stepsByDayImputed) <- c("Date", "DailySteps")

#sum(is.na(stepsByDayImputed$dailySteps))
# All missing values are imputed

hist <- ggplot(data=stepsByDayImputed, aes(x=DailySteps))
hist + geom_histogram(fill="Blue",binwidth=1000)+labs(title = "Daily Steps", x = "Daily Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/dailystepscode-1.png)<!-- -->

##### Calculate and report the mean and median total number of steps taken per day. 


```r
stepsByDayMeanImputed <- mean(stepsByDayImputed$DailySteps,na.rm=TRUE)
stepsByDayMedianImputed <- median(stepsByDayImputed$DailySteps,na.rm=TRUE)
```
###### Mean and Median of total number of steps taken per day.
* Mean (Imputed) : 1.0766189\times 10^{4}
* Median (Imputed) :  1.0766189\times 10^{4}

### Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityImputed$daysofWeek <- weekdays(activityImputed$date)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activityImputed$wDay <- factor(activityImputed$daysofWeek %in% weekdays1,levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
```

##### 2. Make a panel plot containing a time series plot


```r
# Creating the data set that will be plotted

activityByDay <-  aggregate(steps ~ interval + wDay, activityImputed, mean)
ggplot(activityByDay , aes(x = interval , y = steps, color=`wDay`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`wDay` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/ggplot2code-1.png)<!-- -->
Activity on Weekdays has the highest peak in one interval over others. On weekends the activity have more peaks. The weekend activity rate is much higher throughout the day compared to the weekdays.
