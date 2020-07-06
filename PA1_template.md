---
title: "Reproducible Research/project1"
author: "Ravi S.Gonella"
date: "7/3/2020"
output: 
  html_document: 
    keep_md: yes
---

## Reading csv Data into data frame.

```r
activityData <- read.csv("c:/datascience/05/project1/activity.csv")
```

## What is the mean total number of steps taken per day

### 1.Calculate the total number of steps taken per day

```r
total_steps <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)

head((total_steps), 10)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
```

### 2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(total_steps$steps, xlab="Steps", ylab="Frequency", main = "Daily Steps", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
### 3.Calculate and report the mean and median of the total number of steps taken per day


```r
mean_steps <- mean(total_steps$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps <- median(total_steps$steps)
median_steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
### 1.Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_per_interval <- aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
head(steps_per_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(steps~interval, data=steps_per_interval, type="l", col="green", xlab="5min interval", ylab="Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps <- steps_per_interval[which.max(steps_per_interval$steps),]$interval
max_steps
```

```
## [1] 835
```

## Imputing missing values
### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
missing_values <- sum(is.na(activityData$steps))
missing_values
```

```
## [1] 2304
```

```r
summary(activityData)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc


```r
mean_steps_interval<-function(interval){
    steps_per_interval[steps_per_interval$interval==interval,]$steps
}
```
### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
activityDataNoNA <- activityData

for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        
        activityDataNoNA[i,]$steps <- mean_steps_interval(activityDataNoNA[i,]$interval)
    }
}
```

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_steps_per_day <- aggregate(steps ~ date, data=activityDataNoNA, sum)
head(total_steps_per_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(total_steps_per_day$steps, xlab = "Total Steps per Day", main = "Histogram of Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
mean_steps_per_day <- mean(total_steps_per_day$steps)
mean_steps_per_day
```

```
## [1] 10766.19
```

```r
median_steps_per_day <- median(total_steps_per_day$steps)
median_steps_per_day
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
### 1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.



```r
library("data.table")

activityDT <- data.table::fread(input = "c:/datascience/05/project1/activity.csv")

activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]

activityDT[, `Day of Week`:= weekdays(x = date)]

activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"

activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"

activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]

head(activityDT, 10)
```

```
##     steps       date interval Day of Week weekday or weekend
##  1:    NA 2012-10-01        0      Monday            weekday
##  2:    NA 2012-10-01        5      Monday            weekday
##  3:    NA 2012-10-01       10      Monday            weekday
##  4:    NA 2012-10-01       15      Monday            weekday
##  5:    NA 2012-10-01       20      Monday            weekday
##  6:    NA 2012-10-01       25      Monday            weekday
##  7:    NA 2012-10-01       30      Monday            weekday
##  8:    NA 2012-10-01       35      Monday            weekday
##  9:    NA 2012-10-01       40      Monday            weekday
## 10:    NA 2012-10-01       45      Monday            weekday
```
### 2.Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
library("ggplot2")

activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]

IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + 
    geom_line() + 
    labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
