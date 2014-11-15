# Reproducible Research: Peer Assessment 1
## Introduction

It is now possible to collect a large amount of data about personal movement using 
activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. 
These type of devices are part of the “quantified self” movement – a group of 
enthusiasts who take measurements about themselves regularly to improve their 
health, to find patterns in their behavior, or because they are tech geeks. But 
these data remain under-utilized both because the raw data are hard to obtain 
and there is a lack of statistical methods and software for processing and 
interpreting the data.

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. The data 
consists of two months of data from an anonymous individual collected during the 
months of October and November, 2012 and include the number of steps taken in 5 
minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total 
of 17,568 observations in this dataset.


## Loading and preprocessing the data


```r
setwd("~/Desktop/RepData_PeerAssessment1") 
data1 <- read.csv("activity.csv")
head(data1)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(data1)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

```r
data1.splt.day <- split(data1, as.factor(data1$date))
steps_day <- sapply(data1.splt.day, function(x) sum(x$steps, na.rm=T))

# Make a histogram of the total number of steps taken each day
hist(steps_day, breaks=20)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Calculate and report the mean and median total number of steps taken per day
mean(sapply(data1.splt.day, function(x) sum(x$steps, na.rm=T)))
```

```
## [1] 9354.23
```

```r
median(sapply(data1.splt.day, function(x) sum(x$steps, na.rm=T)))
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
data1.splt.interv <- split(data1, as.factor(data1$interval))
out <- sapply(data1.splt.interv, function(x) mean(x$steps, na.rm=T))

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
plot(as.numeric(names(out)), out, type="l", lwd=2,
     xlab="Interval", ylab="Steps (averaged over days)")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
c(interval=names(which.max(out)), round(max_steps=max(out),))
```

```
## interval          
##    "835"    "206"
```

## Imputing missing values

```r
# Calculate and report the total number of missing values in the dataset (i.e. 
# the total number of rows with NAs)
sum(is.na(data1$steps))
```

```
## [1] 2304
```

```r
# Devise a strategy for filling in all of the missing values in the dataset. The 
# strategy does not need to be sophisticated. For example, you could use the 
# mean/median for that day, or the mean for that 5-minute interval, etc.
missing <- is.na(data1$steps)
mean_interv <- sapply(data1.splt.interv, function(x) mean(x$steps, na.rm=T))

n_day <- length(levels(as.factor(data1$date)))
n_day
```

```
## [1] 61
```


```r
# Create a new dataset that is equal to the original dataset but with the missing 
# data filled in.
data1.imputed <- data1
data1.imputed[missing,]$steps <- rep(mean_interv, n_day)[missing]
cbind(head(data1[missing,]), head(data1.imputed[missing,]))
```

```
##   steps       date interval     steps       date interval
## 1    NA 2012-10-01        0 1.7169811 2012-10-01        0
## 2    NA 2012-10-01        5 0.3396226 2012-10-01        5
## 3    NA 2012-10-01       10 0.1320755 2012-10-01       10
## 4    NA 2012-10-01       15 0.1509434 2012-10-01       15
## 5    NA 2012-10-01       20 0.0754717 2012-10-01       20
## 6    NA 2012-10-01       25 2.0943396 2012-10-01       25
```


```r
# Make a histogram of the total number of steps taken each day and calculate and 
# report the mean and median total number of steps taken per day. Do these values 
# differ from the estimates from the first part of the assignment? What is the 
# impact of imputing missing data on the estimates of the total daily number of steps?

# Make a histogram of the total number of steps taken each day 
steps_day.imputed <- tapply(data1.imputed$steps, as.factor(data1$date), sum)
hist(steps_day.imputed)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
# Mean before and after imputation
c(before=mean(tapply(data1$steps, as.factor(data1$date), function(x) sum(x, na.rm=T))),
  after=mean(tapply(data1.imputed$steps, as.factor(data1.imputed$date), sum)))
```

```
##   before    after 
##  9354.23 10766.19
```

```r
# Median before and after imputation
c(before=median(tapply(data1$steps, as.factor(data1$date), function(x) sum(x, na.rm=T))),
  after=median(tapply(data1.imputed$steps, as.factor(data1.imputed$date), sum)))
```

```
##   before    after 
## 10395.00 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels – “weekday” and 
# “weekend” indicating whether a given date is a weekday or weekend day.
data1.imputed$day <- weekdays(as.Date(data1.imputed$date))
daylevel <- vector()

for (i in 1:nrow(data1)) {
        if (data1.imputed$day[i] == "Saturday") {
                daylevel[i] <- "Weekend"
        } else if (data1.imputed$day[i] == "Sunday") {
                daylevel[i] <- "Weekend"
        } else {
                daylevel[i] <- "Weekday"
        }
}

data1.imputed$daylevel <- daylevel
data1.imputed$daylevel <- factor(data1.imputed$daylevel)

stepsbyday <- aggregate(steps ~ interval + daylevel, data = data1.imputed, mean)
names(stepsbyday) <- c("interval", "daylevel", "steps")
```


```r
# Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis).

# install.packages("lattice")
library(lattice)
xyplot(steps ~ interval | factor(daylevel),
       data=stepsbyday,
       type = 'l',
       layout = c(1, 2),
       xlab="5-Minute Intervals",
       ylab="Average Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 




