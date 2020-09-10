---
title: "Reproducible Research: Peer Assessment 1"
author: "Eugene"
date: "9/10/2020"
output: 
  html_document:
    keep_md: true
  
---



## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")
sort<- with(data, tapply(steps, date, sum, na.rm = TRUE))
df_steps <- data.frame(date = names(sort), steps = sort)
n_steps <- df_steps$steps
```

## What is mean total number of steps taken per day?

First, a histogram of the total number of steps is generated.

```r
f <- hist(n_steps, xlab = "Number of steps", ylim = c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Then, the median and mean total number of steps per day is generated.

```r
summary <- summary(n_steps)
median <- summary[[3]]
mean <- summary[[4]]
print(paste0("The median is ", median, ". The mean is ", mean))
```

```
## [1] "The median is 10395. The mean is 9354.22950819672"
```

## What is the average daily activity pattern?

First, the data is preprocessed. 

```r
sort<- with(data, tapply(steps, interval, mean, na.rm = TRUE))
df_steps <- data.frame(interval = names(sort), steps = sort)
```

Then, the time-series plot is generated of the average number of steps taken, averaged across all days.


```r
g <- plot(df_steps$interval, df_steps$steps, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5 minute interval, on average across all days, contains the maximum number of steps?

```r
newdf <- df_steps[which(df_steps$steps == max(df_steps$steps)),]
print(newdf$interval)
```

```
## [1] "835"
```

## Imputing missing values

First, the number of missing data is reported.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
nas <- data[is.na(data)]
num_na <- length(nas)
print(paste0("The number of NAs is ", num_na))
```

```
## [1] "The number of NAs is 2304"
```

Then, a new dataset is created, by replacing the missing data with the mean for that 5-minute interval. This is done using a function.


```r
sort<- with(data, tapply(steps, interval, mean, na.rm = TRUE))
df_steps <- data.frame(interval = names(sort), steps = sort)
missingdata <- data[!complete.cases(data),]
completedata <- data[complete.cases(data),]
missingdf <- data.frame(steps = missingdata$steps, date = missingdata$date, interval = missingdata$interval) 


for (i in (1:nrow(missingdf))) {
  interval <- as.numeric(missingdf[i,]["interval"])
  row <- df_steps[(df_steps$interval == interval), ]
  missingdf[i,]["steps"] <- row["steps"]
}

newdf <- rbind(missingdf, completedata)
newdf <- newdf[order(newdf$date),]
```

Next, a histogram of the total number of steps per day is generated. 


```r
sort<- with(newdf, tapply(steps, date, sum))
df_sort <- data.frame(date = names(sort), steps = sort)
n_steps <- df_sort$steps
f <- hist(n_steps, xlab = "Number of steps", ylim = c(0,40))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Then, the mean and median total number of steps per day is calculated.


```r
summ <- summary(n_steps)
median <- summ[[3]]
mean <- summ[[4]]
print(paste0("The median is ", median, ". The mean is ", mean))
```

```
## [1] "The median is 10766.1886792453. The mean is 10766.1886792453"
```

Yes, the mean and median differs from that of the original data set containing missing data. Now they are both higher.

## Are there differences in activity patterns between weekdays and weekends?

First, the data set is sorted and split into two datasets, one for weekdays and one for weekends.


```r
newdf$date <- as.POSIXct(newdf$date)
newdf$days <- sapply(newdf$date,weekdays)

daytype <- function(x){
  if (x %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
    return("Weekday")
  }
  if (x %in% c("Saturday", "Sunday")){
    return ("Weekend")
  }
}

newdf$dayt <- sapply(newdf$days,daytype)
weekdaydf <- subset(newdf, dayt == 'Weekday')
weekenddf <- subset(newdf, dayt == 'Weekend')
```

After some processing, the time-series plots, averaged across all weekday days and weekend days, are generated.


```r
wdsort<- with(weekdaydf, tapply(steps, interval, mean))
wesort <- with(weekenddf, tapply(steps, interval, mean))

wddf <- data.frame(interval = names(wdsort), steps = wdsort)
wedf <- data.frame(interval = names(wesort), steps = wesort)

par(mfrow = c(1,2))
k <- plot(wddf$interval, wddf$steps, type = "l", ylab = "Number of steps", xlab = "Interval")
l <- plot(wedf$interval, wedf$steps, type = "l", ylab = "Number of steps", xlab = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
