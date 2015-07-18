---
title: "5CourseProject1"
author: "The Squire"
date: "Friday, July 17, 2015"
output: html_document
---

First I will load a package to use the unzip function and then download the file from the internet and create a folder in the working directory if it doesn't already exist.  Finally I will set my working directory to my project folder


```r
library(downloader)

if (!file.exists("./5CourseProject1")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download(fileURL, dest = "5CourseProject1.zip", mode = "wb")
        unzip("5CourseProject1.zip", exdir = "./5CourseProject1")
        dateDownloaded <- date()
}
```

Now I will calculate the sum of steps by date and display those totals


```r
dat <- read.csv("./5CourseProject1/activity.csv", na.strings = "NA")

dateSteps <- tapply(dat$steps, dat$date, sum)
dateSteps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

Plot the frequency of days with steps in intervals of 5,000:


```r
hist(dateSteps, breaks = c(0, 5000, 10000, 15000, 20000, 25000), main = "Typical Daily Step Counts", xlab = "Total Steps", ylab = "Number of Days")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Display mean and median of the total number of steps taken per day


```r
mean(dateSteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(dateSteps, na.rm = TRUE)
```

```
## [1] 10765
```

Calculate mean at each time interval, show time series plot, and display max average


```r
dat$interval <- as.factor(dat$interval)
intervalSteps <- tapply(dat$steps, dat$interval, mean, na.rm = TRUE)
plot(intervalSteps, type = 'l', main = "Average Steps per 5 Minute Interval", xlab = "Number of 5 Minute Intervals", ylab = "Average Steps Taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
max(intervalSteps)
```

```
## [1] 206.1698
```

Calculate the number of NAs in steps measurements and then replace them with mean for that interval


```r
sum(is.na(dat$steps))
```

```
## [1] 2304
```

```r
datClean <- dat
for (i in 1:length(datClean$steps)){
        if (is.na(datClean$steps[i])){
                a <- as.character(datClean$interval[i])
                datClean$steps[i] <- intervalSteps[a]
        }
}
```

Plot the frequency of days with steps in intervals of 5,000 and display mean and median for number of steps per day:


```r
dateCleanSteps <- tapply(datClean$steps, datClean$date, sum)
hist(dateCleanSteps, breaks = c(0, 5000, 10000, 15000, 20000, 25000), main = "Typical Daily Step Counts", xlab = "Total Steps", ylab = "Number of Days")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
mean(dateCleanSteps)
```

```
## [1] 10766.19
```

```r
median(dateCleanSteps)
```

```
## [1] 10766.19
```

Calculate day of the week and plot weekday versus weekend steps


```r
datTemp <- datClean
datTemp$date <- as.character(datTemp$date)
datTemp$date <- as.POSIXlt(datTemp$date)
for (i in 1:length(datTemp$date)){
        datTemp$day[i] = weekdays(datTemp$date[i])
        if ((datTemp$day[i] == "Saturday") | (datTemp$day[i] == "Sunday")){
                datTemp$daycat[i] = "weekend"
        } else {
                datTemp$daycat[i] = "weekday"
        }
}
datTemp$daycat = as.factor(datTemp$daycat)

library(ggplot2)

weekSteps <- tapply(datTemp$steps, datTemp$interval, mean)
qplot(interval, steps, data = datTemp, facets = daycat~., main = "Weekday versus Weekend Steps", xlab = "Number of 5 Minute Intervals", ylab = "Average Steps Taken") + geom_line()
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
