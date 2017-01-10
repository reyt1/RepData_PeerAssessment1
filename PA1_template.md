---
title: 'Reproducible Research: Peer Assessment 1'
author: "Reynaldo"
date: "10 de enero de 2017"
output: html_document
---

Reproducible Research: Peer Assessment 1
========================================



1.- Loading and preprocessing the data

```r
setwd("C:/Users/Reynaldo/Documents/Rsearch")
library(knitr)
opts_chunk$set(echo=TRUE,results="show",cache=TRUE)

if(!file.exists("Rsearch")) dir.create("Rsearch")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./Rsearch/activity.zip")
unzip("./Rsearch/activity.zip", exdir = "./Rsearch")

activity <- read.table("activity.csv",
               header=TRUE,
               sep=",",
               stringsAsFactors = FALSE,
               colClasses = c("numeric","Date","numeric")
               )

str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity)
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

2.- What is mean total number of steps taken per day?

```r
steps.date <- aggregate(steps~date,activity,sum)

hist(steps.date$steps, 	col="red",
				xlab="Total steps by day", 
				ylab="Frequency [Days]",
				main="Histogram : Number of daily steps",
				xlim=c(0,25000),
				breaks= seq(from=0, to =25000,by=2000))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
##Calculate and report the mean and median of the total number of steps taken per day
median(steps.date$step, na.rm=TRUE)
```

```
## [1] 10765
```

```r
mean(steps.date$step, na.rm=TRUE)
```

```
## [1] 10766.19
```
3.- What is the average daily activity pattern?

```r
steps.interval <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(steps.interval, type="l",
				xlab="interval [in 5min]", 
				ylab="Average daily activity pattern of steps",  
				main="Time Series Plot: Average Number of Steps",
				xlim=c(0,2500))	
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

4.- Imputing missing values

```r
##Calculate and report the total number of missing values in the dataset
sum(is.na(activity))
```

```
## [1] 2304
```

```r
##Create a new dataset that is equal to the original dataset but with the missing data
activity.merged = merge(activity, steps.interval, by="interval")
activity.merged$steps.x[is.na(activity.merged$steps.x)] = activity.merged$steps.y[is.na(activity.merged$steps.x)]

##Make a histogram of the total number of steps taken each day 
activity.merged <- aggregate(steps.x~interval,activity.merged,sum)
hist(activity.merged$steps.x, xlab="Total Steps by Day", 
					ylab="Frequency [Days]",
					main="Histogram : Number of daily steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
##Calculate and report the mean and median total number of steps taken per day
mean2 <- mean(activity.merged$steps, na.rm=TRUE)
median2 <- median(activity.merged$steps, na.rm=TRUE)
```
5.- Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(RColorBrewer)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
library(ggthemes)
library(scales)

##Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
activity$Interval <- as.POSIXct(strptime(sprintf("%04d", activity$interval), "%H%M")) 
activity$date<-as.Date(activity$date)
activity$Weekday<-wday(activity$date, label = TRUE, abbr = FALSE)
imputed<-mutate(activity, Weekend = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
head(imputed, 3)
```

```
##   steps       date interval            Interval Weekday Weekend
## 1    NA 2012-10-01        0 2017-01-10 00:00:00  Monday Weekday
## 2    NA 2012-10-01        5 2017-01-10 00:05:00  Monday Weekday
## 3    NA 2012-10-01       10 2017-01-10 00:10:00  Monday Weekday
```

```r
table(imputed$Weekend, imputed$Weekday)
```

```
##          
##           Sunday Monday Tuesday Wednesday Thursday Friday Saturday
##   Weekday      0   2592    2592      2592     2592   2592        0
##   Weekend   2304      0       0         0        0      0     2304
```

```r
##Make a panel plot containing a time series plot
make.Weekend.ggplot<- function(active.dataframe){
    
  #Transmform the data and get averages.
    
    active.intervals <- active.dataframe %>%
    group_by(Weekend, Interval) %>%
    summarise(Average = mean(steps, na.rm = TRUE))
  
  active.intervals<- cbind(active.intervals)
  ggplot(active.intervals, aes(x = Interval, 
                               y = Average, 
                               group = Weekend, 
                               color = Weekend)) +
    theme_solarized() +
    geom_line() +
    facet_grid(Weekend~.) +
    theme(axis.text.x=element_text(angle=270,hjust=1,vjust=0.5, size = 10)) + 
    scale_x_datetime(breaks = date_breaks("30 mins"),
                     labels = date_format("%H:%M")) +
    ylab("Average steps") + 
    xlab("5-minute Time Intervals (Labeled in chunks of 30-minutes)") +
    theme(legend.position="none")
  
}

make.Weekend.ggplot(imputed)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
knitr2html("PA1_template.Rmd")
