---
title: "Reproducible Research: Peer Assessment 1"
author: "Sharmistha Chakrabarti"
date: "November 25, 2016"
output: 
html_document:
keep_md: true
---


```r
library(knitr)
opts_chunk$set(echo = TRUE, fig.path='myFigFolder ')
```

## Loading and preprocessing the data

```r
activityData  <- read.csv("activity.csv", header =TRUE, sep =",", na.strings="NA")
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activityData$date  <- as.Date(activityData$date)       ## convert factor to date
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activityData_complete <- subset(activityData, !is.na(activityData$steps))     ## getting data without NA's
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day.

2. Make a histogram of the total number of steps taken each day.

3. Calculate and report the mean and median of the total number of steps taken per day.

**Methodology and Result:**


```r
## 1. Calculating the total number of steps taken per day.
totalStepsPerDay <- aggregate(activityData_complete$steps, by = list(activityData_complete$date), FUN = sum, na.rm = TRUE)
colnames(totalStepsPerDay) <- c("Date", "Total_Steps")
head(totalStepsPerDay, 10)
```

```
##          Date Total_Steps
## 1  2012-10-02         126
## 2  2012-10-03       11352
## 3  2012-10-04       12116
## 4  2012-10-05       13294
## 5  2012-10-06       15420
## 6  2012-10-07       11015
## 7  2012-10-09       12811
## 8  2012-10-10        9900
## 9  2012-10-11       10304
## 10 2012-10-12       17382
```

```r
## 2. Histogram of the total number of steps taken each day.
library(ggplot2)
ggplot(totalStepsPerDay, aes(x = Total_Steps)) + 
        geom_histogram(origin = 0, fill = "firebrick", binwidth = 1000) + 
        labs(title = "Histogram of daily steps (with NAs removed)", x = "Total steps per day", y = "Frequency")
```

```
## Warning: `origin` is deprecated. Please use `boundary` instead.
```

![plot of chunk total steps per day](myFigFolder total steps per day-1.png)

```r
## 3. Calculating the mean and median of the total number of steps taken per day.
mean_steps   <- mean(totalStepsPerDay$Total_Steps)
median_steps <- median(totalStepsPerDay$Total_Steps)
mean_steps; median_steps
```

```
## [1] 10766.19
```

```
## [1] 10765
```
*The mean and median total steps taken per day are 10766 and 10765, respectively.*

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

**Methodology and Result:**


```r
## 1. Calculating the average steps for each 5-minute inteval, averaged across all days.
avgStepsPerInterval <- aggregate(activityData_complete$steps, by = list(activityData_complete$interval), FUN = mean, na.rm = TRUE)
colnames(avgStepsPerInterval) <- c("Interval", "Avg_Steps")
head(avgStepsPerInterval, 10)
```

```
##    Interval Avg_Steps
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
```

```r
## 1.1. Plotting average steps for each 5-minute interval, averaged across all days.
ggplot(avgStepsPerInterval, aes(x = Interval, y = Avg_Steps)) +
        geom_line(color= "firebrick", size = 1) + 
        labs(title = "Average Daily Steps", x = "5-minute interval", y = "Avg. steps taken, averaged across all days")
```

![plot of chunk average steps per interval](myFigFolder average steps per interval-1.png)

```r
## 2. Finding the 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps.
max_AvgSteps <- avgStepsPerInterval[which.max(avgStepsPerInterval$Avg_Steps), ]
max_AvgSteps
```

```
##     Interval Avg_Steps
## 104      835  206.1698
```
*The interval 835, on average, has the highest count of steps, with approximately 206 steps.*

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

**Methodology and Result:**


```r
## 1. Calculating the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing_rows <- sum(is.na(activityData$steps))
missing_rows
```

```
## [1] 2304
```
*There are a total of 2304 rows with NAs in the original data set* 

2. Strategy for Imputing:

We will use a simple strategy for filling in all of the missing values in the dataset. If a 5-minute interval has missing value, we would use the mean for that 5-minute interval, averaged across all days.


```r
activityData_impute <- activityData             
index_NAs <- is.na(activityData_impute$steps)   ## finding and saving the indices of the missing values

## 3. Creating a new dataset that is equal to the original dataset but with the missing data filled in. 
avgStepsPerInterval <- tapply(activityData_impute$steps, activityData_impute$interval, mean, na.rm = TRUE, simplify = T)
str(avgStepsPerInterval)
```

```
##  num [1:288(1d)] 1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:288] "0" "5" "10" "15" ...
```

```r
activityData_impute$steps[index_NAs] <- avgStepsPerInterval[as.character(activityData_impute$interval[index_NAs])]
sum(is.na(activityData_impute$steps))   ## check if no missing values appear in the imputed data set.
```

```
## [1] 0
```

**BINGO!!**, the imputed data set contains no missing values, as expected.



```r
## 4.1. Calculating the total number of steps taken per day.
total_steps <- aggregate(activityData_impute$steps, by = list(activityData_impute$date), FUN = sum)
colnames(total_steps) <- c("Date", "Total_Steps")

## 4.2. Histogram of the total number of steps taken each day.
ggplot(total_steps, aes(x = Total_Steps)) + 
        geom_histogram(origin = 0, fill = "dark blue", binwidth = 1000) + 
        labs(title = "Histogram of daily steps (with NAs imputed)", x = "Total steps per day", y = "Frequency")
```

```
## Warning: `origin` is deprecated. Please use `boundary` instead.
```

![plot of chunk total steps per day for the imputed data](myFigFolder total steps per day for the imputed data-1.png)

```r
## 4.3. Calculating the mean and median of the total number of steps taken per day.
mean_steps_imputed <- mean(total_steps$Total_Steps)
median_steps_imputed <- median(total_steps$Total_Steps)
mean_steps_imputed; median_steps_imputed
```

```
## [1] 10766.19
```

```
## [1] 10766.19
```

**Impact of Imputing Missing Data :**

*The mean and median total steps taken per day for the imputed data are 10766 and 10766, respectively. The mean of the original data is 10766. There is no change in mean steps taken per day between the original and the imputed data sets. There has been a small increase in the median of the imputed data - from 10765 to 10766. It is notable that the mean and median of the imputed data are identical, implying that the distribution of the daily total steps is almost symmetric about the mean.*


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

**Methodology and Result:**


```r
## 1. Creating a new variable weektype for weekday and weekend.
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.1
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
activityData_impute <- mutate(activityData_impute, weektype = ifelse(weekdays(activityData_impute$date) == "Saturday" | weekdays(activityData_impute$date) == "Sunday", "weekend", "weekday"))
activityData_impute$weektype <- as.factor(activityData_impute$weektype)
str(activityData_impute)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weektype: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
## 2. Calculating the average steps for each 5-minute inteval, averaged across each weektype.
avg_interval <- aggregate(activityData_impute$steps, by = list(activityData_impute$interval, activityData_impute$weektype), FUN = mean)
colnames(avg_interval) <- c("interval", "weektype", "avg_steps")

## 2.2 Making a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
ggplot(avg_interval, aes(x = interval, y = avg_steps, color = weektype)) + 
        geom_line(size = 0.75) + 
        facet_wrap(~ weektype, ncol = 1, nrow =  2) + 
        labs(x = "Interval", y = "Number of steps")
```

![plot of chunk adding a column containing weektype](myFigFolder adding a column containing weektype-1.png)

From the panel plot it appears that the test object is more active earlier in the day during weekdays compared to weekends, but more active throughout the weekends compared with weekdays.

