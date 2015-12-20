# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
1. Calculate the total number of the steps taken per day.

```r
stepsPerDay <- aggregate(steps ~ date, data = activity, FUN = sum)
```

2. Make a histogram of the total number of steps taken each day.

3. Calculate and report the mean and median of the total number of stepshis taken per day.

```r
mean(stepsPerDay$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
