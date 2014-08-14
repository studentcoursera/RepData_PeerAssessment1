# Reproducible Research: Peer Assessment 1
Title:  PA1_template.Rmd                         

## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv", colClasses = c("integer","character","integer"),
                     comment="",stringsAsFactors = FALSE, nrows = 17568)
```

## What is mean total number of steps taken per day?

```r
library(reshape2)
library(plyr)

completeactvity <- activity[complete.cases(activity$steps),]
totalsteps <- ddply(completeactvity, .(date), summarize, total=sum(steps))
```
#### Total number of steps taken each day

```r
### With "breaks=50", the 8 days difference (during comparison) is evident. 
hist(totalsteps$total, breaks=50, col="lightgreen", xlab='Total Steps', ylab = "Days", 
     main = "Total number of steps taken each day")
```

![plot of chunk plotHist](figure/plotHist.png) 
#### MEAN and MEDIAN

```r
mn1 <- mean(totalsteps$total)
md1 <- median(totalsteps$total)
```
Total number of steps taken per day: Mean and Median are 10766 and 10765 respectively.

## What is the average daily activity pattern?

```r
## Average and median of steps for every interval (averaged across all days)
summ <- ddply(completeactvity, .(interval), summarize, mn=mean(steps), md=median(steps))
## To get the time interval in hrs:mins format 
interval2hrsmins <- format(strptime(paste((summ$interval  %/% 100), 
                                      (summ$interval %% 100)),"%H%M"), "%H:%M")
```

#### Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(summ$mn ~ summ$interval, type = "l", xlab = "5-minutes interval",
     ylab = "Average steps", main = "Time series plot of the 5-minute interval", col="blue", xaxt = "n")
axis(1, at=summ$interval, labels=interval2hrsmins)
```

![plot of chunk plotL](figure/plotL.png) 

#### Max number of steps in 5 minutes interval

```r
max5min <- c(summ$interval[which.max(summ$mn)],interval2hrsmins[which.max(summ$mn)])
```
The 5-minute interval which on average across all the days in the dataset, contains the maximum number of steps is 835 [08:35].

## Imputing missing values

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
The total number of missing values [steps] in the dataset is 2304. 
  
#### Strategy to fill in the missing values
A new data set with the NA values in 'number of steps' be filled with the median value for each interval of completed cases.  

```r
filledData <- merge(activity, summ, by.x = "interval", by.y = "interval", all = TRUE)
filledData$steps[is.na(filledData$steps)] <- filledData$md[is.na(filledData$steps)]
reorderFilledData <- filledData[order(filledData$date,filledData$interval),c(2,3,1)]
totalfilledsteps <- ddply(reorderFilledData, .(date), summarize, total=sum(steps))
```

#### Total number of steps taken each day (after filling missing values)

```r
### With "breaks=50", the 8 days difference (during comparison) is evident.
hist(totalfilledsteps$total, breaks=50, col="lightpink", xlab='Total Steps', ylab = "Days",  
     main = "Total number of steps taken each day \n (after filling missing values)")
```

![plot of chunk plotFillHist](figure/plotFillHist.png) 
#### MEAN and MEDIAN

```r
mn2 <- mean(totalfilledsteps$total)
md2 <- median(totalfilledsteps$total)
```
Total number of steps taken per day: Mean and Median are 9504 and 10395 respectively.

#### Do these values differ from the estimates from the first part of the assignment? 
* YES, they do.  Decrease in mean and median by 11.7249% and 3.4371% respectively.

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?  
Following are the observations:   
* Mean and Median: Before vs After [imputing missing data]
      * Mean: 10766 vs 9504  [-11.7249%]
      * Median : 10765 vs 10395 [-3.4371%]
* As per the 2 histograms comparison, there are about 8 days difference in the range of 1000 to 2000 [exactly 1141] total steps and this is clearly reflected with "breaks = 50" in histogram plots..  
   * Each date and the corresponding number of steps for NAs, after imputing the missing values
      
      ```
      ## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
      ##       1141       1141       1141       1141       1141       1141 
      ## 2012-11-14 2012-11-30 
      ##       1141       1141
      ```

 
## Are there differences in activity patterns between weekdays and weekends?

```r
weekend <- weekdays(as.Date(reorderFilledData$date),TRUE) %in% c("Sat", "Sun")
reorderFilledData$day <- factor(ifelse(weekend, "weekend", "weekday"))

# Average the steps on interval and day
dayavg <- ddply(reorderFilledData, .(interval, day), summarize, avgsteps=mean(steps))
```

```r
library(lattice)
xyplot(avgsteps ~ interval | day, data = dayavg, type="l", layout=c(1,2),xlab="Interval", 
       ylab="Number of steps", main="Activity patterns between weekdays and weekends")
```

![plot of chunk weeksplot](figure/weeksplot.png) 


