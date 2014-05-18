# Reproducible Research: Peer Assessment 1

#Set the global options for the R markdown document to echo the code



#Installing the required external packages and loading them for use

```r
install.packages("zoo")
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library("zoo")
```

```
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```



## Loading and preprocessing the data

#Read data from the csv file from the working directory

```r
Activity <- read.csv("~/Rwork/activity.csv")
```


#Convert the columns into corresponding suitable formats, i.e., Date for date and numeric for the others

```r
Activity$steps <- sapply(Activity$steps, as.numeric)
Activity$interval <- sapply(Activity$interval, as.numeric)
Activity$date <- as.Date(Activity$date, "%Y-%m-%d")
```



## What is mean total number of steps taken per day?

#Computing the total number of steps in a day and plotting the histogram

```r
stepsDay <- tapply(Activity$steps, Activity$date, sum, simplify = TRUE)
hist(stepsDay, xlab = "steps per day", main = "Number of steps in a day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


#Computing the mean and the median of total number of steps per day

```r
median(stepsDay, na.rm = TRUE)
```

```
## [1] 10765
```

```r
mean(stepsDay, na.rm = TRUE)
```

```
## [1] 10766
```



## What is the average daily activity pattern?

#Computing the average number of steps at a given time interval across all days and plotting them as a line graph

```r
avgStepsPerTime <- tapply(Activity$steps, Activity$interval, mean, na.rm = T)
plot(avgStepsPerTime ~ Activity$interval[1:288], type = "l", xlab = "Time Interval", 
    ylab = "Average number of steps", xlim = c(0, 2400))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


#Identify the time interval that contains the maximum number of steps on an average

```r
intervalMax <- which(avgStepsPerTime == max(avgStepsPerTime))
names(intervalMax)
```

```
## [1] "835"
```


## Imputing missing values

#Total number of missing values in the dataset

```r
sum(is.na(Activity$steps))
```

```
## [1] 2304
```


#Replacing the NAs with the mean value of the time interval where NAs were originally found (using the 'zoo' package)

```r
Activity$steps <- na.aggregate(Activity$steps, by = Activity$interval, fun = mean)
```


#Calculating the new mean number of steps in a day and plotting the required histogram

```r
newStepsDay <- tapply(Activity$steps, Activity$date, sum, simplify = TRUE)
hist(newStepsDay, xlab = "steps per day", main = "Number of steps in a day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


#Computing the new mean and median total number of steps taken per day

```r
median(newStepsDay)
```

```
## [1] 10766
```

```r
mean(newStepsDay)
```

```
## [1] 10766
```


  There is no major difference between the mean and median before and after imputation of the missing values. This is because the missing number of steps for any interval was replaced with the mean value of steps for the corresponding interval across all the days. Effectively, this increased the sum of the number of steps for that interval by the mean value while simultaenously increasing the number of available indices (denominator) by 1, thereby leaving the total mean unchanged. Similarly, the median is unchanged because the missing values were filled for all the time steps almost uniformly (example, Day 1 was fully missing resulting in an uniform imputed value for the entire day) thereby not affecting the median in this case.

  The total number of steps have increased in those days that had missing values originally. Logically, this is sound since we can see that the number of steps (so imputed) cannot be negative and therefore should lead to an increase in the daily number of steps value.

## Are there differences in activity patterns between weekdays and weekends?

#Creating a factor variable in the dataset to represent whether it is a weekday or a weekend

```r
Activity$day <- weekdays(Activity$date)
Activity$weektype <- factor(ifelse(grepl("Sun|Sat", Activity$day) == TRUE, "weekend", 
    "weekday"))
```


#Create a planel plot to represent average number of steps per time interval for weekday and weekend using base plotting system


```r
par(mfrow = c(2, 1))
summary <- by(Activity$steps, Activity[, c(3, 5)], mean, simplify = TRUE)
plot(summary[, 2] ~ Activity$interval[1:288], type = "l", main = "weekend", 
    xlab = "Interval", ylab = "Number of Steps", ylim = c(0, 250))
plot(summary[, 1] ~ Activity$interval[1:288], type = "l", main = "weekday", 
    xlab = "Interval", ylab = "Number of Steps", ylim = c(0, 250))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


There is clearly a difference in the activity on an weekday and that on a weekend as illustrated by the above graphs.
