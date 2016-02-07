# Reproducible Research: Peer Assessment 1


* Global Settings

```r
echo = TRUE
```


* Required Libraries

```r
library(ggplot2)
library(lattice)
```


## Loading and preprocessing the data
* 1. Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
* 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
unzip("activity.zip")
activity_data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
activity_data$month <- as.numeric(format(activity_data$date, "%m"))
filtered_activity_data <- na.omit(activity_data)
rownames(filtered_activity_data) <- 1:nrow(filtered_activity_data)
head(filtered_activity_data)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(filtered_activity_data)
```

```
## [1] 15264     4
```

## What is mean total number of steps taken per day?
* 1. Calculate the total number of steps taken per day
* 2. Make a histogram of the total number of steps taken each day

```r
ggplot(filtered_activity_data, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Total number of steps taken each day", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  
* 3.Calculate and report the mean and median of the total number of steps taken per day

Mean total number of steps taken per day,

```r
total_steps_per_day <- aggregate(filtered_activity_data$steps, list(Date = filtered_activity_data$date), FUN = "sum")$x
mean(total_steps_per_day)
```

```
## [1] 10766.19
```

Median total number of steps taken per day,

```r
median(total_steps_per_day)
```

```
## [1] 10765
```

  
## What is the average daily activity pattern?
* 1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_steps <- aggregate(filtered_activity_data$steps, list(interval = as.numeric(as.character(filtered_activity_data$interval))), FUN = "mean")
names(avg_steps)[2] <- "mean_of_steps"

ggplot(avg_steps, aes(interval, mean_of_steps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "5-minute Interval", x = "5-minute intervals", y = "Average number of steps taken", type = "1")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


* 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avg_steps[avg_steps$mean_of_steps == max(avg_steps$mean_of_steps), ]
```

```
##     interval mean_of_steps
## 104      835      206.1698
```



## Imputing missing values
* 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
sum(is.na(activity_data))
```

```
## [1] 2304
```


* 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_data_na_replaced <- activity_data 
for (i in 1:nrow(activity_data_na_replaced)) {
    if (is.na(activity_data_na_replaced$steps[i])) {
        activity_data_na_replaced$steps[i] <- avg_steps[which(activity_data_na_replaced$interval[i] == avg_steps$interval), ]$mean_of_steps
    }
}

head(activity_data_na_replaced)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(activity_data_na_replaced))
```

```
## [1] 0
```


* 3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
ggplot(activity_data_na_replaced, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue",width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day ", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
head(activity_data_na_replaced)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
activity_data_na_replaced$weekdays <- factor(format(activity_data_na_replaced$date, "%A"))
levels(activity_data_na_replaced$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(activity_data_na_replaced$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
levels(activity_data_na_replaced$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(activity_data_na_replaced$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avg_steps <- aggregate(activity_data_na_replaced$steps, list(interval = as.numeric(as.character(activity_data_na_replaced$interval)), weekdays = activity_data_na_replaced$weekdays), FUN = "mean")
names(avg_steps)[3] <- "mean_of_steps"
xyplot(avg_steps$mean_of_steps ~ avg_steps$interval | avg_steps$weekdays, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
