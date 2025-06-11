---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


``` r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?


``` r
library(dplyr)
daily_steps <- activity %>%
  group_by(date) %>%
  summarize(total = sum(steps, na.rm = TRUE))
```


``` r
hist(daily_steps$total, main = "Histogram of Total Steps per Day", col = "skyblue", xlab = "Steps")
```

![](PA1_template_files/figure-html/hist-steps-per-day-1.png)<!-- -->


``` r
mean_steps <- mean(daily_steps$total)
median_steps <- median(daily_steps$total)
mean_steps
```

```
## [1] 9354.23
```

``` r
median_steps
```

```
## [1] 10395
```

## What is the average daily activity pattern?


``` r
interval_avg <- activity %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))
```


``` r
plot(interval_avg$interval, interval_avg$mean_steps, type = "l", col = "blue", 
     main = "Average Daily Activity Pattern", xlab = "5-minute Interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/plot-interval-1.png)<!-- -->


``` r
interval_avg[which.max(interval_avg$mean_steps), ]
```

```
## # A tibble: 1 × 2
##   interval mean_steps
##      <int>      <dbl>
## 1      835       206.
```

## Imputing missing values


``` r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


``` r
activity_imputed <- activity
for (i in which(is.na(activity_imputed$steps))) {
  interval_val <- activity_imputed$interval[i]
  activity_imputed$steps[i] <- interval_avg$mean_steps[interval_avg$interval == interval_val]
}
```

### New histogram, mean, median with imputed data


``` r
daily_steps_imputed <- activity_imputed %>%
  group_by(date) %>%
  summarize(total = sum(steps))
```


``` r
hist(daily_steps_imputed$total, main = "Histogram with Imputed Data", col = "orange", xlab = "Steps")
```

![](PA1_template_files/figure-html/hist-imputed-1.png)<!-- -->


``` r
mean_imputed <- mean(daily_steps_imputed$total)
median_imputed <- median(daily_steps_imputed$total)
mean_imputed
```

```
## [1] 10766.19
```

``` r
median_imputed
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


``` r
activity_imputed$day <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity_imputed$day <- factor(activity_imputed$day)
```


``` r
interval_day_avg <- activity_imputed %>%
  group_by(interval, day) %>%
  summarize(mean_steps = mean(steps), .groups = "drop")
```


``` r
library(lattice)
xyplot(mean_steps ~ interval | day, data = interval_day_avg, type = "l",
       layout = c(1, 2), xlab = "Interval", ylab = "Steps",
       main = "Activity Patterns: Weekday vs Weekend")
```

![](PA1_template_files/figure-html/plot-weekday-weekend-1.png)<!-- -->

