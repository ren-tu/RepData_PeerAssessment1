--- 
title: "Reproducible Research Course Project 1" 
output: 
  html_document: 
    keep_md: true 
---


We start by loading libraries


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
library(lattice)
```

We then load the data from the working directory and clean the data by removing NA values and changing to appropriate data types


```r
data1 <- read.csv("activity.csv")
data1$date <- as.Date(data1$date)
data1$steps <- as.numeric(data1$steps)
data_clean <- data1[!is.na(data1$steps), ]
```

We can calculate the total daily steps and plot the totals on a histogram


```r
df_steps <- data_clean %>% group_by(date) %>% summarise(total_steps = sum(steps))
hist(df_steps$total_steps, 
     main="Histogram of Total Daily Steps", 
     xlab="Total Daily Steps", col="blue")
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Here is the mean total daily steps


```r
df_steps_mean <- mean(df_steps$total_steps)
df_steps_mean
```

```
## [1] 10766.19
```

Here is the median total daily steps


```r
df_steps_median <- median(df_steps$total_steps)
df_steps_median
```

```
## [1] 10765
```

We can calculate average steps per time interval across all days on plot results on line plot


```r
df_intervals <- data_clean %>% group_by(interval) %>% summarise(mean_steps = mean(steps))
plot(df_intervals$interval, df_intervals$mean_steps, type="l",
     main="Mean Steps Across Intervals",
     xlab="Intervals", ylab="Mean Steps")
```

![](PA1_Template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Here is the 5-minute interval that on average contains the max number of steps


```r
max_interval <- df_intervals$interval[df_intervals$mean_steps==max(df_intervals$mean_steps)]
max_interval
```

```
## [1] 835
```

We can find the total number of missing values in the dataset


```r
missing_total <- sum(is.na(data1$steps))
missing_total
```

```
## [1] 2304
```

We can impute the missing values by using the mean steps at specific intervals 


```r
data_na <- data1[is.na(data1$steps), ]
data_na2 <- left_join(data_na, df_intervals, by="interval")
data_na2 <- data_na2[c("mean_steps", "date", "interval")]
names(data_na2) <- c("steps", "date", "interval")
```

Here is the fully cleaned original dataset by adding back the imputed data_na2 dataset


```r
df_final <- rbind(data_clean, data_na2)
df_final <- arrange(df_final, date, interval)
```

With the fully cleaned dataset, we can calculate the total daily steps and plot the totals on a histogram


```r
df_steps_clean <- df_final %>% group_by(date) %>% summarise(total_steps = sum(steps))
hist(df_steps_clean$total_steps, 
     main="Histogram of Total Daily Steps", 
     xlab="Total Daily Steps", col="blue")
```

![](PA1_Template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Here is the new mean total daily steps


```r
df_steps_clean_mean <- mean(df_steps_clean$total_steps)
df_steps_clean_mean
```

```
## [1] 10766.19
```

Here is the new median total daily steps


```r
df_steps_clean_median <- median(df_steps_clean$total_steps)
df_steps_clean_median
```

```
## [1] 10766.19
```

Overall, imputing had minimal impact on total daily step estimates. The mean stayed the same and the median increased a tiny bit to equal the mean.

Let's take a look at differences in weekday/weekend activity patterns. We can begin by creating a new day_of_week factor variable for the dataset


```r
check_day <- function(day) {
  weekend_days <- c("Saturday", "Sunday")
  if(is.element(day, weekend_days)) { result <- "weekend" }
  else { result <- "weekday" }
  return(result)
}

df_final$day_of_week <- mapply(check_day, weekdays(df_final$date))
df_final$day_of_week <- as.factor(df_final$day_of_week)
```

Here is a panel plot of mean steps by interval across weekday and weekend days


```r
df_intervals_clean <- df_final %>% group_by(interval, day_of_week) %>% summarise(mean_steps = mean(steps))
xyplot(mean_steps ~ interval | day_of_week, type="l", data=df_intervals_clean, layout=c(1,2),
       main="Mean Steps Across Intervals",
       xlab="Intervals", ylab="Mean Steps")
```

![](PA1_Template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
