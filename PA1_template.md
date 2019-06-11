---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading packages
First, let's load all packages that we make use of for this assignment.

```r
library(tidyverse)
```
FOR OUTPUT MANIPULATION
## Loading and preprocessing the data
UNZIPPED DATA ARE READ FROM HERE

```r
data <- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
str(data)
```

```
## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```
The `read_csv` command automatically recognizes the variables as containing double and data values. There does not seem to be a need to further tidy up the data.

## What is mean total number of steps taken per day?
As the data is provided for 5-minute intervalls, we first have to sum up the steps taken for each day before we can plot the data.

```r
daily <- data %>% group_by(date) %>% 
    summarise(StepsPerDay = sum(steps))

ggplot(data = daily, aes(x = StepsPerDay)) + 
    geom_histogram() + 
    labs(title = "Histogramm of Average Steps per Day", x = "Steps Per Day")
```

![](PA1_template_files/figure-html/daily_steps1-1.png)<!-- -->

The histogramm shows that, on most days, around 10000 steps have been recorded.

This is underlined when looking at summary statistics.


```r
summary(daily$StepsPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```
A PLAIN SUMMARY OF STEPS PER DAY

## What is the average daily activity pattern?
In order to find a daily pattern we can look at the average steps taken, grouped by the provided 5-minute intervalls.


```r
pattern <- data %>% group_by(interval) %>% 
    summarise(StepsPerInt = mean(steps, na.rm = TRUE))

ggplot(data = pattern, aes(x = interval, y = StepsPerInt)) + 
    geom_line() +
    labs(title = "Average Steps Taken Over The Course of a Day", 
        x = "5-Minute Interval", y = "Average Number of Steps per Interval")
```

![](PA1_template_files/figure-html/daily_pattern-1.png)<!-- -->

A SPIKE IS OBSERVABLE AT 800


```r
max(pattern$StepsPerInt)
```

```
## [1] 206.1698
```
The maximum number of average steps taken for a 5-minute intervall is 206.


```r
pattern$interval[which(pattern$StepsPerInt == max(pattern$StepsPerInt))]
```

```
## [1] 835
```
The interval associated with this value is interval number 835.


## Imputing missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

FILLING THE 2304 MISSING VALUES IN THE DATASET


```r
getIntervalMean <- function(interval) {
    pattern[pattern$interval == interval,]$StepsPerInt
}
```

Now we can simply loop over all rows and replace missing values with the mean number of steps for the respective interval. We will store the complete dataset in a new object.


```r
data_complete <- data
for (i in 1:length(data_complete$steps)) {
    if(is.na(data_complete$steps[i])) {
        data_complete$steps[i] <- getIntervalMean(data_complete$interval[i])
    }
}
```
Let's inspect the filled in dataset...



```r
daily_imputed <- data_complete %>% group_by(date) %>% 
    summarise(StepsPerDay = sum(steps))

ggplot(data = daily_imputed, aes(x = StepsPerDay)) + 
    geom_histogram() + 
    labs(title = "Histogramm of Average Steps per Day", x = "Steps Per Day")
```

![](PA1_template_files/figure-html/daily_steps_imputed-1.png)<!-- -->

```r
summary(daily_imputed$StepsPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```
THE PLOT AND SUMMARY HAS ALMOST SAME MESSAGE

## Are there differences in activity patterns between weekdays and weekends?

ADDING A FACTOR VARIABLE TO DETECT A PATTERN


```r
weekend_fun <- function(weekday) {
    if(weekday %in% c("mondays", "tuesdays", "wednesdays", "thursdays", "fridays")) {
        return(0)
    }
    else if(weekday %in% c("egwuregwu", "ubosi uka")) {
        return(1)
    }
}

data_complete$weekend <- sapply(weekdays(data_complete$date), weekend_fun)
data_complete$weekend <- factor(data_complete$weekend)
```

we compare the weekday and weekend patterns


```r
pattern_weekend <- data_complete %>% group_by(weekend,interval) %>% 
    summarise(StepsPerInt = mean(steps, na.rm = TRUE))

ggplot(data = pattern_weekend, aes(x = interval, y = StepsPerInt)) + 
    geom_line() +
    facet_grid(rows = vars(weekend)) +
    labs(title = "Average Steps Taken Over The Course of a Day", 
        x = "5-Minute Interval", y = "Average Number of Steps per Interval")
```

![](PA1_template_files/figure-html/patterns_weekend-1.png)<!-- -->

FROM THE PATTERN ANALYSIS, STEPS ARE MORE DURING WEEKDAYS THAN WEEKENDS
UKACHUKWU CHRISTIAN_SPIRITUAL
