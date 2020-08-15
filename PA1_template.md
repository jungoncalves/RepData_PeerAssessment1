---
title: 'Reproducible Research: Peer Assessment 1'
author: "jungoncalves"
date: "12 de agosto de 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

getwd()

library(glue)
library(ggplot2)
library(dplyr)


```


## Loading and preprocessing the data

```{r echo = TRUE}

df_activity <- read.csv('activity/activity.csv')

df_activity$date <- as.Date(df_activity$date, tryFormats = "%Y-%m-%d")

head(df_activity)

```

## What is mean total number of steps taken per day?

### 1. Make a histogram of the total number of steps taken each day

### 2. Calculate and report the mean and median total number of steps taken per day

```{r echo = TRUE}


stepsperday <- aggregate(steps ~ date, df_activity, sum, na.rm = TRUE)

head(stepsperday)

mean_steps <- round(mean(stepsperday$steps))
median_steps <- round(median(stepsperday$steps))

hist(stepsperday$steps, 
     breaks = 25, 
     main=glue('Steps per day with a mean of {mean_steps} and median of {median_steps}'),
     xlab="Steps per day")



```

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r echo = TRUE}

averageinter <- aggregate(steps ~ interval, df_activity, FUN = mean, na.rm = TRUE)

head(averageinter)

plot( averageinter$interval, averageinter$steps, type = 'l',
      xlab = 'Intervals',
      ylab = 'Average Steps', 
      main = 'Average daily activity pattern')



```

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r echo = TRUE}

max_interval_steps <- averageinter[which.max(  
        averageinter$steps),]

head(max_interval_steps)

print('The interval with the maximun number of steps is 835 with 206 steps')


```



## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r echo = TRUE}

missing <- sum(is.na(df_activity$steps))
print(glue('The total of missing values is {missing}')) 
```


### 2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```{r echo = TRUE}

print('The strategy is imputing the mean of intervals in the na values') 

```

### 3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r echo = TRUE}

df_imputed <- df_activity %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ averageinter$steps[match(df_activity$interval, averageinter$interval)],      
      TRUE ~ as.numeric(steps)
    ))

head(df_imputed)

```


### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r echo = TRUE}

df_imputed <- df_activity %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ averageinter$steps[match(df_activity$interval, averageinter$interval)],      
      TRUE ~ as.numeric(steps)
    ))

stepsperday_nona <- aggregate(steps ~ date, df_imputed, sum, na.rm = TRUE)

mean_steps_nona <- round(mean(stepsperday_nona$steps))
median_steps_nona <- round(median(stepsperday_nona$steps))

hist(stepsperday_nona$steps, 
     breaks = 25, 
     main=glue('Steps per day with a mean of {mean_steps_nona} and median of {median_steps_nona}'),
     xlab="Steps per day")


missing_after <- sum(is.na(df_imputed$steps))
print(glue('The total of missing values after imputing the mean of interval is {missing_after}'))


```

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r  echo = TRUE}

df_imputed$weekday <- ifelse(as.POSIXlt(df_imputed$date)$wday %in% c(0,6),"Weekend", "Weekday")

head(df_imputed)


```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r  echo = TRUE}


averageinter_imputed <- aggregate(steps ~ interval + weekday, df_imputed, mean )

head(averageinter_imputed)

ggplot(averageinter_imputed, aes(x=interval,y=steps, group = weekday)) + 
geom_line(color="blue") + 
facet_wrap(~weekday, nrow=2, ncol=1)+ 
ggtitle("Steps averaged across all weekday days or weekend days")+
labs(x="Interval",y="Steps")


```

