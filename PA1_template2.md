# Reproducible Research: Peer Assessment 1

---



## Loading and preprocessing the data

Read in the data.

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
```

```
## Warning: error 1 in extracting from zip file
```

```r
activity <- read.csv('activity.csv')
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

Create a date.time column that combines the date and interval columns.

```r
time <- formatC(activity$interval / 100, 2, format='f')
```

```
## Error: object 'activity' not found
```

```r
activity$date.time <- as.POSIXct(paste(activity$date, time),
                                 format='%Y-%m-%d %H.%M',
                                 tz='GMT')
```

```
## Error: object 'activity' not found
```

For analyzing the means at the different times of day, it will also be convenient to have a time column. To do this, I convert all of the dates to be for today. since we only care about the time for that column, it will help us with the analysis.

```r
activity$time <- format(activity$date.time, format='%H:%M:%S')
```

```
## Error: object 'activity' not found
```

```r
activity$time <- as.POSIXct(activity$time, format='%H:%M:%S')
```

```
## Error: object 'activity' not found
```

## What is mean total number of steps taken per day?

First, calculate the mean number of steps for each day:

```r
total.steps <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```

```
## Error: object 'activity' not found
```

Let's look at the mean and median for the total steps per day:

```r
mean(total.steps)
```

```
## Error: object 'total.steps' not found
```

```r
median(total.steps)
```

```
## Error: object 'total.steps' not found
```

And let's take a look at the distribution of total number of steps per day with a histogram:

```r
library(ggplot2)
qplot(total.steps, xlab='Total steps', ylab='Frequency')
```

```
## Error: object 'total.steps' not found
```


## What is the average daily activity pattern?

Calculate the mean steps for each five minute interval, and then put it in a data frame.

```r
mean.steps <- tapply(activity$steps, activity$time, mean, na.rm=TRUE)
```

```
## Error: object 'activity' not found
```

```r
daily.pattern <- data.frame(time=as.POSIXct(names(mean.steps)),
                            mean.steps=mean.steps)
```

```
## Error: object 'mean.steps' not found
```

Let's take a look at a time series plot for the mean steps.

```r
library(scales)
ggplot(daily.pattern, aes(time, mean.steps)) + 
    geom_line() +
    xlab('Time of day') +
    ylab('Mean number of steps') +
    scale_x_datetime(labels=date_format(format='%H:%M'))
```

```
## Error: object 'daily.pattern' not found
```

Which five minute interval has the highest mean number of steps?

```r
most <- which.max(daily.pattern$mean.steps)
```

```
## Error: object 'daily.pattern' not found
```

```r
format(daily.pattern[most,'time'], format='%H:%M')
```

```
## Error: object 'daily.pattern' not found
```


## Imputing missing values
Identify the number of intervals with missing step counts ("NA's"):

```r
summary(activity$steps)
```

```
## Error: object 'activity' not found
```

To fill in the missing values, I'll use mean steps for a five-minute interval for the entire dataset.

```r
library(Hmisc)
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: splines
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
activity.imputed <- activity
```

```
## Error: object 'activity' not found
```

```r
activity.imputed$steps <- with(activity.imputed, impute(steps, mean))
```

```
## Error: object 'activity.imputed' not found
```

Let's compare the mean and median steps for each day between the original data set and the imputed data set.

```r
total.steps.imputed <- tapply(activity.imputed$steps, 
                              activity.imputed$date, sum)
```

```
## Error: object 'activity.imputed' not found
```

```r
mean(total.steps)
```

```
## Error: object 'total.steps' not found
```

```r
mean(total.steps.imputed)
```

```
## Error: object 'total.steps.imputed' not found
```

```r
median(total.steps)
```

```
## Error: object 'total.steps' not found
```

```r
median(total.steps.imputed)
```

```
## Error: object 'total.steps.imputed' not found
```

And a histogram of the imputed dataset.

```r
qplot(total.steps.imputed, xlab='Total steps', ylab='Frequency')
```

```
## Error: object 'total.steps.imputed' not found
```

Imputing the missing data has increased the average number of steps. 

## Are there differences in activity patterns between weekdays and weekends?

Add a factor column for whether a day is a weekday or weekend.

```r
day.type <- function(date) {
    if (weekdays(date) %in% c('Saturday', 'Sunday')) {
        return('weekend')
    } else {
        return('weekday')
    }
}

day.types <- sapply(activity.imputed$date.time, day.type)
```

```
## Error: object 'activity.imputed' not found
```

```r
activity.imputed$day.type <- as.factor(day.types)
```

```
## Error: object 'day.types' not found
```

Create a dataframe that holds the mean steps for weekdays and weekends.

```r
mean.steps <- tapply(activity.imputed$steps, 
                     interaction(activity.imputed$time,
                                 activity.imputed$day.type),
                     mean, na.rm=TRUE)
```

```
## Error: object 'activity.imputed' not found
```

```r
day.type.pattern <- data.frame(time=as.POSIXct(names(mean.steps)),
                               mean.steps=mean.steps,
                               day.type=as.factor(c(rep('weekday', 288),
                                                   rep('weekend', 288))))
```

```
## Error: object 'mean.steps' not found
```

Now let's compare the patterns between weekdays and weekends.

```r
ggplot(day.type.pattern, aes(time, mean.steps)) + 
    geom_line() +
    xlab('Time of day') +
    ylab('Mean number of steps') +
    scale_x_datetime(labels=date_format(format='%H:%M')) +
    facet_grid(. ~ day.type)
```

```
## Error: object 'day.type.pattern' not found
```
