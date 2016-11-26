---
title: "Project 1"
author: "Kenneth Fleming"
date: "11/25/2016"
output: html_document
---
Read in the data (assuming the activity.csv file is in the current working directory) and summarize the data.

```{r readdata}
activity <- read.csv("activity.csv")
summary(activity)
```

Transform the 'date' column into a date field.  We won't show the results since the date transformation is very long.

```{r date, results='hide'}
activity$date <- as.Date(activity$date, "%m/%d/%y")
```

Aggregate the steps for each day and create a histogram showing the frequency of various step totals

```{r aggregate}
stepsperday <- aggregate(steps ~ date, data=activity, sum)
hist(stepsperday$steps, main = "Histogram of Steps Per Day", xlab = "Total Steps", ylab = "Number of Days")
```

Calculate the mean and median.

```{r mean}
mean(stepsperday$steps)
median(stepsperday$steps)
```

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r timeseries}
intervaldata <- aggregate(steps ~ interval, data=activity, mean)
plot(intervaldata, type = "l")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r max}
largest <- which.max(intervaldata$steps)
max <- intervaldata[largest,]
max$interval
```

Calculate and report the total number of missing values in the dataset.

```{r count}
 sum(is.na(activity))
```

Devise a strategy for filling in all of the missing values in the dataset.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

The code creates a new data set then loops through the data and replaces any 'NA' values with the mean of the interval in which the 'NA' is found.

```{r missing_values}
activity2 <- activity
for (i in which(sapply(activity2, is.numeric))) {
    for (j in which(is.na(activity2[, i]))) {
        activity2[j, i] <- mean(activity2[activity2[, "interval"] == activity2[j, "interval"], i],  na.rm = TRUE)
    }
}
```

Aggregate the steps for each day and create a histogram showing the frequency of various step totals. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 

```{r aggregate2}
stepsperday <- aggregate(steps ~ date, data=activity2, sum)
hist(stepsperday$steps, main = "Histogram of Steps Per Day (New Data Set)", xlab = "Total Steps", ylab = "Number of Days")
```

Calculate the mean and median.

```{r mean2}
mean(stepsperday$steps)
median(stepsperday$steps)
```

Do these values differ from the estimates from the first part of the assignment? No.  

What is the impact of imputing missing data on the estimates of the total daily number of steps? None.  Since we are using the interval averages to replace NAs, there is no impact on the overall daily average.

Create a new factor variable in the dataset with two levels weekday and weekend indicating whether a given date is a weekday or weekend day.

```{r weekday}
activity2$weekday <- weekdays(activity2$date)
activity2$weekday2 <- ifelse(activity2$weekday != "Saturday" & activity2$weekday != "Sunday", c("Weekday"), c("Weekend"))
weekday <- subset(activity2, weekday2 == "Weekday") 
weekend <- subset(activity2, weekday2 == "Weekend")
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r timeseries2}
intervalweekday <- aggregate(steps ~ interval, data=weekday, mean)
intervalweekend <- aggregate(steps ~ interval, data=weekend, mean)
par(mfrow=c(2,1))
plot(intervalweekday, type = "l", main = "Weekdays")
plot(intervalweekend, type = "l", main = "Weekends")
```
