


# Loading and preprocessing the data

Read the data csv file. Assign variable activity as original data set.



```r
activity <- read.csv("activity.csv")
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
library(ggplot2)
library(scales)
library(lattice)
library(timeDate)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

# What is mean total number of steps taken per day?

Create a subset, activity1, which ignores missing values after checking which column variables contain missing values.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity$interval))
```

```
## [1] 0
```

```r
sum(is.na(activity$date))
```

```
## [1] 0
```

```r
activity1 <- subset(activity, !is.na(activity$steps))
str(activity1)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Calculate the total number of steps taken per day.
Aggregate the steps by day and find the sum of each day.
Make a histogram of the total number of steps taken each day.
Calculate and report the mean and median of the total number of steps taken per day.
The mean and median value represent the dataset with no missing values.


```r
stepsByDay1 <- aggregate(steps ~ date, activity1, sum)

str(stepsByDay1)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```r
hist(stepsByDay1$steps, xlab = "Grouped steps for each day", ylab = "Number of days", main = "Total number of steps per day")
```

![plot of chunk steps per day dataset 2](figure/steps per day dataset 2-1.png)

```r
mean1 <- mean(stepsByDay1$steps)
mean1
```

```
## [1] 10766.19
```

```r
median1<- median(stepsByDay1$steps)
median1
```

```
## [1] 10765
```

#What is the average daily activity pattern?
Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?




```r
dailyAvg <- tapply(activity1$steps, activity1$interval, mean, na.rm = TRUE, simplify = TRUE)

plot( activity1$interval, activity1$steps, type="l", xlab = "Minute intervals", ylab = "Number of steps", main = "Average number of steps per day per interval")
```

![plot of chunk daily activity pattern](figure/daily activity pattern-1.png)

```r
dailyAvg[which(dailyAvg == max(dailyAvg))]
```

```
##      835 
## 206.1698
```

# Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
To find out whether the percentage of missing values is negligible, call the following function. 
13 % of the dataset has missing values.
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
The missing values will be filled by using the mean of the daily average pattern steps. 
Create a new dataset that is equal to the original dataset but with the missing data filled in
A third data set, activity2 is created whereby the Nafiller value is inputed where NA values existed in the original data set. 
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
Find the summation.
Creats a histogram 
The mean and median values are then calculated.
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
The mean values have no difference while the mediam value increases slightly with the imputed data. 



```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity$interval))
```

```
## [1] 0
```

```r
sum(is.na(activity$date))
```

```
## [1] 0
```

```r
mean(is.na(activity$steps))
```

```
## [1] 0.1311475
```

```r
dailyAvg <- aggregate(steps~interval, activity, mean)
Nafiller <- mean(dailyAvg$steps)
Nafiller
```

```
## [1] 37.3826
```

```r
activity2 <- activity
MissingNO <- is.na(activity2$steps)
activity2[MissingNO, 1] <- Nafiller
stepsByDay2 <- aggregate(steps ~ date, activity2, sum)
str(stepsByDay2)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps: num  10766 126 11352 12116 13294 ...
```

```r
hist(stepsByDay2$steps, xlab = "Grouped steps for each day", ylab = "Number of days", main = "Total number of steps per day")
```

![plot of chunk NA imputed dataset vs raw set](figure/NA imputed dataset vs raw set-1.png)

```r
mean2 <- mean(stepsByDay2$steps)
mean2
```

```
## [1] 10766.19
```

```r
median2 <- median(stepsByDay2$steps)
median2
```

```
## [1] 10766.19
```

```r
median2 - median1
```

```
## [1] 1.188679
```

```r
mean1 - mean2
```

```
## [1] 0
```

# Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.Create weekend and weekday variables and compare the activity pattern in each using data set activity 2 with imputed missing value.
Create an additional column with the day labels.
Create the subsets, weekend and weekday for comparison.
Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity2 <- mutate(activity2, day = weekdays(activity$date))
activity2$day <- factor((weekdays(activity2$date) %in% weekdays),levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
Weekday <- subset(activity2, day == "Weekday")
Weekend <- subset(activity2, day == "Weekend")
paneldata <- aggregate(steps ~ day+interval, data = activity2, FUN = mean)
xyplot(steps ~ interval | factor(day),layout = c(1, 2),xlab="5 minute Interval",ylab="Average Number of steps",type="l", lty=1, data=paneldata)
```

![plot of chunk weekend weekday comparison](figure/weekend weekday comparison-1.png)


