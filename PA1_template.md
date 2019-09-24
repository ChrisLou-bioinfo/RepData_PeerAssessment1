RepData peer Assesssment 1
========================================================




```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Loading the data
----------------

```{r echo=TRUE}
activity<-read.csv("./activity.csv",header = T)
```
* Process/transform the data (if necessary) into a format suitable for your analysis
```{r echo=TRUE}
Steps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
```

What is mean total number of steps taken per day?
-------------------------------------------------

* Make a histogram of the total number of steps taken each day
```{r echo=TRUE}
hist(Steps$steps)
```

* Calculate and report the **mean** and **median** total number of steps taken 
per day 

```{r echo=TRUE}
mean(Steps$steps)
median(Steps$steps)
```
What is the average daily activity pattern?
-------------------------------------------


* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```{r echo=TRUE}
Interval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps~interval,data=Interval,type="l")
```
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 
```{r echo=TRUE}
Interval[which.max(Interval$steps),]$interval
```
Imputing missing values
-----------------------

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```{r echo=TRUE}
sum(is.na(activity$steps))
```
* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
: I built a strategy for filing in all of the missing values with the mean for that 5-minute interval. 
* Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
newactivity<-activity   # Make a new dataset with the original data
for(i in 1:nrow(newactivity)){
    if(is.na(newactivity[i,]$steps)){
        newactivity[i,]$steps<-Interval[Interval$interval==newactivity[i,]$interval,]$steps
    }
}

```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
```{r echo=TRUE}
Steps2<-aggregate(steps~date,data=newactivity,sum)
hist(Steps2$steps)
mean(Steps2$steps)
median(Steps2$steps)
```

Are there differences in activity patterns between weekdays and weekends?
---------------------------------------------------------------------------

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r echo=TRUE}
newactivity$day=ifelse(as.POSIXlt(as.Date(newactivity$date))$wday%%6==0,
                          "weekend","weekday")
newactivity$day=factor(newactivity$day,levels=c("weekday","weekend"))
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```{r echo=TRUE}
Interval2=aggregate(steps~interval+day,newactivity,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=Interval2,aspect=1/2,type="l")

```


