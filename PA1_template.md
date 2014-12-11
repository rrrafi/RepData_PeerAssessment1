---
title: "Coursera Reproducible Research Assigment 1"
output: html_document
---

# Pre-processing #
  
We first load the activity.csv file into R.  Below is the top of the file, in order to illustrate its structure.


```r
activity <- read.csv("activity.csv")
intervals <- dim(activity)[1]
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

There are total of 17568 5-minute intervals.
In order to facilitate the calculation of daily summary statistics, we 'wrangle' the data into a new format, specifically a dataframe with days as rows and intervals as columns.  Below we show the upper left corner of this new dataframe.


```r
startdate <- as.Date(activity[1,2])
enddate <- as.Date(activity[17568,2])
dates <- seq(from=startdate,to=enddate,by=1)
act2 <-data.frame(date=dates)
for (i in seq(0,2355,5)){
        act2[as.character(i)] = activity[activity$interval==i,1]
        }
act2[1:10,1:20]
```

```
##          date  0  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90
## 1  2012-10-01 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## 2  2012-10-02  0  0  0  0  0  0  0  0  0  0  0  0 NA NA NA NA NA NA NA
## 3  2012-10-03  0  0  0  0  0  0  0  0  0  0  0  0 NA NA NA NA NA NA NA
## 4  2012-10-04 47  0  0  0  0  0  0  0  0  0  0  0 NA NA NA NA NA NA NA
## 5  2012-10-05  0  0  0  0  0  0  0  0  0  0  0  0 NA NA NA NA NA NA NA
## 6  2012-10-06  0  0  0  0  0  0  0  0  0  0  0  0 NA NA NA NA NA NA NA
## 7  2012-10-07  0  0  0  0  0  0  0  0  0  0  0  7 NA NA NA NA NA NA NA
## 8  2012-10-08 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## 9  2012-10-09  0  0  0  0  0 13 28  0  0  0  0  0 NA NA NA NA NA NA NA
## 10 2012-10-10 34 18  7  0  0  0  0  0  0  0  0  0 NA NA NA NA NA NA NA
```
  
  
# Mean steps per day #

We now show a histogram of the steps taken per day, as well as the mean and median of steps taken per day.


```r
dailysteps <- sapply(seq(1,61), function(x) sum(act2[x,2:473],na.rm=TRUE))
hist(dailysteps,10,xlab="Steps / Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
ds_mean = round(mean(dailysteps),0)
ds_median = round(median(dailysteps),0)
```
The mean and median for steps taken per day are 9354 and 1.0395 &times; 10<sup>4</sup>, respectively.
  
    
# Daily activity pattern #
The below chart shows the daily activity pattern, as illustrated by mean (over the observed time period) number of steps taken per time interval during the day.


```r
intervals <- sapply(seq(2,473), function(x) mean(act2[,x],na.rm=TRUE))
plot(colnames(act2[2:473]),intervals,type="l",xlab="Interval #",ylab="Mean steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
  
    
# Missing values #
We now substitute any missing (NA) values for steps with the mean number of steps per interval for the same day.


```r
act_clean <- na.omit(activity)
missing <- dim(activity)[1]-dim(act_clean)[1]

dailystepsavg <- dailysteps/472
act3 <- act2

for (i in seq(2,473)){
        for(d in seq(1,61)){
                if(is.na(act2[d,i])==TRUE){
                        act3[d,i] <- dailystepsavg[d]
                        }
                }
        }
```

In the original data, 2304 observations were missing.  We now recalculate mean and median number of steps per day and redraw the histogram.


```r
dailysteps2 <- sapply(seq(1,61), function(x) sum(act3[x,2:473],na.rm=TRUE))
hist(dailysteps2,10,xlab="Steps / Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
ds_mean2 = round(mean(dailysteps2),0)
ds_median2 = round(median(dailysteps2),0)
```

The mean and median for steps taken per day are 1.3001 &times; 10<sup>4</sup> and 1.4447 &times; 10<sup>4</sup>, respectively, dragged up by the use of the daily means as proxies for the missing values.
  
    
# Weekdays vs. weekends #
In the following, we consider activity patterns on weekday vs. weekend days.


```r
act3["day"] <- weekdays(act3[,"date"])
daytype <-function(d){
        if(d=="Saturday"|d=="Sunday"){"weekend"}else{"weekday"}
        }
act3["daytype"] <- sapply(act3[,"day"],function(x) daytype(x))
act3wd = act3[act3$daytype=="weekday",]
act3we = act3[act3$daytype=="weekend",]

intervals_wd <- sapply(seq(2,473), function(x) mean(act3wd[,x],na.rm=TRUE))
intervals_we <- sapply(seq(2,473), function(x) mean(act3we[,x],na.rm=TRUE))

par(mar=c(2,2,2,2))
par(mfrow=c(2,1))
plot(colnames(act3[2:473]),intervals_wd,type="l",xlab="Interval #",ylab="Mean steps", main="Weekdays")
plot(colnames(act3[2:473]),intervals_we,type="l",xlab="Interval #",ylab="Mean steps", main="Weekends")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 
