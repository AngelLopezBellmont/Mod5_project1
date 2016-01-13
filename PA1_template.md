# Module-5 Project-1
Angel_Lopez_Bellmont  
January 10, 2016  

    
## 1.Code for reading in the dataset and/or processing the data

I point to my workingdirectory, and I control if the dataSet activity.csv is already read or not


```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.3
```

```r
library(timeDate)
```

```
## Warning: package 'timeDate' was built under R version 3.2.3
```

```r
library(stringr)
```

```
## Warning: package 'stringr' was built under R version 3.2.3
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.2.3
```

```
## 
## Attaching package: 'plyr'
```

```
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
setwd("C:/2015.07.31_Angel/00Angel/SoftwareProgramsCursosIT/Coursera/2016.01.06-Mod5_ReproducibleResearch/Mod5_Proj_1")
#here I control that the file is read only when it is not been already read
if(!exists("myData")) 
{
  mypath_toFile_csv <-  paste("C:/2015.07.31_Angel/00Angel/SoftwareProgramsCursosIT/Coursera/2016.01.06-Mod5_ReproducibleResearch",
                              "/Mod5_Proj_1/activity.csv", sep="")
  
  myData  <-  read.csv(file=mypath_toFile_csv,  header=TRUE, sep=",") 
  head (myData)
}
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

```r
stepsPerDay <- aggregate (steps ~ date, myData, sum, na.rm = TRUE)
head(stepsPerDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

 
## 2. Histogram of the total number of steps taken each day 



```r
#png ("stepsPerDay_Total.png", height = 500, width = 500)
plot (stepsPerDay$date, stepsPerDay$steps, type ="l",  main = "Total Steps make forEach Day", xlab = "Number of Steps", ylab="number")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)\

```r
#graphics.off()
```

## 3.mean and median
Here we calculate the mean and median


```r
stepsPerDayMean <- mean (stepsPerDay$steps)
stepsPerDayMedian <- median (stepsPerDay$steps)

stepsPerDayMean
```

```
## [1] 10766.19
```

```r
stepsPerDayMedian
```

```
## [1] 10765
```



## 4.Time series plot of the average number of steps taken



```r
stepsMeanEachInterval <- aggregate (steps ~ interval, myData, mean)

plot (stepsMeanEachInterval$interval, stepsMeanEachInterval$steps
  ,type = "l"
  ,xlab = "Interval", 
  ,ylab = "Number of Steps"
  ,main = "Average Number of Steps for Each Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

```r
#No me deja pintar pq dice que tiene NA el dataSet.
# qplot (stepsMeanEachInterval$interval, stepsMeanEachInterval$steps
# ,type = "line"
# ,xlab = "Interval", 
# ,ylab = "Number of Steps"
# ,main = "Average Number of Steps for Each Interval")
```



## 5. The 5-minute interval that, on average, contains the maximum number of steps


```r
  intervalMaxStep <- stepsMeanEachInterval [which.max(stepsMeanEachInterval$steps), 1]
  intervalMaxStep
```

```
## [1] 835
```


## 6. Code to describe and show a strategy for imputing missing data

  We change the NA of each interval in the dataSet, for the mean value for that interval.
  I run all myData each row and if it's NA and  I put the mean value for that interval in case there is a NA.
  The new data set I call it myData_2.
  

```r
  newColumnSteps <- numeric ()
  
  for (i in 1:nrow (myData)) 
{
     row_i <- myData [i, ]
    
    if (is.na (row_i$steps)) 
    {
       stepsNew <- subset (stepsMeanEachInterval, interval == row_i$interval)$steps
    }
    else 
    {
      stepsNew <- row_i$steps
    }
    
  newColumnSteps <- c (newColumnSteps, stepsNew)
}

# we create the new DataSet myData_2
myData_2 <- myData
myData_2$steps <- newColumnSteps

head(myData_2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```



## 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
stepsPerDay_2 <- aggregate (steps ~ date, myData_2, sum, na.rm = TRUE)
plot(stepsPerDay_2$date, stepsPerDay_2$steps, type = "l", main = "Total Steps make forEach Day", xlab = "Number of Steps", ylab="number")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)\

```r
#plot (stepsMeanEachInterval$interval, stepsMeanEachInterval$steps, type = "l", main = "main", xlab = "Interval", ylab = "Number of Steps")
```


## 8.Panel plot comparing steps in weekdays vs. weekends

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
weekdays gives back the day of the week example: x <- weekdays(as.Date("2016-01-09")) gives x = "Saturday"

I have to transfor the original dataset  myDate, the date colum into date.


```r
myData$date <- as.Date(myData$date, "%Y-%m-%d")

day <- weekdays (myData$date)
daytype <- vector ()

  for (i in 1:nrow (myData)) 
  {
    if (day [i] == "Saturday")
    {
      daytype [i] <- "weekend"
    }
    else if (day [i] == "Sunday")
    {
      daytype [i] <- "weekend"
    }
    else 
    {
      daytype [i] <- "weekday"
    }
  }

myData$daytype <- daytype
myData$daytype <- factor (myData$daytype)



myDataWeek <- aggregate (steps ~ interval + daytype, myData, mean)
names(myDataWeek) <- c ("interval", "daytype", "steps")

noWeekEnd <- subset (myDataWeek, daytype=="weekday")
weekEnd <- subset (myDataWeek, daytype=="weekend")

plot (noWeekEnd$interval, noWeekEnd$steps, type ="l" , col="blue",  xlab="interval", ylab="steps")
lines (  weekEnd$interval,   weekEnd$steps, type ="l", col="red")
legend("topright", c("no weekEnds", "Weekends"), lty=1, lwd=2.5, col=c("blue", "red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)\


















