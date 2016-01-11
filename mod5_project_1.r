## Module-5 Project-1



setwd("C:/2015.07.31_Angel/00Angel/SoftwareProgramsCursosIT/Coursera/2016.01.06-Mod5_ReproducibleResearch/Mod5_Proj_1")


# 1.Code for reading in the dataset and/or processing the data

#myData <- read.csv(file="C:/2015.07.31_Angel/00Angel/SoftwareProgramsCursosIT/Coursera/2016.01.06-Mod5_ReproducibleResearch/Mod5_Proj_1/activity.csv", colClasses = c("numeric", "character", "numeric"))  
#myData <- read.csv(file="C:/2015.07.31_Angel/00Angel/SoftwareProgramsCursosIT/Coursera/2016.01.06-Mod5_ReproducibleResearch/Mod5_Proj_1/activity.csv", header=TRUE, sep=",")  

#here I control that the file is read only when it is not been already read
if(!exists("myData")) 
{
  mypath_toFile_csv <-  paste("C:/2015.07.31_Angel/00Angel/SoftwareProgramsCursosIT/Coursera/2016.01.06-Mod5_ReproducibleResearch",
                              "/Mod5_Proj_1/activity.csv", sep="")
  
  myData  <-  read.csv(file=mypath_toFile_csv,  header=TRUE, sep=",") 
  head (myData)
}




# 2. Histogram of the total number of steps taken each day 

# myData_Date <- myData$date
# myData_Date_dateFormat <- as.Date(myData$date)
#myData$date <- as.Date(myData$date, "%Y-%m-%d")
#stepsForEachDate_2<- aggregate (steps ~ date, myData, sum)
stepsPerDay <- aggregate (steps ~ date, myData, sum, na.rm = TRUE)


# plot:
png ("stepsPerDay_Total.png", height = 500, width = 500)
hist (stepsPerDay$steps, main = "Total Steps make forEach Day", xlab = "Number of Steps", ylab="number")
graphics.off()




# 3. mean and median
stepsPerDayMean <- mean (stepsPerDay$steps)
stepsPerDayMedian <- median (stepsPerDay$steps)



# 4.Time series plot of the average number of steps taken

stepsMeanEachInterval <- aggregate (steps ~ interval, myData, mean)

#plot:
png ("AveStepsforEachInterval.png", height = 500, width = 500)
plot (stepsMeanEachInterval$interval, stepsMeanEachInterval$steps, type = "l", main = "Average Number of Steps for Each Interval", xlab = "Interval", ylab = "Number of Steps")
graphics.off()



# 5. The 5-minute interval that, on average, contains the maximum number of steps
intervalMaxStep <- stepsMeanEachInterval [which.max(stepsMeanEachInterval$steps), 1]



# 6. Code to describe and show a strategy for imputing missing data

numberTotalNA <- sum (is.na (myData$steps))

# changing NA of each interval for the mean value for that interval

newColumnSteps <- numeric ()

## we have already stepsMeanEachInterval <- aggregate (steps ~ interval, myData, mean)

#I run all myData each row and if it's NA and  I put the mean value for that interval in case there is a NA
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


# 7. Histogram of the total number of steps taken each day after missing values are imputed

stepsPerDay_2 <- aggregate (steps ~ date, myData_2, sum, na.rm = TRUE)
# plot:
png ("stepsPerDay_Total_WITHOUT_NA.png", height = 500, width = 500)
hist (stepsPerDay_2$steps, main = "Total Steps make forEach Day", xlab = "Number of Steps", ylab="number")
graphics.off()



# 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# weekdays gives back the day of the week example: x <- weekdays(as.Date("2016-01-09")) gives x = "Saturday"

#I have to transfor the date colum into date. 
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


png ("ComparativeWeekAndWeekends.png", height = 500, width = 500)
plot (noWeekEnd$interval, noWeekEnd$steps, type ="l" , col="blue",  xlab="interval", ylab="steps")
lines (  weekEnd$interval,   weekEnd$steps, type ="l", col="red")

legend("topright", c("no weekEnds", "Weekends"), lty=1, lwd=2.5, col=c("blue", "red"))
graphics.off()


