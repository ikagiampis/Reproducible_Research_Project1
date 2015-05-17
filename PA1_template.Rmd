---
title: "Project1"
---

##Part1

#Loading and preprocessing the data

-Load the data

-Process the data into a format suitable for your analysis

```{r part1, echo = TRUE}
library('dplyr')
library('data.table')

acs<-read.csv("activity.csv")
acs <- data.table(acs)
```

##Part2

#What is mean total number of steps taken per day?

-Calculate the total number of steps taken per day

-Make a histogram of the total number of steps taken per day

-Mean and median of the total number of steps taken per day

```{r part2, echo = TRUE}
acs1<- acs[,list(Steps=sum(steps)), by=as.Date(date, format="%Y-%m-%d")]
#acs1[is.na(acs1)] <- 0
acs1
hist(as.numeric(as.character(acs1$Steps)), col = "red", main = "Histogram of the total number of steps taken each day", xlab = "Number of Steps" )
acs1[is.na(acs1)] <- 0
mean(acs1$Steps)
median(acs1$Steps)
```

##Part3

#What is the average daily activity pattern?

-Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

-Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```{r part3, echo = TRUE}
acs$interval <- as.factor(acs$interval)
acs3<- tapply(acs$steps, acs$interval, sum, na.rm = TRUE, simplify = TRUE)/length(levels(acs$date))
View(acs3)

x<-levels(acs$interval)
y<-acs3

plot(x, y ,type = "l", xaxt = "n", xlab="Intervals",  ylab = "Number of Steps",main = "Average number of Steps taken every 5-minute interval",  col = "red", cex.axis=1.0)

names(which.max(acs3))
```

##Part4

#Imputing missing values

-Calculate and report the total number of missing values in the dataset

-Change the NAs values using the mean of the day

-Create a new dataset that is equal to the original dataset but with the missing data filled in.

-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```{r part4, echo = TRUE}
naNumber <- !complete.cases(acs$steps)
sum(naNumber)

initialTableLength = length(acs$steps)
acs5 <- as.data.frame(acs3)
#asc5[0,5]
colnames(acs5) <- c("mSteps")
acs5 <- cbind(interval = rownames(acs5), acs5)
#View(acs5)
meanTableLength = nrow(acs5)
meanTableLength

for (i in 1:initialTableLength){
  if(is.na(acs$steps[i])){
    for(k in 1:meanTableLength){
      if(as.numeric(acs$interval[i]) == as.numeric(acs5$interval[k])){
        acs$steps[i] = acs5$mSteps[k]
      }
    }
  }
}
newTable <- acs
View(newTable)
nacs1<- newTable[,list(Steps=sum(steps)), by=as.Date(date, format="%Y-%m-%d")]
#acs1[is.na(acs1)] <- 0
View(nacs1)
hist(as.numeric(as.character(nacs1$Steps)), col = "red", main = "Histogram of the total number of steps taken each day", xlab = "Number of Steps" )

mean(nacs1$Steps)
median(nacs1$Steps)
```
We see small differences (small increase) between the mean and median if we change the NAs values with the means.

##Part5

#Are there differences in activity patterns between weekdays and weekends?

-Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

-Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r part5, echo = TRUE}
Sys.setlocale("LC_TIME")
#View(newTable$as.date)
weekdays <- weekdays(as.Date(newTable$date, "%Y-%m-%d"))
nextTable <- cbind(newTable, weekdays)
#View(nextTable)
SatTable <-subset(nextTable,grepl('^Saturday',nextTable$weekdays))
SunTable <- subset(nextTable,grepl('^Sunday',nextTable$weekdays)) 
weekendTable = rbind(SatTable, SunTable)
#View(weekendTable)

MTable <-subset(nextTable,grepl('^Monday',nextTable$weekdays))
TTable <-subset(nextTable,grepl('^Tuesday',nextTable$weekdays))
WTable <-subset(nextTable,grepl('^Wednesday',nextTable$weekdays))
TuTable <-subset(nextTable,grepl('^Thursday',nextTable$weekdays))
FTable <- subset(nextTable,grepl('^Friday',nextTable$weekdays))

weekTable <- rbind(MTable,TTable, WTable,TuTable, FTable )

weekTable <- as.data.frame(weekTable)
weekendTable <- as.data.frame(weekendTable)

weekendLength <- nrow(weekendTable)
weekLength <- nrow(weekTable)

stepsAverWeek <- tapply(weekTable$steps, weekTable$interval, sum, simplify = TRUE)/(weekLength/288)
stepsAverWeekend <- tapply(weekendTable$steps, weekendTable$interval, sum, simplify = TRUE)/(weekendLength/288)


output <- data.frame(steps = c(stepsAverWeek, stepsAverWeekend), 
    interval = c(levels(acs$interval), levels(acs$interval)), weekday = as.factor(c(rep("weekday", 
        length(stepsAverWeek)), rep("weekend", length(stepsAverWeekend)))))
#View(output)

library(lattice)
xyplot(steps ~ interval | weekday, data = output, layout = c(1, 2), ylab = "Number Of Steps", 
    main = "Average Number of Steps for the Weekdays or the Weekend")

```
