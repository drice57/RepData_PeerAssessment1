## Load lattice library
library(lattice)

## Load data, identify NA values
data <- read.csv("activity.csv", header=TRUE, sep=",", na.strings = "NA",
    stringsAsFactors = FALSE)

data$dateTime <- strptime(sprintf("%s %02d:%02d", data$date,
    data$interval %/% 100, data$interval %% 100),"%Y-%m-%d %H:%M")

data$date <- as.Date(data$date)

## Summarize total steps per day
dataStepsByDay <- aggregate(data$steps, by=list(data$date),
    FUN=sum, na.rm=TRUE)

names(dataStepsByDay) <- c("date", "totalSteps") # fix headings

## Plot number of steps walked by day
plot(dataStepsByDay$date,dataStepsByDay$totalSteps, type="h",
    xlab="Date",ylab="Total # steps",
    main="Histogram of steps walked per day")

## Calculate and print mean number of steps per day
dailyMeanSteps <- round(mean(dataStepsByDay$totalSteps, na.rm=TRUE))
print(sprintf("Mean number of steps per day is %d", dailyMeanSteps))

## Calculate and print median number of steps per day
dailyMedianSteps <- round(median(dataStepsByDay$totalSteps, na.rm=TRUE))
print(sprintf("Median number of steps per day is %d", dailyMedianSteps))

## Calculate and print maximum number of steps per day, 
## and date when max took place
dailyMaxSteps  <- round(max(dataStepsByDay$totalSteps, na.rm=TRUE))
dateMaxSteps <- dataStepsByDay[dataStepsByDay$totalSteps==dailyMaxSteps,1]
print(sprintf("Max number of steps per day is %d, which occured on %s",
    dailyMaxSteps, dateMaxSteps))

## Summarize by time of day (5 minute intervals)
dataStepsByTime <- aggregate(data$steps, by=list(data$interval), FUN=mean,
    na.rm=TRUE)

names(dataStepsByTime) <- c("interval","meanSteps") ## correct names

## Convert interval to time of day (date is arbitrary)
dataStepsByTime$dateTime <- strptime(sprintf("%s %02d:%02d", "2014-07-01", 
    dataStepsByTime$interval %/% 100, 
    dataStepsByTime$interval %% 100),"%Y-%m-%d %H:%M")

## Plot average number of steps by time of day
plot(dataStepsByTime$dateTime, dataStepsByTime$meanSteps,type="l",
    xlab="Time of day",ylab=" Average # steps",
    main="Time series plot of average steps walked 
    \nby time of day (5 minute intervals)")

## Calculate and print maximum average no. of steps per time interval,
## and interval when max took place

timeMaxSteps  <- max(dataStepsByTime$meanSteps, na.rm=TRUE)

timeOfMaxSteps <- dataStepsByTime[dataStepsByTime$meanSteps
      ==timeMaxSteps,3]

p <- "The max of the average number of steps in a 5-min period is %d, "
p <- paste(p,"starting at %s"", sep="")

print(sprintf(p,round(timeMaxSteps), 
  substr(as.character(timeOfMaxSteps),12,16)))

## Count and print number of NAs
print(sprintf("Number of 5-minute intervals with no data is %d.",sum(is.na(data$steps))))

## Create a subset of the rows with NA in steps column
dataSubset <- data[is.na(data$steps),]

## Replace the NAs with 1, then aggregate to get a list of dates and the number of NAs on that date
dataSubset$steps<-1
datesNA <- aggregate(dataSubset$steps,by=list(dataSubset$date), FUN=sum)
names(datesNA) <- c("date","# of NAs")
print(sprintf("Dates with missing values, and number of missing:"))
print(datesNA)

## Copy original data file, replace NAs with average for the 5-minute interval
dataReplaceNA <- data
dataReplaceNA$steps<-ifelse(is.na(dataReplaceNA$steps),
    dataStepsByTime[(dataReplaceNA$interval %/% 100)*12
    + (dataReplaceNA$interval %% 100)/5 +1,2],dataReplaceNA$steps)

## Recreate the histogram of steps walked by day, with NAs replaced
dataStepsByDay <- aggregate(dataReplaceNA$steps, 
    by=list(dataReplaceNA$date), FUN=sum)

names(dataStepsByDay) <- c("date", "totalSteps") ## correct names

plot(dataStepsByDay$date,dataStepsByDay$totalSteps, type="h",
    xlab="Date",ylab="# steps",main="Histogram of steps walked per day")

## Calculate and print mean number of steps per day
dailyMeanSteps <- round(mean(dataStepsByDay$totalSteps))
print(sprintf("Mean number of steps per day is %d", dailyMeanSteps))

## Calculate and print median number of steps per day
dailyMedianSteps <- round(median(dataStepsByDay$totalSteps))
print(sprintf("Median number of steps per day is %d", dailyMedianSteps))

## Add new factor column with weekday vs. weekend distinction
dataReplaceNA$weekend <- ifelse(
    (substr(weekdays(as.Date(dataReplaceNA$date)),1,1) == "S"),
    "weekend","weekday")

dataReplaceNA$weekend<- as.factor(dataReplaceNA$weekend)

## Summarize by time of day (5 minute intervals), contingent on weekday vs. weekend
dataStepsByTime <- aggregate(dataReplaceNA$steps, by=list(dataReplaceNA$interval, dataReplaceNA$weekend), FUN=mean, na.rm=TRUE)
names(dataStepsByTime) <- c("interval","weekend", "meanSteps")

## Convert interval to time of day (date is arbitrary)
dataStepsByTime$dateTime <- strptime(sprintf("%s %02d:%02d", "2014-07-01", 
    dataStepsByTime$interval %/% 100, 
    dataStepsByTime$interval %% 100),"%Y-%m-%d %H:%M")

## Plot average number of steps by time of day, with 2 panels:  
## weekday vs. weekend
p <- xyplot(meanSteps ~ interval | weekend, data=dataStepsByTime, 
    layout = c(1,2), type="l", 
    xlab="Time of day", ylab="Average number of steps", 
    main="Weekday vs. weekend")
print(p)


