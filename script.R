


## Loading and Pre processing Data

unzip("activity.zip")
activity <- read.csv("activity.csv", sep = ",")
activity$date <- as.Date(activity$date)
weekday <- weekdays(activity$date)
activity <- cbind(activity, weekday)


## What is the mean total number of steps taken per day?

totalStepsPerDay <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE) 
names(totalStepsPerDay) = c("date", "totalSteps")

hist(totalStepsPerDay$totalSteps, main = "Total steps taken each day", col = "black",
                xlab = "Steps", breaks = 5)

mean(totalStepsPerDay$totalSteps)

median(totalStepsPerDay$totalSteps)


## What is the average daily activity pattern?

averageStepsPerDay <- aggregate(activity$steps, by = list(activity$interval), 
                FUN = mean, na.rm = TRUE) 
names(averageStepsPerDay) = c("interval", "averageSteps")

plot(averageStepsPerDay$interval, averageStepsPerDay$averageSteps, type = "l", lwd = 2,
                col = "black", xlab = "Interval", ylab = "Average steps")

averageStepsPerDay[which.max(averageStepsPerDay$averageSteps), ]$interval


## Imputing missing values

sum(is.na(activity$steps))

activityComplete <- activity
blank <- is.na(activityComplete$steps)

stepReplace <- tapply(activityComplete$steps, activityComplete$interval, 
                mean, na.rm=TRUE, simplify=TRUE)
activityComplete$steps[blank] <- stepReplace[as.character(activityComplete$interval[blank])]

sum(is.na(activityComplete$steps))

totalStepsPerDay <- aggregate(activityComplete$steps, by = list(activityComplete$date), 
                FUN = sum, na.rm = TRUE) 
names(totalStepsPerDay) = c("date", "totalSteps")

hist(totalStepsPerDay$totalSteps, main = "Total steps taken each day", col = "black",
                xlab = "Steps", breaks = 5)

mean(totalStepsPerDay$totalSteps)

median(totalStepsPerDay$totalSteps)


## Are there differences in activity patterns between weekdays and weekends?

checkDayType <- function(x) {
        y <- weekdays(x)
        ifelse (y == "Saturday" | y == "Sunday", "weekend", "weekday")}

z <- sapply(activityComplete$date, checkDayType)
activityComplete$dayType <- as.factor(z)

activityCompleteWeekday <- aggregate(steps ~ dayType + interval, data = activityComplete, 
                FUN = mean)

library(lattice)

xyplot(steps ~ interval | factor(dayType), data = activityCompleteWeekday, 
                layout = c(1, 2), xlab = "Interval", ylab = "Steps", type = "l", lwd = 2)



