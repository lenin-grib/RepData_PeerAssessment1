library(plyr)
library(dplyr)

## 1
zipF <- "RepData_PeerAssessment1/activity.zip"
outdir <- "."
unzip(zipF,exdir=outdir)
activity <- read.csv("activity.csv")
activity <- tbl_df(activity)
activity$date <- as.Date(as.character(activity$date), "%Y-%m-%d")

## 2
stepsDay <- 
        activity %>% 
        group_by(date) %>% 
        summarize(steps.total = sum(steps))

hist(stepsDay$steps.total, main = "Steps Taken Daily", xlab = "Steps")

meanStepsDay <- mean(stepsDay$steps.total, na.rm = T)
medianStepsDay <- median(stepsDay$steps.total, na.rm = T)


# 3
stepsInt <- 
        activity %>% 
        group_by(interval) %>% 
        summarize(steps.int = mean(steps, na.rm = T))

plot(stepsInt, type = "l", col = "dark green",
        xlab = "time interval", ylab = "average number of steps",
        main = "Average number of steps per 5-minute interval")

maxTime <- stepsInt[which.max(stepsInt$steps.int),1]

maxTime <- paste0("0",maxTime)
maxTime <- format(strptime(maxTime, format="%H%M"), format = "%H:%M")

#4
totalNA <- sum(is.na(activity$steps))

## merge the data with average steps per interval
activityFull <- merge(activity, stepsInt, by = "interval")

## replace missing values
steps.na <- is.na(activityFull$steps)
activityFull$steps[steps.na] <- activityFull$steps.int[steps.na]

activityFull <- activityFull[,-4]

stepsDayFull <- 
        activityFull %>% 
        group_by(date) %>% 
        summarize(steps.total = sum(steps))
hist(stepsDayFull$steps.total, main = "Steps Taken Daily", xlab = "Steps")
meanStepsDayF <- mean(stepsDayFull$steps.total, na.rm = T)
medianStepsDayF <- median(stepsDayFull$steps.total, na.rm = T)


activityFull$day <- factor(sapply(activityFull$date, function(x){
        if (weekdays(x) == "Sunday" | weekdays(x) == "Saturday") {
                "weekend"
        }
        else{
                "weekday"
        }
}))

stepsWeek <- 
        activityFull %>% 
        group_by(interval, day) %>% 
        summarize(steps = mean(steps))


library(ggplot2)

ggplot(stepsWeek, aes(y=steps, x=interval)) + 
        geom_line(aes(color = day)) +
        ggtitle("Average activity on weekdays and weekends") +
        facet_grid(day  ~ ., scales="free")

library(lattice)


xyplot(steps ~ interval | day, data = stepsWeek, type = "l", layout = c(1, 
        2), xlab = "Interval", ylab = "Number of steps")




