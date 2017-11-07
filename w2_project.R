# Setting the working directory
setwd("C:\\00-AdityaTibrewal\\OneDrive\\Study Mtrl\\Data Science\\Coursera\\Reproducible Analysis\\W2\\Peer Project\\RepData_PeerAssessment1")

# Loading libraries
library(dplyr)
library(ggplot2)

# File reading and pre-processing
activity = read.csv(unzip("activity.zip"), na.strings = "NA")
activity$date = as.Date(activity$date)

# Ask 1 - Steps per day
totalStepsDayWise = group_by(activity, date) %>%
      summarise(totalSteps = sum(steps))
meanSteps = mean(totalStepsDayWise$totalSteps, na.rm = T)

# Ask 2 - Steps per day (histogram)
hist(totalStepsDayWise$totalSteps, col = "blue", xlab = "Total # of Steps",
     main = "Distribution of Total Steps")

# Ask 3 - Mean and median of the total number of steps taken per day
medianSteps = median(totalStepsDayWise$totalSteps, na.rm = T)
meanSteps = mean(totalStepsDayWise$totalSteps, na.rm = T)

print(paste('Median steps per day: ', medianSteps))
print(paste('Mean steps per day: ', round(meanSteps, 0)))

# Ask 4 - Time series plot of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)
meanStepsMinWise = group_by(activity, interval) %>%
      summarise(meanSteps = mean(steps, na.rm = T))

plot(x = meanStepsMinWise$interval, y = meanStepsMinWise$meanSteps, 
     type =  "l", xlab = "5-minute intervals", 
     ylab = "Mean of steps taken across days",
     main = "Average number of steps taken by 5-minute intervals")

# Ask 5 - Interval with max average steps
meanStepsMinWise[meanStepsMinWise$meanSteps == max(meanStepsMinWise$meanSteps), 1]

# Ask 6 - Count of missing values
nrow(activity[is.na(activity$steps), ])

# Ask 7 and 8 - Imputing missing values for steps
# Each missing value to be replaced with the median of total steps taken 
# in that interval
medianStepsMinWise = group_by(activity, interval) %>%
      summarise(meanSteps = median(steps, na.rm = T))
activityCompleted = activity

# Going throuh the dataset - finding rows with missing values - 
# finding median steps for those intervals - imputing in the dataset
for (i in 1:nrow(activityCompleted)) {
      if (is.na(activityCompleted[i, 1])) {
            activityCompleted[i, 1] = 
                  medianStepsMinWise[medianStepsMinWise$interval == activityCompleted[i, 3], 2]
      }
}

# Ask 8 - Histogram of imputed dataset
totalStepsDayWiseCompleted = group_by(activityCompleted, date) %>%
      summarise(totalSteps = sum(steps))

par(mfrow = c(1, 2))
hist(totalStepsDayWise$totalSteps, col = "blue", xlab = "Total # of Steps",
     main = "Distribution of Total Steps")
hist(totalStepsDayWiseCompleted$totalSteps, col = "blue", xlab = "Total # of Steps",
     main = "Distribution of Total Steps (Imputed)")
dev.off()

# Mean and median of total steps
meanStepsCompleted = mean(totalStepsDayWiseCompleted$totalSteps)
medianStepsCompleted = median(totalStepsDayWiseCompleted$totalSteps)

print(paste('Median steps per day for the imputed data set: ', medianStepsCompleted))
print(paste('Mean steps per day for the imputed data set: ', round(meanStepsCompleted, 0)))

# Comparison of mean and median pre and post imputation
boxplot(totalStepsDayWise$totalSteps, totalStepsDayWiseCompleted$totalSteps, 
        names = c("Original Data", "Imputed Data"), col = "grey")
legend(1, meanSteps, c("Mean Original Data"), cex = 0.7)
legend(2, meanStepsCompleted, c("Mean Imputed Data"), cex = 0.7)
abline(h = meanSteps, col = "red")
abline(h = meanStepsCompleted, col = "blue")

# Ask 9 - Splitting into Weekday/ Weekend

getDayType = function (dayVal) {
      dayType = "weekday"
      wkEnd = c("Saturday", "Sunday")
      if (weekdays(dayVal) %in% wkEnd) {
            dayType =  "weekend"
      } else {
            dayType = "weekday"
      }
      dayType
}
activityDayType = activityCompleted
activityDayType$dayType = sapply(activityDayType$date, getDayType)
activityDayType$dayType = as.factor(activityDayType$dayType)

# Ask 10 - Average steps at 5-minute intervals split across weekday ~ weekend
activityDayTypeAvg = group_by(activityDayType, interval, dayType) %>%
      summarise(avgSteps = mean(steps, na.rm = T))

ggplot(activityDayTypeAvg, aes(x = interval, y = avgSteps)) +
      facet_wrap(~ dayType, nrow = 2) + 
      geom_line(col = "blue") +
      labs(x = "Interval in minutes", y = "Average steps", 
           title = "Average steps by weekday/ weekend")