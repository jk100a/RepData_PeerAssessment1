# Step 1: Loading and preprocessing the data
# 1. Load the data
data <- read.csv("../activity.csv")
head(data)
# 2. Process/transform the data (if necessary) into a format suitable for your analysis
data$date <- as.Date(data$date, "%Y-%m-%d")

# Step 2: What is mean total number of steps taken per day?
library(dplyr, warn.conflicts = FALSE)
# 1. Calculate the total number of steps taken per day
dataByDate <- data %>% group_by(date) %>% summarise(sum(steps, na.rm = TRUE))
# 2. Make a histogram of the total number of steps taken each day
colnames(dataByDate) = c("Date", "Sum")
dataByDate.f <- filter(dataByDate, Sum != 0)
hist(dataByDate.f$Sum, main = "Histogram of Total Number of Steps Taken per Day", xlab = "total number of steps taken per day",
     col = "green")
# 3. Calculate and report the mean and median of the total number of steps taken per day
round(mean(dataByDate.f$Sum))
median(dataByDate.f$Sum)

# Step 3: What is the average daily activity pattern?
# 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,
#    averaged across all days (y-axis).
dataByInterval <- data %>% group_by(interval) %>% summarise(mean(steps, na.rm = TRUE))
colnames(dataByInterval) = c("Interval", "Mean")
plot(dataByInterval$Mean ~ dataByInterval$Interval, type = "l", main = "Average Number of Steps Taken",
     xlab = "5-minute interval", ylab = "average steps across all days", col = "red")
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
filter(dataByInterval, Mean == max(dataByInterval$Mean))$Interval

# Step 4: Imputing missing values
# 1. Calculate and report the total number of missing values in the dataset.
sum(is.na(data$steps))
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.
#    For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
fillNA <- numeric()
for (i in 1:nrow(data)) {
        temp <- data[i, ]
        if (is.na(temp$steps)) {
                steps <- subset(dataByInterval, Interval == temp$interval)$Mean
        }
        else {
                steps <- temp$steps
        }
        fillNA <- c(fillNA, steps)
}
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
newData <- data
newData$steps <- fillNA
head(newData)
# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and
#    median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment?
#    What is the impact of imputing missing data on the estimates of the total daily number of steps?
newDataByDate <- newData %>% group_by(date) %>% summarise(sum(steps))
colnames(newDataByDate) = c("Date", "Sum")
hist(newDataByDate$Sum, main = "Histogram of Total Number of Steps Taken per Day", xlab = "total number of steps taken per day",
     col = "green")
mean(newDataByDate$Sum)
median(newDataByDate$Sum)

# Step 5: Are there differences in activity patterns between weekdays and weekends?
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
# indicating whether a given date is a weekday or weekend day.
day <- weekdays(data$date)
level <- vector()
for (i in 1:nrow(newData)) {
        if (day[i] == "Saturday" | day[i] == "Sunday") {
                level[i] <- "Weekend"
        }
        else {
                level[i] <- "Weekday"
        }
}
newData <- mutate(newData, weekday.level = as.factor(level))
head(newData)
# 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,
#    averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example
#    of what this plot should look like using simulated data.
newDataByDay <- newData %>% group_by(interval, weekday.level) %>% summarise(mean(steps))
colnames(newDataByDay) = c("interval", "weekday.level", "mean")
library(ggplot2)
ggplot(newDataByDay, aes(interval, mean, fill = weekday.level)) +
        geom_line(col = "red") +
        facet_grid(weekday.level~.) +
        labs(x = "5-minute interval", y = "average steps across weekday levels", title = "Average Number of Steps Taken")
