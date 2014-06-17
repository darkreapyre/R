## Load the data
df <- read.csv("./data/activity.csv", header = TRUE)

## Inspect the raw data
summary(df)

## Transform the column data is of a usable format, i.e steps column is numeric,
## date colum is date format and the interval colum is numeric.
df$steps <- as.numeric(df$steps)
df$date <- as.character(df$date)
df$interval <- as.numeric(df$interval)

## Convert the date colum to the appropriately formatted date
df$date <- as.Date(df$date, format = "%Y-%m-%d")

## Summarize the number of steps by day
sumByDate <- tapply(df$steps, df$date, sum)

## Get the Mean of the number of steps per day
meanByDate <- mean(sumByDate, na.rm = TRUE)

## Get the Median of the numbner of steps per day
medianByDate <- median(sumByDate, na.rm = TRUE)

## Plot the histogram showing the number steps taken per day.
hist(sumByDate, xlab = "No. steps taken per day", col = "red",
     main = "Histogram depicting the number of steps taken per day")

## Summarize the number of steps per interval and the mean
## sumByInterval <- tapply(df$steps, df$interval, sum, na.rm = TRUE)
meanByInterval <- tapply(df$steps, df$interval, mean, na.rm = TRUE)

## Plot the pattern of daily activity
plot(unique(df$interval), meanByInterval, type = "l", col = "red", 
     xlab = "5-minute intervals", ylab = "Average no. steps taken for all days")

## Find the interval containing the maximum number of steps on average
interval <- df[which.max(meanByInterval), 3]

## Confirm the total number of missing values (NA) in the data set
totalMissing <- sum(is.na(df))

## Create a temporary data frame of all the complete cases
tmpdf <- df[complete.cases(df), ]

## create a another data frame of the average steps per interval
tmpMean <- as.data.frame(tapply(tmpdf$steps, INDEX = tmpdf$interval, mean, na.rm = TRUE))
## Tidy up the new data frame
colnames(tmpMean) <- "mean"
tmpMean$interval <- rownames(tmpMean)

## Merge the temporary data frames into a new working data frame
df2 <- merge(tmpMean, df, by = "interval")
sort <- order(df2$date, as.numeric(df2$interval))
df2 <- df2[sort, ]

## Interate through the rows with a for loop, replacing the NA value with the mean vlaue
for (i in 1:nrow(df2)) {
        if (is.na(df2$steps[i])) {
                df2$steps[i] <- df2$mean[i]
        } else {
                df2$steps[i] <- df2$steps[i]
        }
}

## Summarize the number of steps by day in the new data frame
newsumByDate <- tapply(df2$steps, df2$date, sum)

## Get the Mean of the number of steps per day in the new data frame
newmeanByDate <- mean(newsumByDate)

## Get the Median of the numbner of steps per day in the new data frame
newmedianByDate <- median(newsumByDate)

## Plot the histogram showing the number steps taken per day from the new data frame.
hist(newsumByDate, xlab = "No. steps taken per day", col = "red",
     main = "Histogram depicting the number of steps taken per day (missing values replaced)")

####################################################################################################
## Formating????
## Create a new variable (day) in the new data frame and apply the day of the week based on the date
## df2$day <- factor(format(df2$date, "%A"))

## Catagorize the data into weekdays and weekend, based on the day of the week factor
## levels(df2$day) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
##                        weekend = c("Saturday", "Sunday"))

#################################################################################################

test_weekend <- subset(test, test$weekday == "weekend")
test_weekday <- subset(test, test$weekday == "weekday")

## Get the the average no. of steps per interval for each subset
meanByWeekday <- tapply(test_weekday$steps, test_weekday$interval, mean)
meanByWeekend <- tapply(test_weekend$steps, test_weekend$interval, mean)


## Create a unique data frame for plotting
library(lattice)
x <- NULL
y <- NULL
dfplot <- NULL
x <- data.frame(interval = unique(test_weekday$interval), mean = as.numeric(meanByWeekday),
                day = rep("weekday", length(meanByWeekday)))
y <- data.frame(interval = unique(test_weekend$interval), mean = as.numeric(meanByWeekend),
                day = rep("weekend", length(meanByWeekend)))
dfplot <- rbind(x, y)

xyplot(mean ~ interval | day, data = dfplot, layout = c(1, 2), 
       type = "l", xlab = "Interval", ylab = "Number of steps")


### Plot the average number of steps taken on weekdays and weekends
###library(lattice)
### Get the Mean of the number of steps per day
###newmeanByDate <- mean(sumByDate, na.rm = TRUE)

### Get the Median of the numbner of steps per day
###newmeanByInterval <- tapply(df2$steps, df2$interval, mean, na.rm = TRUE)
###xyplot(df2$steps ~ df2$day | df2$interval, type = "l", xlab = "Interval", ylab = "No. of steps",
###      layout = c(1, 2))

