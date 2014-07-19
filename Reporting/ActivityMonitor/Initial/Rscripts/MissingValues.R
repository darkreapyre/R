### Option 1 --> NOPE MISMATCH IN ROW SIZES
df[is.na(df$steps), "steps"] <- tapply(df$steps, df$interval, mean,  
                                         na.rm=TRUE)[ df[is.na(df$steps),"interval"] ]



### Option 2 --> NOPE SAME ISSUES AS OPTION 1
df2 <- df
f=function(x){
        x[is.na(x)] = mean(x, na.rm=TRUE) #convert the item with NA to median value from the column
        x #display the column
}
tmp <- data.frame(apply(df2,2,f))

### option 3 --> ISSUES WUITH THE SIZE FOT THE VARIABLE
lapply( df2, function(x) x[x=="NA"] <- mean(as.numeric(as.character(x)), na.rm=TRUE) )

### Option 4 --> Build an entiryl new data frame and merge the values (SUCCESS)
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

### Option 5 --> USe subsetting for xypplot
## For loop for integrating the "sumByInterval" into new data frame
test <- df

f <- tapply(df$steps, df$interval, mean, na.rm=T)

for (i in 1:nrow(df)){
        if(is.na(df$steps[i])){
                test$steps[i]<- f[[as.character(df[i, "interval"])]]
        }
}

## Add weekday colum to catagorize 
test$weekday <- c("weekday")
test[weekdays(as.Date(test[, 2])) %in% c("Saturday", "Sunday", "saturday", "sunday"), ][4] <- c("weekend")

## Add the factor levels
test$weekday <- factor(test$weekday)


## New alternative to Agregate --> Subsetting
## subset the data
test_weekend <- subset(test, test$weekday == "weekend")
test_weekday <- subset(test, test$weekday == "weekday")

## Get the the average no. of steps per interval for each subset
meanByWeekday <- tapply(test_weekday$steps, test_weekday$interval, mean)
meanByWeekend <- tapply(test_weekend$steps, test_weekend$interval, mean)


## Create a unique data frame for plotting
library(lattice)
#x <- NULL
#y <- NULL
#dfplot <- NULL
x <- data.frame(interval = unique(test_weekday$interval), mean = as.numeric(meanByWeekday),
                        day = rep("weekday", length(meanByWeekday)))
y <- data.frame(interval = unique(test_weekend$interval), mean = as.numeric(meanByWeekend),
                        day = rep("weekend", length(meanByWeekend)))
dfplot <- rbind(x, y)

xyplot(mean ~ interval | day, data = dfplot, layout = c(1, 2), 
       type = "l", xlab = "Interval", ylab = "Number of steps")