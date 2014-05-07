cols <- c("Date", "Time", "Global_active_power", "Global_reactive_power",
          "Voltage", "Global_intensity", "Sub_metering_1",
          "Sub_metering_2", "Sub_metering_3")
df <- read.table("household_power_consumption.txt", sep = ";",
                 skip = 66637, nrows = 2881, col.names = cols)

## Coerce to workable format

df[, 1] <- as.character(df[, 1])
df[, 2] <- as.character(df[, 2])
df[, 3] <- as.numeric(df[, 3])
df[, 4] <- as.numeric(df[, 4])
df[, 5] <- as.numeric(df[, 5])
df[, 6] <- as.numeric(df[, 6])
df[, 7] <- as.numeric(df[, 7])
df[, 8] <- as.numeric(df[, 8])
df[, 9] <- as.numeric(df[, 9])

## Merge Date/Time
x <- as.Date(df$Date, "%d/%m/%Y")
y <- paste(x, df$Time)
date <- strptime(y, "%Y-%m-%d %H:%M:%S")

## Plot
png("plot4.png", width = 480, height = 480)
par(mfrow = c(2, 2))
plot(date, df$Global_active_power, type = "l",
     ylab = "Global Active Power", cex = 0.2)
plot(date, df$Voltage, type = "l", ylab = "Voltage", xlab = "datetime")
plot(date, df$Sub_metering_1, type = "l", ylab = "Energy Submetering")
lines(date, df$Sub_metering_2, type = "l", col = "red")
lines(date, df$Sub_metering_3, type = "l", col = "blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       lty = 1, lwd = 2.5, col = c("black", "red", "blue"), bty = "n")
plot(date, df$Global_reactive_power, type = "l", xlab = "datetime",
     ylab = "Global_reactive_power")
dev.off()
