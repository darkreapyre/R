cols <- c("Date", "Time", "Global_active_power", "Global_reactive_power",
          "Voltage", "Global_intensity", "Sub_metering_1",
          "Sub_metering_2", "Sub_metering_3")
df <- read.table("household_power_consumption.txt", sep = ";",
                 skip = 66637, nrows = 2881, col.names = cols)

png("plot1.png", width = 480, height = 480)
hist(df$Global_active_power, main = "Global Active Power", col = "red", 
     xlab = "Global Active Power (kilowatts)")
dev.off()
