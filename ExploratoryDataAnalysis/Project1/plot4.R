getWorkingData <- function(){
  data <- read.csv("household_power_consumption.txt", header=T, sep=";", na.strings="?")
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  workingData <- subset(data, "2007-02-01" <= Date & Date <= "2007-02-02")
  datetime <- paste(as.Date(workingData$Date), workingData$Time)
  workingData$Datetime <- as.POSIXct(datetime)
  return(workingData)
}

saveFile <- function(title){
  dev.copy(png, file=title, height=480, width=480)
  dev.off()
}

data <- getWorkingData()
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data, {
  plot(x = Datetime, y = Global_active_power, xlab = "", 
       ylab = "Global Active Power (kilowatts)", type = "l")
  plot(x = Datetime, y = Voltage, xlab = "datetime", 
       ylab = "Voltage", type = "l")
  plot(x = Datetime, y = Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "l")
  lines(x = Datetime, y = Sub_metering_2, col="Red")
  lines(x = Datetime, y = Sub_metering_3, col = "Blue")
  legend("topright", col=c("black", "red", "blue"), 
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, lwd = 2, cex=0.25)
  plot(x = Datetime, y = Global_reactive_power, xlab = "datetime", type = "l")
})
saveFile("plot4.png")