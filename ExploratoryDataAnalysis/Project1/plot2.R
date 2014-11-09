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
plot(x = data$Datetime, y = data$Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "l")
saveFile("plot2.png")
