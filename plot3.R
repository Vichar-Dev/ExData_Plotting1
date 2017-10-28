unzipFile <- "household_power_consumption.txt"
downloadFile <- "household_power_consumption.zip"
downloadLink <- "https://d396qusza40orc.cloudfront.net/exdata/data/household_power_consumption.zip"
  
plot3 <- function(){
  ## Check if dataset file has already been unzipped
  if(!file.exists(unzipFile)){
    
    ## Check if file has already been downloaded
    if (!file.exists(downloadFile)){
      
      ## File not downloaded, hence download file
      download.file(downloadLink, downloadFile)
      
    } else {
      ## File already downloaded
    }
    
    ## now Unzip file and extract dataset
    unzip(downloadFile)
    
  } else {
    ## Dataset file exist already
  }
  
  t <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
  
  ## Format date to Type Date
  t$Date <- as.Date(t$Date, "%d/%m/%Y")
  
  ## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
  t <- subset(t,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
  
  ## Remove incomplete observation
  t <- t[complete.cases(t),]
  
  ## Combine Date and Time column
  dateTime <- paste(t$Date, t$Time)
  
  ## Name the vector
  dateTime <- setNames(dateTime, "DateTime")
  
  ## Remove Date and Time column
  t <- t[ ,!(names(t) %in% c("Date","Time"))]
  
  ## Add DateTime column
  t <- cbind(dateTime, t)
  
  ## Format dateTime Column
  t$dateTime <- as.POSIXct(dateTime)
  
  with(t, {
    plot(Sub_metering_1~dateTime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
  })
  legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  ## Saving to file
  dev.copy(png, file="plot3.png", height=480, width=480)
  dev.off()
}
