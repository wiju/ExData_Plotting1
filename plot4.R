plot4 <- function() {
 
## Project 1 Exploratory Data Analysis
## Use strptime() and as.Date() to convert the text entries 
## to date data types
## Note that in this dataset missing values are coded as ?.
        
library(lubridate)
library(dplyr)

## Reads whole UC Irvine householdx power data set in & 
## reeadies data for plotting
        
        dataPlotTotal <- data.frame()
        dataPlotTarget <- data.frame()
        dataSubs <- vector()

        ## Read file
        fileUrl <- "./household_power_consumption.txt" 
        dateDownloaded <- "2015-06-06"
        list.files()
        ## Read data into table; colClasses = char to suppress conversions 
        dataPlotTotal <- read.table(fileUrl, header=TRUE, sep=";", na.strings="?", 
                                colClasses = "character")
        ## Remove incomplete data
        dataPlotTotal <- dataPlotTotal[complete.cases(dataPlotTotal), ]

        ## Subset data to just the dates requested in Project 1: 2/01-2/02/2007
        dataPlotTarget <- dataPlotTotal[((dataPlotTotal$Date == "1/2/2007") | 
                                        (dataPlotTotal$Date == "2/2/2007")), ]

        ## Merge Date & Time variables into Date        
        dataPlotTarget <- mutate(dataPlotTarget, Date = paste(Date, Time, sep=' ')) 
      
        ## Convert Date from text to date type
        dataPlotTarget$Date <- strptime(dataPlotTarget$Date, format="%d/%m/%Y %H:%M:%S", 
                                tz="America/Los_Angeles")

        ## Remove unneeded Time variable
        dataPlotTarget <- dataPlotTarget[ , c(1, 3:9)]
        date <- dataPlotTarget$Date
        weekday <- wday(date, label=TRUE, abbr=TRUE)

        lineColors <- c("black", "red", "blue")
        plotLayout <- c(2,2)
        innerMargins <- c(5,4,2,2)
        outerMargins <- c(2,2,2,2)

        ## open connection to png file device
        png(filename="./plot4.png", width=480, height=480, units="px")
        par(mfcol=plotLayout, mar=innerMargins, oma=outerMargins)


        ## First plot in layout - same as Prog Assignment 1 - Plot 2
        with(dataPlotTarget, plot(date, Global_active_power, type="l", xlab="", 
                                  ylab="Global Active Power"))

        ## Second plot in layout - same as Prog Assignment 1 - Plot 3
        with(dataPlotTarget, plot(date, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering"))
        with(subset(dataPlotTarget, dataPlotTarget$Sub_metering_1>0), points(date, 
                                        Sub_metering_1, col="black", type="l"))
        with(subset(dataPlotTarget, dataPlotTarget$Sub_metering_2>0), points(date, 
                                        Sub_metering_2, col="red", type="l"))
        with(subset(dataPlotTarget, dataPlotTarget$Sub_metering_3>0), points(date, 
                                        Sub_metering_3, col="blue", type="l"))

        legend("topright", col=lineColors, lty=1,  bty="n", cex=0.70, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))                  

        ## Third plot in layout
        with(dataPlotTarget, plot(date, Voltage, type="l", xlab="date/time"))

        ## Fourth plot in layout
        with(dataPlotTarget, plot(date, Global_reactive_power, type="l", xlab="date/time"))

        ## close connection to png file device
        dev.off()

## print(head(dataPlotTarget[1:4], n=5))     ## test
## print(tail(dataPlotTarget[1:4], n=5))     ## test

     return("done plot4")
}
