plot2 <- function() {
 
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
        
        ## connect to png device
        png(filename="./plot2.png", width=480, height=480, units="px")
        ## plot
        with(dataPlotTarget, plot(date, Global_active_power, type="l", xlab="", 
                                  ylab="Global_active_power (kilowatts)"))
        ## close connect to png device
        dev.off()


## print(head(dataPlotTarget[1:4], n=5))     ## test
## print(tail(dataPlotTarget[1:4], n=5))     ## test

     return("done plot2")
}
