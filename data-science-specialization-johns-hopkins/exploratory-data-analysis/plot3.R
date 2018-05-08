## Plot 3 - Energy Sub Metering by day

##  Setting working directory
setwd("C://GitHub//ExData_Plotting1")

##  Loading the data from the online source
##  Dataset : Electrical Power Consumption - present in the working directory folder as a zip file

##  Getting the column names of the data and extracting into vector
colNames <- read.table(unz("exdata-data-household_power_consumption.zip", 
                           "household_power_consumption.txt"), sep=";", na.strings="?",
                       nrows=1, stringsAsFactors = FALSE)
colNames <- as.character(as.vector(colNames[1,]))

##  Data required from 2007-02-01 and 2007-02-02
##  By observation of the table, starting row is 66638 till 60 * 24 * 2 = 2880 rows after
##  This is because it's a two day period with 24 hours per day, 60 minutes each hour

data <- read.table(unz("exdata-data-household_power_consumption.zip", 
                       "household_power_consumption.txt"), sep=";", na.strings="?",
                   nrows=2880, skip = 66637, col.names = colNames, header=TRUE)

##  Renaming the columns appropriately
colnames(data) <- colNames

## Removing extra variables 
rm(colNames)

##  Converting Date and Time variables to Date/Time classes
data$Date <- as.Date(data$Date, "%d/%m/%Y")

##  Saving to png file - opening png device first
png(filename = "plot3.png",
    width = 480, height = 480, units = "px")

##  Plot 3 - Submetering by day
  ##  Submetering 1
  plot(data$Sub_metering_1, 
     type="l", xaxt="n", ylab="Energy sub metering")
  ##  Submetering 2
  lines(data$Sub_metering_2, 
     type="l", col="RED")
  ##  Submetering 3
  lines(data$Sub_metering_3, 
     type="l", col="BLUE")
  ##  Formating axis
  axis(1, at=seq(1,2881,1440), labels=c("Thu", "Fri", "Sat"))
  ##  Legend
  legend('topright', c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3") , 
       lty=1, col=c('black', 'red', 'blue'), cex=.75)

##  Switching device off
dev.off()