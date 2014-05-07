library(data.table)

# Main function to run in order to download a file and produce a plot
plot4 <- function(){
        #Download the source file
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        data <- readData(url)
        
        #parse the data (subset, set column types etc.)
        data <- parseData(data)
        
        #plot 1st visualization (plot4.png)
        plotChart(data)
}

# This function will download the source file from a provided URL and place it in a "./data" directory as "input.zip".
# If the ".data" directory does not exist the function will create it. 
# After downloading the file, the function will unzip it and read it into a data.table using fread().
readData <- function (fileUrl){
        print("Checking if data directory exists...")
        if(!file.exists("./data")){
                print("Creating data directory")
                dir.create("./data")
        }
        else{
                print("Data directory exists, moving on...")
        }
        
        print("Attempting to download data source, this could take some time depending on the file size...")
        download.file(url=fileUrl,destfile="./data/input.zip",method="curl")
        
        print("Unzipping file...")
        unzip("./data/input.zip",exdir="./data")
        
        print("Reading file...")
        df <- fread(input="./data/household_power_consumption.txt", sep=";")
}

# This function parses the downloaded file to format it nicely for us to analyze and plot.
# The function will subset the file to get only data from February 1st and 2nd 2007,
# will convert the "Date" and "Time" columns to one DateTime column and will convert all other columns to numeric columns.
parseData <- function(df){
        print("Subsetting relevant dates...")
        df <- subset(df,df$Date %in% c("1/2/2007", "2/2/2007"))
        print("Parsing data...")
        cols <- names(df)[3:9]
        DateTime <- strptime(paste(df$Date,df$Time),format="%d/%m/%Y %H:%M:%S")
        DateTime <- data.frame(DateTime)
        df <- subset(df,select=cols)
        df <- sapply(df,as.numeric)
        df <- cbind(DateTime,df)
}

# This function actually plots the required plot4 graph and saves it as a png image name "plot4.png"
plotChart <- function(df){
        print("Plotting visualization to: plot4.png")
        png(filename="plot4.png",width=480,height=480)
        #set plot area to 4x4
        par(mfcol=c(2,2))
        #plot1
        plot(df$Global_active_power ~ df$DateTime, type="l", ylab="Global Active Power", xlab="")
        #plot2
        plot(df$Sub_metering_1 ~ df$DateTime, type="l", ylab="Energy sub metering", xlab="")
        lines(df$Sub_metering_2 ~ df$DateTime, type="l", col="red")
        lines(df$Sub_metering_3 ~ df$DateTime, type="l", col="blue")
        legend("topright", names(df)[6:8], lty=c(1,1,1), lwd=c(2,2,2), col=c("black","red","blue"),box.lwd=0, inset=0.01)
        #plot3
        plot(df$Voltage ~ df$DateTime, type="l", ylab="Voltage", xlab="datetime")
        #plot4
        plot(df$Global_reactive_power ~ df$DateTime, type="l", ylab="Global_reactive_power", xlab="datetime")
        dev.off()
}