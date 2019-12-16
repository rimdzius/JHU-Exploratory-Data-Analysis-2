plot1.R <- function() {
  
  #############################################################################
  ## Download and Read data
  ## Same for every graph.
  
  ## Download data, if not available
  if(!(file.exists("summarySCC_PM25.rds") & file.exists("Source_Classification_Code.rds"))) {
    courseDataURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    destFile <- "data.zip"
    download.file(courseDataURL, destFile)
    unzip(destFile)
  }
  
  ## Read in data
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  #############################################################################
  ## QUESTION 1
  ## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
  ## Using the base plotting system, make a plot showing the total PM2.5 emission from 
  ## all sources for each of the years 1999, 2002, 2005, and 2008.
  
  ## Sumarize all emmissions data by year
  summary <- with(NEI, tapply(Emissions, year, sum))
  ## Create data frame with proper classes
  summary_data_frame <- data.frame(Emissions = unname(summary), Year = as.Date(as.character(unique(NEI$year)),"%Y"))
  ## Create plot with trendline
  png(filename="plot1.png")
  with(summary_data_frame, plot(Year, Emissions, pch = 16, xlab = "Year", ylab = "Total Emissions from PM2.5 (tons)", main = "Total Emissions from PM2.5 (tons) in the United States"))
  with(summary_data_frame, abline(lm(Emissions ~ Year), col="red"))
  dev.off()
  
  ## There is a clear negative trend to the data.
  
  ## Don't return anything from function
  invisible()
  
}