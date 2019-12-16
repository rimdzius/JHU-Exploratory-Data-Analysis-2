plot2.R <- function() {
  
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
  ## QUESTION 2
  ## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
  ## (fips=="24510") from 1999 to 2008? Use the base plotting system to make a 
  ## plot answering this question.
  
  ## Subset the NEI data with the appropriate fips (Baltimore City, MD)
  NEI_subset <- subset(NEI, fips == "24510")
  ## Sumarize all emmissions data by year
  summary <- with(NEI_subset, tapply(Emissions, year, sum))
  ## Create data frame with proper classes
  summary_data_frame <- data.frame(Emissions = unname(summary), Year = as.Date(as.character(unique(NEI_subset$year)),"%Y"))
  ## Create plot with trendline
  png(filename="plot2.png")
  with(summary_data_frame, plot(Year, Emissions, pch = 16, xlab = "Year", ylab = "Total Emissions from PM2.5 (tons)", main = "Total Emissions from PM2.5 (tons) in Baltimore City, MD"))
  with(summary_data_frame, abline(lm(Emissions ~ Year), col="red"))
  dev.off()
  
  ## There is a negative trend, though data from 2006 was higher than 2003.
  
  ## Don't return anything from function
  invisible()
  
}