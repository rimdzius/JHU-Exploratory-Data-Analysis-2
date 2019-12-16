plot5.R <- function() {
  
  require(dplyr)
  require(ggplot2)
  
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
  ## QUESTION 5
  ## How have emissions from motor vehicle sources changed from 1999–2008 in 
  ## Baltimore City?

  
  ## Subset SCC data to only include "Vehicle" in the EI Sector column.
  SCC_subset <- SCC[grepl("[Vv]ehicle",SCC$EI.Sector),]
  ## Subset NEI with the subsetted SCC codes, and by Baltimore City MD (fips = 24510)
  NEI_subset <- NEI %>% 
    filter(NEI$SCC %in% as.character(SCC_subset$SCC) & NEI$fips == "24510") %>%
    select(Emissions,year) %>% 
    group_by(year) %>%
    summarize(total_emissions = sum(Emissions))
  
  ## Create plot
  plot5 <- ggplot(NEI_subset,aes(x = year, y = total_emissions))+
    geom_point() +
    geom_smooth(method="lm", se = FALSE, color = "red") +
    xlab("Year") +
    ylab("Motor Vehicle related Emissions from PM2.5 (tons)") +
    ggtitle("Motor Vehicle related Emissions from PM2.5 in Baltimore City, MD")
  png(filename="plot5.png")
  print(plot5)
  dev.off()
  
  ## There is a negative trend, though the 1999 data skews it to be more negative.
  
  ## Don't return anything from function
  invisible()
  
}