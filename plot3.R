plot3.R <- function() {
  
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
  ## QUESTION 3
  ## Of the four types of sources indicated by the type (point, nonpoint, onroad, 
  ## nonroad) variable, which of these four sources have seen decreases in 
  ## emissions from 1999–2008 for Baltimore City? Which have seen increases in 
  ## emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
  ## answer this question.
  
  ## Sumarize all emmissions data by year for Baltimore City, MD (fips="24510")
  summary <- NEI %>%
    filter(fips=="24510") %>%
    select(Emissions,type,year) %>%
    group_by(type,year) %>% 
    summarize(total_emissions = sum(Emissions))
  
  ## Create plot
  plot3 <- ggplot(summary,aes(x = year, y = total_emissions)) +
      geom_point() +
      facet_grid(.~type) +
      geom_smooth(method="lm", se=FALSE, color = "red") +
      xlab("Year") +
      ylab("Total Emissions from PM2.5 (tons)") +
      ggtitle("Total Emissions from PM2.5 by Type in Baltimore City, MD")
  png(filename="plot3.png")
  print(plot3)
  dev.off()
  
  ## NON-ROAD, NONPOINT, ON-ROAD all have clear negative trends.
  ## POINT has a slightly positive trend, but more data should be collected to get a better sense of trend.
  
  ## Don't return anything from function
  invisible()
  
}