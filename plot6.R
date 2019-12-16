plot6.R <- function() {
  
  require(plyr)
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
  ## Compare emissions from motor vehicle sources in Baltimore City with emissions
  ## from motor vehicle sources in Los Angeles County, California fips=="06037".
  ## Which city has seen greater changes over time in motor vehicle emissions?
  
  
  ## Subset SCC data to only include "Vehicle" in the EI Sector column.
  SCC_subset <- SCC[grepl("[Vv]ehicle",SCC$EI.Sector),]
  ## Subset NEI with the subsetted SCC codes, and by Baltimore City (fips = 24510) Or LA (fips == 06037)
  NEI_subset <- NEI %>% 
    filter(NEI$SCC %in% as.character(SCC_subset$SCC) & (NEI$fips == "24510" | NEI$fips == "06037")) %>%
    mutate(fips = as.factor(fips)) %>%
    select(Emissions,year,fips) %>% 
    group_by(year, fips) %>%
    summarize(total_emissions = sum(Emissions))
  ## Rename fips labels to be descriptive
  NEI_subset$fips <- mapvalues(NEI_subset$fips, from=c("24510", "06037"), to=c("Baltimore City, MD", "LA County, CA"))
  
  ## Create plot
  plot6 <- ggplot(NEI_subset,aes(x = year, y = total_emissions))+
    geom_point() +
    facet_grid(.~fips) +
    geom_smooth(method="lm", se = FALSE, color = "red") +
    xlab("Year") +
    ylab("Motor Vehicle related Emissions from PM2.5 (tons)") +
    ggtitle("Motor Vehicle related Emissions from PM2.5 from 1999 to 2008")
  png(filename="plot6.png")
  print(plot6)
  dev.off()
  
  ## LA county has a slightly positive trend. Baltimore City has a slightly negative trend.
  ## LA county has a much higher overall level of emissions vs. Baltimore City.
  
  ## Don't return anything from function
  invisible()
  
}
