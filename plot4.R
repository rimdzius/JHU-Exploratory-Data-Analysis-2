plot4.R <- function() {
  
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
  ## QUESTION 4
  ## Across the United States, how have emissions from coal combustion-related 
  ## sources changed from 1999–2008?
  
  ## Subset SCC data to only include rows with "Coal" in the EI Sector column.
  SCC_subset <- SCC[grepl("[Cc]oal",SCC$EI.Sector),]
  ## Subset NEI with the subsetted SCC codes
  NEI_subset <- NEI %>% 
    filter(NEI$SCC %in% as.character(SCC_subset$SCC)) %>%
    select(Emissions,year) %>% 
    group_by(year) %>%
    summarize(total_emissions = sum(Emissions))
  
  ## Create plot
  plot4 <- ggplot(NEI_subset,aes(x = year, y = total_emissions))+
      geom_point() +
      geom_smooth(method="lm", se = FALSE, color = "red") +
      xlab("Year") +
      ylab("Coal-Related Emissions from PM2.5 (tons)") +
      ggtitle("Coal-Related Emissions from PM2.5 in United States")
  png(filename="plot4.png")
  print(plot4)
  dev.off()
  
  ## There is a negative trend in Coal-related emissions, though the 2008 data skews it to be more negative.
  
  ## Don't return anything from function
  invisible()
  
}