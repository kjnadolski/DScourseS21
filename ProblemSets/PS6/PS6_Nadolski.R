## Karley Nadolski
## ECON 5253: Problem Set 6

library(tidyverse)
library(ggplot2)

# 3. Find some data that insterests you and clean it 
    # Use Kaggle datasets - https://www.kaggle.com/jsphyg/weather-dataset-rattle-package
    # to find data to be used for analysis and visualization

 aus_rain <- read.csv("weatherAUS.csv")
 View(aus_rain)

 # Clean the data 
      length(aus_rain$Evaporation)
      sum(aus_rain$Evaporation)
      sum(aus_rain$Sunshine) 
      # both of these variables are entirely NA, drop these two columns
      
      aus_rain1 <- subset(aus_rain, select=-c(Evaporation, Sunshine))
      
      # a. Visualize Rainfall by MaxTemp 
      ggplot() +
        geom_jitter(data=aus_rain1, aes(x=MaxTemp, y=Rainfall, color=Location)) +
        ggtitle("The Relationship Between MaxTemp and Rainfall") +
        theme(
          plot.title=element_text(size=14, lineheight=0.8, color='grey20', hjust=0.5),
          axis.title.x=element_text(color='grey20'),
          axis.title.y=element_text(color='grey20'))
      
      # b. Visualize - AVG Rainfall (or another variable) by Location
          unique(aus_rain1$Location) # 49 different Locations
          
          locations <- unique(aus_rain1$Location)
          avgrainfall <- matrix(data=NA, nrow=length(locations), ncol=2)
          avgrainfall[,1] <- locations
          
          for(i in 1:length(locations)) {
            mean <- mean(aus_rain1$MaxTemp[which(aus_rain1$Location ==locations[i])], na.rm=TRUE)
            avgrainfall[i,2] <- mean
          }
          
          colnames(avgrainfall) <- c("Location", "AvgRainfall")
          avgrainfall <- as.data.frame(avgrainfall)
          avgrainfall$AvgRainfall <- as.numeric(avgrainfall$AvgRainfall)
        
          
          top.rainfall <- avgrainfall %>%
            arrange(desc(AvgRainfall)) %>%
            slice(1:10)
         
          ggplot(top.rainfall, aes(x=Location, y=AvgRainfall, color=Location, fill=Location))+
           geom_bar(stat="identity") +
            xlab("Location") +
            ylab("Average Rainfall") +
            scale_x_discrete(breaks=waiver()) +
            ggtitle("Top 10 Locations in Australia with the Highest Average Rainfall")
          

      # c. Chart High temperature over time for one location
      
        katherine <- aus_rain1[which(aus_rain1$Location=="Katherine"),]  
        class(katherine$Date)
        katherine$Date <- as.Date(katherine$Date)
        
        ggplot(data=katherine, aes(x=Date, y=as.numeric(MaxTemp))) +
          geom_point() +
          geom_smooth() +
          theme_minimal() +
          ylab("Maximum Temperature (C)")+
          ggtitle("Daily High Temperatures in Katherine, Australia")
        
      
      