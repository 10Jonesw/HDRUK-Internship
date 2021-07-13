### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      07-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a Diverging  bar chart to compare deaths per million in Europe
# Learnt how to normalise data 

### LIBRARIES ##################################################################################################################

library(tidyverse)
library(ggplot2)
library(Hmisc)
library(scales)

### SUBSET DIRECTORIES #########################################################################################################

SubsetDatasets.dir <- "~/Desktop/HDRUK-Internship-2021/Data/SubsetDatasets/x"
Graphs.dir <- "~/Desktop/HDRUK-Internship-2021/Data/Graphs"

### CODE #######################################################################################################################

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim("owid-covid-data.txt")

# Select rows corresponding to 30-06-2021 for each country 
CurrentData <- subset(owid,owid$date=="2021-06-30" )                              ##Note2Self: Only 209 out of 230 countries present - Could repeat this code for the 2021-06-29 to include UK vaccination data

# Remove data relating to grouped OWI_ data
CurrentData<-CurrentData[!(CurrentData$iso_code=="OWID_AFR" | 
                             CurrentData$iso_code=="OWID_ASI"|
                             CurrentData$iso_code=="OWID_EUR"|
                             CurrentData$iso_code=="OWID_EUN"|
                             CurrentData$iso_code=="OWID_NAM"|
                             CurrentData$iso_code=="OWID_OCE"|
                             CurrentData$iso_code=="OWID_SAM"|
                             CurrentData$iso_code=="OWID_WRL"),]                                           ##Note2Self: Only 201 out of 230 countries present 

# Subset dataset for Europe 
CurrentData <- subset(CurrentData,CurrentData$continent=="Europe" ) 

# Create a dataset of the countries with missing data
na_deaths <- which(!complete.cases(CurrentData$total_deaths_per_million))

# Create a dataset with the remaining countries with present death data 
CurrentData <- CurrentData[-na_deaths,]

# Normalise total deaths per million data
CurrentData["Normalised_Deaths"]<- round((CurrentData$total_deaths_per_million - mean(CurrentData$total_deaths_per_million))/sd(CurrentData$total_deaths_per_million), 2)

# Create new column to sort countries into death per million above and below the average
CurrentData["Death_type"] <- ifelse(CurrentData$Normalised_Deaths < 0, "below", "above") 

# Sort data in order of high to low
CurrentData <- CurrentData[order(CurrentData$Normalised_Deaths), ]

# Convert to factor to keep the sorted order
CurrentData$location<- factor(CurrentData$location, levels = CurrentData$location)

# Plot diverging bars
ggplot(CurrentData, aes(x=location, y=Normalised_Deaths, label=Normalised_Deaths)) + 
  geom_bar(stat='identity', aes(fill=Death_type), width=.5) +
  scale_fill_manual(name="Deaths per Million", labels = c("Above Average", "Below Average"), values = c("above"="#f8766d", "below"="#00ba38")) +
  coord_flip() +
  ylab('Normalised Total Deaths per Million') + xlab('Country') +
  ggtitle("Deaths Per million in Europe (30-06-2021)") +
  theme(plot.title = element_text(hjust = 0.5))

# Save Plot
ggsave("DIVERGING BAR Deaths per million.png", width = 20, limitsize = FALSE, path = Graphs.dir)     










