### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      05-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a scatter graph to compare the total cases and deaths per million for countries within Europe 

### LIBRARIES ##################################################################################################################

library(tidyverse)
library(ggplot2)
library(Hmisc)
library(scales)

### SUBSET DIRECTORIES #########################################################################################################

SubsetDatasets.dir <- "~/Desktop/HDRUK-Internship-2021/Data/SubsetDatasets/x"
Graphs.dir <- "~/Desktop/HDRUK-Internship-2021/Data/Graphs"

### CODE #######################################################################################################################

### CONTINENT LEVEL DATA ###

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim("owid-covid-data.txt")

# Find details about the dataset
describe (owid)

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

# Look at how many continents there are 
Continent <- unique(CurrentData[,2])

# Subset dataset for Europe 
Europe <- subset(CurrentData,CurrentData$continent=="Europe" )  

# Generate scatter graph for COVID deaths and cases [GRAPH 5]
ggplot(Europe, aes(total_deaths_per_million, total_cases_per_million, color = human_development_index)) +
  geom_point() +
  geom_text(aes(label=location), size=3, hjust = 0.5, vjust = 1.5) +
  geom_point(size = 2) +
  scale_color_gradient(low="orange", high="Red") +
  xlab('Total Number of Deaths per Million') + ylab('Total Number of Cases per Million') +
  scale_y_continuous(breaks=seq(0, 200000, 20000)) + 
  scale_x_continuous(breaks=seq(0, 4000, 500)) +
  ggtitle("Total Number of COVID deaths and cases in Europe (30-06-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Human Devlopment Index")

# Save Plot
ggsave("Europe Cases and Deaths.png",width = 20, limitsize = FALSE, path = Graphs.dir)

