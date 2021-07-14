### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      02-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a bar graph  to compare the total deaths and total cases between different countries 
# Created log graphs also 

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

# Find details about the dataset
describe (owid)

# Look at the number of unique countries listed in the database
CountryNames <- unique(owid[,3])
CountryNames

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
                             CurrentData$iso_code=="OWID_WRL"),] ##Note2Self: Only 201 out of 230 countries present 

# Steven's suggestions: filter by continent
# This is quicker and also cuts down on the number of countries in the graph, so you can see everything
#CurrentData <- filter(CurrentData, continent == 'Europe')



# Save the dataset for 30-06-2021
write.table(
  CurrentData, 
  file = paste(SubsetDatasets.dir, "CurrentData.txt", sep = ""),
  row.names = TRUE,
  col.name = TRUE,
  sep = "\t"
);
# Steven's suggestion: csv is a more standard format for data rather than txt.
#write.csv(CurrentData, paste0(SubsetDatasets.dir, "CurrentData.txt"))



### TOTAL CASES ###

CurrentData_CountryName <- CurrentData$location
CurrentData_TotalCases <- CurrentData$total_cases

# Removes scientific notion from graphs 
options( scipen = 999 )

# Plot Total Cases [GRAPH 1]
ggplot() +
  geom_bar(aes(x = reorder(CurrentData_CountryName, -CurrentData_TotalCases), y = CurrentData_TotalCases), stat = "identity", fill = " Red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Total number of COVID Cases per country (30-06-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('') + ylab('Total Number of Cases') +
  scale_y_continuous(breaks=seq(0, 30000000, 5000000))

# Save Plot
ggsave("BAR CHART Total Cases.png",width = 25, limitsize = FALSE, path = Graphs.dir)

# Plot Total Cases (LOG) [GRAPH 2]
ggplot() +
  geom_bar(aes(x = reorder(CurrentData_CountryName, -CurrentData_TotalCases), y = CurrentData_TotalCases), stat = "identity", fill = "dark red") +
  scale_y_log10() +
  annotation_logticks(sides = "lr", outside = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.y = element_text(hjust = 0.3, vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Total number of COVID Cases per country (30-06-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('') + ylab('Total Number of Cases')

# Save Plot
ggsave("BAR CHART Total Cases (log).png",width = 25, limitsize = FALSE, path = Graphs.dir)



### TOTAL DEATHS ###
CurrentData_CountryName <- CurrentData$location
CurrentData_TotalDeath <- CurrentData$total_deaths

# Removes scientific notion from graphs 
options( scipen = 999 )

# Plot Total Deaths [GRAPH 3]
ggplot() +
  geom_bar(aes(x = reorder(CurrentData_CountryName, -CurrentData_TotalDeath), y = CurrentData_TotalDeath), stat = "identity", fill = " blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Total number of COVID deaths per country (30-06-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('') + ylab('Total Number of Deaths') +
  scale_y_continuous(breaks=seq(0, 600000, 50000))

# Save Plot
ggsave("BAR CHART Total Deaths.png",width = 25, limitsize = FALSE, path = Graphs.dir)

# Plot Total Deaths (LOG) [GRAPH 4]
ggplot() +
  geom_bar(aes(x = reorder(CurrentData_CountryName, -CurrentData_TotalDeath), y = CurrentData_TotalDeath), stat = "identity", fill = "dark blue") +
  scale_y_log10() +
  annotation_logticks(sides = "lr", outside = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.y = element_text(hjust = 0.3, vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Total number of COVID deaths per country (30-06-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('') + ylab('Total Number of Deaths')

# Save Plot
ggsave("BAR CHART Total Deaths (log).png",width = 25, limitsize = FALSE, path = Graphs.dir)