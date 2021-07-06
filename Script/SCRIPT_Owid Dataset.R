### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      01-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 

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
    CurrentData$iso_code=="OWID_WRL"),]                                           ##Note2Self: Only 201 out of 230 countries present 

# Save the dataset for 30-06-2021
write.table(
  CurrentData, 
  file = paste(SubsetDatasets.dir, "CurrentData.txt", sep = ""),
  row.names = TRUE,
  col.name = TRUE,
  sep = "\t"
);

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
ggsave("Total Cases.png",width = 25, limitsize = FALSE, path = Graphs.dir)

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
ggsave("Total Cases (LOG).png",width = 25, limitsize = FALSE, path = Graphs.dir)



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
ggsave("Total Deaths.png",width = 25, limitsize = FALSE, path = Graphs.dir)

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
ggsave("Tota lDeaths (LOG).png",width = 25, limitsize = FALSE, path = Graphs.dir)



### CONTINENT LEVEL DATA ###


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


### PERCENTAGE VACCINATED ###

# Calculate the % of the population vaccinated 
Europe["Percentage_Vaccinated"] <- Europe$people_vaccinated / Europe$population * 100

# Find the number of countries missing % vaccination data
sum(is.na(Europe$Percentage_Vaccinated))

# Create a dataset of the 30 countries with missing % vaccination data 
na_vaccination <- which(!complete.cases(Europe$Percentage_Vaccinated))

# Create a dataset with the remaining countries with present vaccination data 
Europe_vaccination_data <- Europe[-na_vaccination,]

# Create Dot plot for vaccination data [GRAPH 6]
ggplot(Europe_vaccination_data, aes(x = reorder(location, Percentage_Vaccinated), y = Percentage_Vaccinated), na.rm = TRUE) +
  geom_point(col="tomato2", size=3) +
  coord_flip() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line( size=0.3, color="grey", linetype = 'dashed' )) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  ylab('Percentage (%)') + xlab('Country') +
  ggtitle("Percentage of the population vaccinated in Europe (30-06-2021)") +
  theme(plot.title = element_text(hjust = 0.5))

# Save Plot
ggsave("Percentage of the population vaccination.png", width = 20, limitsize = FALSE, path = Graphs.dir)
  