### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      06-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a dot plot  to compare the percentage of the population vaccinated in Europe 

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

### PERCENTAGE VACCINATED ###

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
