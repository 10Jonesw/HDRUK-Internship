### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      08-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a pie chart to compare continents with the most confirmed cases 

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

# Filter for overall continent data 
Continent_data <- filter(CurrentData, iso_code=="OWID_AFR" | iso_code=="OWID_ASI" | iso_code=="OWID_EUR" | iso_code=="OWID_NAM" | iso_code=="OWID_OCE"|iso_code=="OWID_SAM"|iso_code=="OWID_WRL")

# Create a new column to calculate the % of cases for each continent
Continent_data["Cases"] <- Continent_data$total_cases / 182202370

# Remove the world data 
Continent_data <- Continent_data %>% slice(1:6)

# Steven: Don't include the world data in the first place, and use this to calculate %
#Continent_data["bob"] <- Continent_data$total_cases / sum(Continent_data$total_cases)

# Draw a pie chart 
ggplot(Continent_data, aes(x = "", y=-Cases, fill = reorder(location, -Cases))) + 
  geom_bar(width = 1, stat = "identity", colour = "black") +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) +
  coord_polar(theta = "y", start=0) +
  labs(fill="Continent",  x=NULL, y=NULL)+
  theme_minimal() +
  theme (panel.grid=element_blank(), axis.text = element_blank()) +
  geom_text(aes(label = scales::percent(round(Cases,3))), position = position_stack(vjust = 0.5), size = 5)+
  ggtitle("Top 10 continents with the most COVID cases (30-06-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.text = element_text(size = 10), legend.title = element_text(size = 13), plot.background = element_rect("white"))

# Save Plot
ggsave("PIE CHART Top continents.png", width = 20, limitsize = FALSE, path = Graphs.dir)    

#################### Europe data 

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim("owid-covid-data.txt")
# You don't need to do this again. Data is alreayd there

# Select rows corresponding to 30-06-2021 for each country 
CurrentData <- subset(owid,owid$date=="2021-06-30" )  

# Subset dataset for Europe 
Europe <- subset(CurrentData,CurrentData$continent=="Europe" ) 

# Create a new column to calculate the % of cases for each continent
Europe["Europe_Cases"] <- Europe$total_cases / 48260346

# Create a dataset of the countries with missing data
na_cases <- which(!complete.cases(Europe$total_cases))

# Create a dataset with the remaining countries with present death data 
Europe <- Europe[-na_cases,]

# Sort data in order of high to low
Europe <- Europe[order(-Europe$Europe_Cases), ]

# Select the top 10 countries 
Europe <- Europe %>% slice(1:10)

# Draw a pie chart 
ggplot(Europe, aes(x = "", y=-Europe_Cases, fill = reorder(location, -Europe_Cases))) + 
  geom_bar(width = 1, stat = "identity", colour = "black") +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) +
  coord_polar(theta = "y", start=0) +
  labs(fill="Country",  x=NULL, y=NULL)+
  theme_minimal() +
  theme (panel.grid=element_blank(), axis.text = element_blank()) +
  geom_text(aes(label = scales::percent(round(Europe_Cases,3))), position = position_stack(vjust = 0.5), size = 3, angle = 0, vjust = -1)+
  ggtitle("Top 10 countries within Europe with the most COVID cases (30-06-2021)") +
  theme(plot.title = element_text(hjust = -1.8)) +
  theme(legend.text = element_text(size = 10), legend.title = element_text(size = 13), plot.background = element_rect("white"))

# Save Plot
ggsave("PIE CHART Top 10 Europe countries.png", width = 20, limitsize = FALSE, path = Graphs.dir)  
