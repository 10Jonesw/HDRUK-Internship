### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      08-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a time series graph for the number of confirmed cases in the UK

### LIBRARIES ##################################################################################################################

library(tidyverse)
library(ggplot2)
library(Hmisc)
library(scales)
library(reshape2)

### SUBSET DIRECTORIES #########################################################################################################

SubsetDatasets.dir <- "~/Desktop/HDRUK-Internship-2021/Data/SubsetDatasets/x"
Graphs.dir <- "~/Desktop/HDRUK-Internship-2021/Data/Graphs"

### CODE #######################################################################################################################

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim("owid-covid-data.txt")

# Select rows corresponding to UK
UK <- filter(owid, location=="United Kingdom")

# Convert the data column from "character' to 'Date'  variable type 
class(UK$date)
UK$date <- as.Date(UK$date)
class(UK$date)

#PLOT A SINGLE 
ggplot(UK, aes(x=date, y = new_cases_smoothed, group = 1)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(label=comma) +
  ylab('New cases') + xlab('Date') +
  ggtitle("Daily confirmed COVID cases by date reported in the UK") +
  theme(plot.title = element_text(hjust = 0.5))


###### Plot multiple time series on 1 graph 

# Select for relevant columns from the dataset
UK <- UK %>% select(4, 7, 10)

# Reshape the data into long format
data_long <- melt(UK2, id.vars = "date")

# Plot time series
ggplot(data_long, aes(x=date, y = value, col = variable)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(label=comma)

# Save Plot
ggsave("TIME SERIES PLOT: New cases UK.png",width = 25, limitsize = FALSE, path = Graphs.dir)
