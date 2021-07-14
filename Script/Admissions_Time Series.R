### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      12-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a time series graph for the number of hospital admissions and ICU patients in the UK
# Annotated

### LIBRARIES ##################################################################################################################

library(tidyverse)
library(ggplot2)
library(Hmisc)
library(scales)
library(reshape2)

### SUBSET DIRECTORIES #########################################################################################################

SubsetDatasets.dir <- "~/Desktop/HDRUK-Internship-2021/Data/SubsetDatasets/x"
Graphs.dir <- "~/Desktop/HDRUK-Internship-2021/Data/Graphs"
Time_Series_Graphs.dir  <- "~/Desktop/HDRUK-Internship-2021/Data/Time Series"

### CODE #######################################################################################################################

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim("owid-covid-data.txt")

# Select rows corresponding to UK
UK <- filter(owid , location=="United Kingdom")


# Convert the data column from "character' to 'Date'  variable type 
class(UK$date)
UK$date <- as.Date(UK$date)
class(UK$date)

# Select for relevant columns from the dataset
UK <- UK %>% select(4, 18, 20)

# Reshape the data into long format
data_long <- melt(UK, id.vars = "date")

# Plot time series
ggplot(data_long, aes(x=date, y = value, col = variable)) + 
  geom_line() +
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0.8), show.legend = F) + 
  aes(group=rev(variable))+
  scale_color_manual(values = c("turquoise4", "darkgoldenrod3")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(fill = "Dose (mg)")+
  scale_y_continuous(label=comma)+
ylab('Patients') + xlab('Date') +
  ggtitle("Daily Hospital and ICU patients reported in the UK") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-26")), linetype=2, colour = "black", lwd = 0.5) +
  annotate("text", x=as.Date("2020-03-26"), y=20000, label= "First Lockdown (26 Mar)", angle=90, hjust = -1, vjust = -0.5, colour = "black", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-04")), linetype=3, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-07-04"), y=20000, label= "Local Lockdown in Leicester (04 Jul)", angle=90, hjust = -0.45, vjust = -0.5, colour = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2020-10-14")), linetype=3, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-10-14"), y=20000, label= "3 tier restrictions (14 Oct)", angle=90, hjust = -1.1, vjust = -0.5, colour = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-24")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-07-24"), y=20000, label= "Mandatory Face Masks in shops (24 Jul)", angle=90, hjust = -0.3, vjust = -0.5, colour = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype=2, colour = "black", lwd = 0.5) +
  annotate("text", x=as.Date("2020-11-05"), y=20000, label= "Second Lockdown (05 Nov)", angle=90, hjust = -0.8, vjust = -0.5, colour = "black", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-12-21")), linetype=3, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-12-21"), y=20000, label= "Tier 4 restictions (21 Dec)", angle=90, hjust = -1.05, vjust = -0.5, colour = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-06")), linetype=2, colour = "black", lwd = 0.5) +
  annotate("text", x=as.Date("2021-01-06"), y=20000, label= "Third Lockdown (06 Jan)", angle=90, hjust = -1, vjust = -0.5, colour = "black", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2021-02-15")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2021-02-15"), y=20000, label= "Travel quarantine (15 Feb)", angle=90, hjust = -1, vjust = -0.5, colour = "blue")

# Save Plot
ggsave("TIME SERIES PLOT Admissions UK.png",width = 25, limitsize = FALSE, path = Time_Series_Graphs.dir)
