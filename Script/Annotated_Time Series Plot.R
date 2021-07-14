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

#################

# Plot Annotated Graph for NEW_CASES_SMOOTHED
ggplot(UK, aes(x=date, y = new_cases_smoothed, group = 1)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(label=comma) +
  ylab('New cases') + xlab('Date') +
  ggtitle("Daily confirmed COVID cases by date reported in the UK") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-26")), linetype=2, colour = "red", lwd = 0.5) +
       annotate("text", x=as.Date("2020-03-26"), y=13000, label= "First Lockdown (26 Mar)", angle=90, hjust = -1, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-04")), linetype=3, colour = "blue", lwd = 0.5) +
        annotate("text", x=as.Date("2020-07-04"), y=13000, label= "Local Lockdown in Leicester (04 Jul)", angle=90, hjust = -0.45, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2020-10-14")), linetype=3, colour = "blue", lwd = 0.5) +
        annotate("text", x=as.Date("2020-10-14"), y=13000, label= "3 tier restrictions (14 Oct)", angle=90, hjust = -1.1, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-24")), linetype=3, colour = "dark green", lwd = 0.5) +
       annotate("text", x=as.Date("2020-07-24"), y=13000, label= "Mandatory Face Masks in shops (24 Jul)", angle=90, hjust = -0.3, vjust = -0.5, colour = "dark green")+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype=2, colour = "red", lwd = 0.5) +
       annotate("text", x=as.Date("2020-11-05"), y=13000, label= "Second Lockdown (05 Nov)", angle=90, hjust = -0.8, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-12-21")), linetype=3, colour = "blue", lwd = 0.5) +
    annotate("text", x=as.Date("2020-12-21"), y=13000, label= "Tier 4 restictions (21 Dec)", angle=90, hjust = -1.05, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-06")), linetype=2, colour = "red", lwd = 0.5) +
    annotate("text", x=as.Date("2021-01-06"), y=13000, label= "Third Lockdown (06 Jan)", angle=90, hjust = -1, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2021-02-15")), linetype=3, colour = "dark green", lwd = 0.5) +
   annotate("text", x=as.Date("2021-02-15"), y=13000, label= "Travel quarantine (15 Feb)", angle=90, hjust = -1, vjust = -0.5, colour = "dark green")

# Save Plot
ggsave("TIME SERIES PLOT new cases smoothed UK.png",width = 25, limitsize = FALSE, path = Time_Series_Graphs.dir)

#################

# Plot Annotated Graph for NEW_CASES
ggplot(UK, aes(x=date, y = new_cases, group = 1)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(label=comma) +
  ylab('New cases') + xlab('Date') +
  ggtitle("Daily confirmed COVID cases by date reported in the UK") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-26")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-03-26"), y=13000, label= "First Lockdown (26 Mar)", angle=90, hjust = -1, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-04")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-07-04"), y=13000, label= "Local Lockdown in Leicester (04 Jul)", angle=90, hjust = -0.45, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2020-10-14")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-10-14"), y=13000, label= "3 tier restrictions (14 Oct)", angle=90, hjust = -1.1, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-24")), linetype=3, colour = "dark green", lwd = 0.5) +
  annotate("text", x=as.Date("2020-07-24"), y=13000, label= "Mandatory Face Masks in shops (24 Jul)", angle=90, hjust = -0.3, vjust = -0.5, colour = "dark green")+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-11-05"), y=13000, label= "Second Lockdown (05 Nov)", angle=90, hjust = -0.8, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-12-21")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-12-21"), y=13000, label= "Tier 4 restictions (21 Dec)", angle=90, hjust = -1.05, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-06")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2021-01-06"), y=13000, label= "Third Lockdown (06 Jan)", angle=90, hjust = -1, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2021-02-15")), linetype=3, colour = "dark green", lwd = 0.5) +
  annotate("text", x=as.Date("2021-02-15"), y=13000, label= "Travel quarantine (15 Feb)", angle=90, hjust = -1, vjust = -0.5, colour = "dark green")

# Save Plot
ggsave("TIME SERIES PLOT new cases UK.png",width = 25, limitsize = FALSE, path = Time_Series_Graphs.dir)

#################

# Plot Annotated Graph for NEW_DEATHS_SMOOTHED
ggplot(UK, aes(x=date, y = new_deaths_smoothed, group = 1)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(limits =c(0, 1500), label=comma) +
  ylab('New Deaths') + xlab('Date') +
  ggtitle("Daily confirmed COVID deaths by date reported in the UK") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-26")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-03-26"), y=1000, label= "First Lockdown (26 Mar)", angle=90, hjust = 0, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-04")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-07-04"), y=1000, label= "Local Lockdown in Leicester (04 Jul)", angle=90, hjust = 0.3, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2020-10-14")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-10-14"), y=1000, label= "3 tier restrictions (14 Oct)", angle=90, hjust = 0, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-24")), linetype=3, colour = "dark green", lwd = 0.5) +
  annotate("text", x=as.Date("2020-07-24"), y=1000, label= "Mandatory Face Masks in shops (24 Jul)", angle=90, hjust = 0.35, vjust = -0.5, colour = "dark green")+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-11-05"), y=1000, label= "Second Lockdown (05 Nov)", angle=90, hjust = 0.15, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-12-21")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-12-21"), y=1000, label= "Tier 4 restictions (21 Dec)", angle=90, hjust = 0.05, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-06")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2021-01-06"), y=1000, label= "Third Lockdown (06 Jan)", angle=90, hjust = 0.05, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2021-02-15")), linetype=3, colour = "dark green", lwd = 0.5) +
  annotate("text", x=as.Date("2021-02-15"), y=1000, label= "Travel quarantine (15 Feb)", angle=90, hjust = 0.1, vjust = -0.5, colour = "dark green")
  

# Save Plot
ggsave("TIME SERIES PLOT new deaths smoothed UK.png",width = 25, limitsize = FALSE, path = Time_Series_Graphs.dir)

#################

# Plot Annotated Graph for NEW_DEATHS
ggplot(UK, aes(x=date, y = new_deaths, group = 1)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(limits =c(0, 1500), label=comma) +
  ylab('New Deaths') + xlab('Date') +
  ggtitle("Daily confirmed COVID deaths by date reported in the UK") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-26")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-03-26"), y=1000, label= "First Lockdown (26 Mar)", angle=90, hjust = 0, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-04")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-07-04"), y=1000, label= "Local Lockdown in Leicester (04 Jul)", angle=90, hjust = 0.3, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2020-10-14")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-10-14"), y=1000, label= "3 tier restrictions (14 Oct)", angle=90, hjust = 0, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-24")), linetype=3, colour = "dark green", lwd = 0.5) +
  annotate("text", x=as.Date("2020-07-24"), y=1000, label= "Mandatory Face Masks in shops (24 Jul)", angle=90, hjust = 0.35, vjust = -0.5, colour = "dark green")+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-11-05"), y=1000, label= "Second Lockdown (05 Nov)", angle=90, hjust = 0.15, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2020-12-21")), linetype=3, colour = "blue", lwd = 0.5) +
  annotate("text", x=as.Date("2020-12-21"), y=1000, label= "Tier 4 restictions (21 Dec)", angle=90, hjust = 0.05, vjust = -0.5, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-06")), linetype=2, colour = "red", lwd = 0.5) +
  annotate("text", x=as.Date("2021-01-06"), y=1000, label= "Third Lockdown (06 Jan)", angle=90, hjust = 0.05, vjust = -0.5, colour = "red", fontface = 'bold') +
  geom_vline(xintercept = as.numeric(as.Date("2021-02-15")), linetype=3, colour = "dark green", lwd = 0.5) +
  annotate("text", x=as.Date("2021-02-15"), y=1000, label= "Travel quarantine (15 Feb)", angle=90, hjust = 0.1, vjust = -0.5, colour = "dark green")


# Save Plot
ggsave("TIME SERIES PLOT new deaths UK.png",width = 25, limitsize = FALSE, path = Time_Series_Graphs.dir)
