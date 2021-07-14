### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      12-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a scatter graph with Loess of regression for cases before and after lockdown (30 days and 60 days)


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

### 30 days before and after lockdown 1 ### 

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim("owid-covid-data.txt")

# Select rows corresponding to UK
UK <- filter(owid , location=="United Kingdom" & date>="2020-02-26" & date <= "2020-04-26")

# Create new variable that groups pre-lockdown and post-lockdown 
UK$lockdown <- c(if_else(UK$date < "2020-03-26", 1, 2))

# Convert the data column from "character' to 'Date'  variable type 
class(UK$date)
UK$date <- as.Date(UK$date)
class(UK$date)

# Plot the graph             
ggplot(UK, aes(x=date, y=new_cases, color = as.factor(lockdown))) +
  geom_point() +
  geom_smooth()+
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%Y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(label=comma)+
  ylab('Daily number of confirmed cases') + xlab('Date')+
  geom_vline(xintercept = as.numeric(as.Date("2020-03-26")), linetype=2, colour = " grey", lwd = 0.5) +
  annotate("text", x=as.Date("2020-03-26"), y=4500, label= "First Lockdown (26 Mar)", angle=90, vjust = -0.5, colour = "grey", fontface = 'bold')+
  ggtitle("UK confirmed cases rate based on Loess regression (Lockdown 1)") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="top") +
  scale_color_manual(name="", labels = c("Pre-Lockdown (26 Feb - 25 Mar)", "Post-Lockdown (26 Mar - 26 Apr)"), values = c("1"="indianred1", "2"="darkturquoise"))

# Save Plot
ggsave("SCATTER PLOT Lockdown (30 day).png",width = 25, limitsize = FALSE, path = Time_Series_Graphs.dir)  


### 60 days before and after lockdown 1 ### 

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim("owid-covid-data.txt")

# Select rows corresponding to UK
UK <- filter(owid , location=="United Kingdom" & date>="2020-01-26" & date <= "2020-05-26")

# Create new variable that groups pre-lockdown and post-lockdown 
UK$lockdown <- c(if_else(UK$date < "2020-03-26", 1, 2))

# Convert the data column from "character' to 'Date'  variable type 
class(UK$date)
UK$date <- as.Date(UK$date)
class(UK$date)

# Plot the graph             
ggplot(UK, aes(x=date, y=new_cases, color = as.factor(lockdown))) +
  geom_point() +
  geom_smooth()+
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%Y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(label=comma)+
  ylab('Daily number of confirmed cases') + xlab('Date')+
  geom_vline(xintercept = as.numeric(as.Date("2020-03-26")), linetype=2, colour = " grey", lwd = 0.5) +
  annotate("text", x=as.Date("2020-03-26"), y=4500, label= "First Lockdown (26 Mar)", angle=90, vjust = -0.5, colour = "grey", fontface = 'bold')+
  ggtitle("UK confirmed cases rate based on Loess regression (Lockdown 1)") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="top") +
  scale_color_manual(name="", labels = c("Pre-Lockdown (26 Jan - 25 Mar)", "Post-Lockdown (26 Mar - 26 May)"), values = c("1"="indianred1", "2"="darkturquoise"))

# Save Plot
ggsave("SCATTER PLOT Lockdown (60 day).png",width = 25, limitsize = FALSE, path = Time_Series_Graphs.dir)  





# Straight Line 
geom_smooth(method=lm, se=FALSE) 
