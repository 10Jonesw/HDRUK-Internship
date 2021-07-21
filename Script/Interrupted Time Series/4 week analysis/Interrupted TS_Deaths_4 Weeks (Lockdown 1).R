### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      20-07-2021

### DESCRIPTION ################################################################################################################
# Poisson regression models 
# Interrupted time series for the number of deaths during Lockdown 1 

### LIBRARIES ##################################################################################################################

library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)

### SUBSET DIRECTORIES #########################################################################################################

SubsetDatasets.dir <- "~/Desktop/HDRUK-Internship-2021/Data/SubsetDatasets/x"
Graphs.dir <- "~/Desktop/HDRUK-Internship-2021/Data/Graphs"
Time_Series_Graphs.dir  <- "~/Desktop/HDRUK-Internship-2021/Data/Time Series"
Interrupred_Time_Series_Graphs.dir  <- "~/Desktop/HDRUK-Internship-2021/Data/Graphs/Interrupted Time Series"

### CODE #######################################################################################################################

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim("owid-covid-data.txt")

# Select rows corresponding to UK
UK <- filter(owid , location=="United Kingdom"& date>="2020-02-27" & date < "2020-07-02")

# Label each date to a corresponding week 
UK$Week <-data.frame(Week = rep(c(1:18), each = 7))

# Find the weekly average of cases  
UK <- UK %>%                                       
  group_by(Week) %>%                         
  summarise_at(vars(new_deaths), list(AverageDeaths = mean))  

# Remove Na rows
UK <- na.omit(UK) 

# Create new variable that groups pre-lockdown and post-lockdown 
UK$lockdown <- c(if_else(UK$Week < 4, 0, 1))
UK$Status <- ifelse(UK$lockdown == 1, "Post-lockdown", "Pre-lockdown")

# Convert the data column from "data.frame' to 'numerical'  variable type 
class(UK$Week)
UK$Week <-as.numeric(unlist(UK$Week))
class(UK$Week)

#Create a new variable named 'week' for the lockdown week (Week 4)
UK <- UK %>% mutate(Lockdownweek = UK$Week-4, change=ifelse(Week>=4, 1, 0))

#Create a new variable named 'weekpostchange' to define the weeks before and after lockdonwn                 
UK$weekpostchange <-UK$Lockdownweek * UK$change

# Convert the data column from "data.frame' to 'numerical'  variable type 
class(UK$weekpostchange)
UK$weekpostchange <-as.numeric(unlist(UK$weekpostchange))
class(UK$weekpostchange)

# Add the date of the week commencing each week 
UKdates <- filter(owid , location=="United Kingdom"& date>="2020-02-27" & date < "2020-07-02")
UKdates <- UKdates %>% filter(row_number() %% 7 == 1)
UKdates <- UKdates %>% select(4)
UKdates <- filter(UKdates , date>="2020-03-12")
UK <- cbind(UK, UKdates)

# Convert the data column from "character' to 'Date'  variable type 
class(UK$date)
UK$date <- as.Date(UK$date)
class(UK$date)

#Baseline model for 2020 only
model0 <- glm(AverageDeaths ~ Week + change + weekpostchange, family="poisson", data=UK)
summary(model0) 

#Get RRs and 95%CIs
exp(coef(model0))
exp(confint(model0))

# Present the predictors of the model and show how the model fits the data
UK <- UK %>% mutate(prediction_model2=0)                        
UK$prediction_model2=predict(model0,type="response")

# Plot the interrupted time series 
ggplot(UK, aes(x=date)) +
  geom_line(aes(y=prediction_model2)) +
  geom_point(aes(y=AverageDeaths))+
  scale_y_log10() +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%Y", limits = as.Date(c('2020-02-27','2020-07-02'))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab('COVID Deaths per week') + xlab('Date')+
  geom_vline(xintercept = as.numeric(as.Date("2020-03-26")), linetype=2, colour = " red", lwd = 0.5) +
  annotate("text", x=as.Date("2020-03-26"), y=100, label= "First Lockdown Begins (26 Mar)", angle=90, vjust = -0.5, colour = "red", fontface = 'bold')+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), linetype=2, colour = " red", lwd = 0.5)+
  annotate("text", x=as.Date("2020-06-01"), y=100, label= "First Lockdown Eases (1 Jun)", angle=90, vjust = -0.5, colour = "red", fontface = 'bold')+
  ggtitle("Weekly count of COVID Deaths in the UK", subtitle = "Plot for 4 weeks before and after lockdown") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 

# Save Plot
ggsave("INTERRUPTED TIME SERIES PLOT_Deaths_4 weeks (Lockdown 1).png",width = 15, limitsize = FALSE, path = Interrupred_Time_Series_Graphs.dir)

