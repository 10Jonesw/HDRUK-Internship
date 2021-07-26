### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      26-07-2021

### DESCRIPTION ################################################################################################################
# UPDATED NEW SCRIPT
# Poisson regression models 
# Interrupted time series for the number of cases and deaths during Lockdown 1 

### LIBRARIES ##################################################################################################################

library(tidyverse)
library(ggplot2)
library(dplyr)

### SUBSET DIRECTORIES #########################################################################################################

SubsetDatasets.dir <- "~/Desktop/HDRUK-Internship-2021/Data/SubsetDatasets/x"
Graphs.dir <- "~/Desktop/HDRUK-Internship-2021/Data/Graphs"
Time_Series_Graphs.dir  <- "~/Desktop/HDRUK-Internship-2021/Data/Time Series"
Interrupred_Time_Series_Graphs.dir  <- "~/Desktop/HDRUK-Internship-2021/Data/UPDATED Interrupted Time Series"

### CODE #######################################################################################################################

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')

# Select rows corresponding to UK
UK <- filter(owid , location=="United Kingdom"& date>="2020-11-12" & date <= "2020-12-24")

UK$Dayno <-data.frame(Dayno = rep(c(1:43)))

# Create new variable that groups pre-lockdown and post-lockdown 
UK$lockdown <- c(if_else(UK$date <"2020-12-03", 0, 1))
UK$Status <- ifelse(UK$lockdown == 1, "Post-lockdown", "Pre-lockdown")

# Convert the data column from "data.frame' to 'numerical'  variable type 
class(UK$Dayno)
UK$Dayno <-as.numeric(unlist(UK$Dayno))
class(UK$Dayno)

#Create a new variable named 'Lockdownday' for the lockdown week (Week 8)
UK <- UK %>% mutate(Lockdownday = UK$Dayno-22, change=ifelse(Dayno>=22, 1, 0))

#Create a new variable named 'daypostchange' to define the weeks before and after lockdonwn                 
UK$daypostchange <-UK$Lockdownday * UK$change

# Convert the data column from "character' to 'Date'  variable type 
UK$date <- as.Date(UK$date)

#Baseline model with squared terms: Cases
model0 <- glm(new_cases ~ Dayno + I(Dayno^2) + change + daypostchange + I(daypostchange^2), family="poisson", data=UK)
summary(model0)

#Baseline model with squared terms: Deaths
model1 <- glm(new_deaths ~ Dayno + I(Dayno^2) + change + daypostchange + I(daypostchange^2), family="poisson", data=UK)
summary(model1)


#Get RRs and 95%CIs
exp(coef(model0))
exp(confint(model0))

# Present the predictors of the model and show how the model fits the data: Cases
UK <- UK %>% mutate(prediction_model0 = predict(model0,type="response"))

# Present the predictors of the model and show how the model fits the data: Deaths
UK <- UK %>% mutate(prediction_model1 = predict(model1,type="response"))

colors <- c("COVID Cases" = "Seagreen3", "COVID Deaths" = "royalblue1")

# Plot the interrupted time series 
ggplot(UK, aes(x=date)) +
  geom_line(aes(y=prediction_model0, color = "COVID Cases"), size = 1) +
  geom_line(aes(y=prediction_model1, color = "COVID Deaths"), size = 1) +
  scale_y_log10() +
  geom_point(aes(y=new_cases), size =2, colour = "seagreen4")+
    geom_point(aes(y=new_deaths), size =2, colour = "royalblue4") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %Y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  ylab('Confirmed cases/deaths per week') + xlab('')+
  geom_vline(xintercept = as.numeric(as.Date("2020-12-03")), linetype=2, colour = " firebrick2", lwd = 0.5)+
  annotate("text", x=as.Date("2020-12-03"), y=600, label= "Second Lockdown Eases (3 Dec)", angle=90, vjust = 1.5, colour = "firebrick2", fontface = 'bold')+
  ggtitle("Weekly count of confirmed case and death numbers in the UK", subtitle = "(Lockdown 2 Eases)") +
  theme(plot.title = element_text(hjust = 0.5, size= 16), plot.subtitle = element_text(hjust = 0.5, size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=12),  axis.text.y = element_text(size=12), axis.title=element_text(size=16), legend.position = "top") +
  labs(color = "") +
  scale_color_manual(values = colors)


# Save Plot
ggsave("Cases&Deaths_Lockdown 2 Ends.png",width = 10, limitsize = FALSE, path = Interrupred_Time_Series_Graphs.dir)

