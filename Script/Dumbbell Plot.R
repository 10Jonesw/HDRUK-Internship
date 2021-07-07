### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      07-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 
# Created a dumbbell plot to compare the percentage of the population vaccinated between May and June

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

# Filter owid data for end of month 
Data_2021 <- filter(owid, date=="2021-06-30" | date=="2021-05-31"|date=="2021-04-30" |date=="2021-03-31" |date=="2021-02-28" |date=="2021-01-31")

# Select for relevant columns 
Data_2021 <- Data_2021 %>% select(3, 4, 36, 45)

# Create a dataset of the countries with missing % vaccination data 
na_vaccination <- which(!complete.cases(Data_2021$people_vaccinated))

# Create a dataset with the remaining countries with present vaccination data 
vaccination_data <- Data_2021[-na_vaccination,]

# Calculate the % of the population vaccinated 
vaccination_data["Percentage_Vaccinated"] <- vaccination_data$people_vaccinated / vaccination_data$population

# Select for May and June data
May_June_Data <- filter(vaccination_data, date=="2021-06-30" | date=="2021-05-31")

# Select for countries that have May and June data                                        ##Note2Self: This is not an efficient method 
May_June_Data <- May_June_Data %>% slice(2,3,8:15, 17:22, 26:27, 29:34, 36:37, 39:40, 44:45, 48:51, 60:63, 65:66, 69:72, 75:78, 81:82, 85:88, 90:95, 97:100, 103:106, 109:114, 117:118, 122:123, 126:127, 129:132, 139:140, 143:144, 147:148, 152:155,157:158, 166:167, 169:174, 177:178, 185:186, 188:189,194:201)

# Create new variable that assigns group corresponding to each country 
May_June_Data <- May_June_Data %>%
  mutate(paired = rep(1:(n()/2),each=2),
         date=factor(date))

# Plot dumbbell chart
May_June_Data %>% 
  ggplot(aes(x= Percentage_Vaccinated, y= reorder(location,Percentage_Vaccinated))) +
  geom_line(aes(group = paired))+
  geom_point(aes(color=date), size=2) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(size=0.3, color="dark grey"),
        legend.position="top",
        axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
        panel.border=element_blank())+
  scale_x_continuous(breaks=seq(0, 1, 0.1), limits=c(0,0.7), labels = scales::percent_format(accuracy = 5L))+
  scale_color_manual(name="Month", labels = c("May", "June"), values = c("2021-05-31"="indianred1", "2021-06-30"="darkturquoise")) +
  xlab('Proportion of the population vaccinated') + ylab('Country') +
  ggtitle("Percentage of the population vaccinated between May and June") +
  theme(plot.title = element_text(hjust = 0.5))

# Save Plot
ggsave("DUMBBELL: Population vaccinated between May and June.png", width = 20, height = 10, limitsize = FALSE, path = Graphs.dir)                     
  