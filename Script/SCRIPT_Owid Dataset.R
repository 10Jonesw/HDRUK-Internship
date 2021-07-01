### AUTHORS ####################################################################################################################
# Name             Date
# Wendy Jones      01-07-2021

### DESCRIPTION ################################################################################################################
# Exploring the owid COVID dataset 

### LIBRARIES ##################################################################################################################

### CODE #######################################################################################################################

# Set Working Directory
setwd("~/Desktop/HDRUK-Internship-2021/Data")
getwd()

# Read in owid covid data 
owid<- read.delim(file.choose("owid-covid-data.txt"))

# Look at the number of unique countries listed in the database
CountryNames <- unique(owid[,3])
CountryNames
