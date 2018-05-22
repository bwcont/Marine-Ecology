################################################# 
# Title: Microhabitat & Size Class for M. californianus
# Purpose: Survey Model comparison
# Created by : E. Cruz
# Created on : 5/21/2018
# Last Edited : 5/21/2018
#################################################
# clear workspace
rm(lists=ls())
# Set WD
setwd()
#Set DAta
data <- read.csv("Model_Format_Summary.csv")
attach(data)

#TH Graph
#Means
lowmean <- mean(data$TotalMussels[data$TideH == "L"], na.rm = TRUE)
midmean <- mean(data$TotalMussels[data$TideH == "M"], na.rm = TRUE)
highmean <- mean(data$TotalMussels[data$TideH == "H"], na.rm = TRUE)
#Standard Deviation
midsd <- sd(data$TotalMussels[data$TideH == "M"], na.rm = TRUE)
highsd <- sd(data$TotalMussels[data$TideH == "H"], na.rm = TRUE)
lowsd <- sd(data$TotalMussels[data$TideH == "L"], na.rm = TRUE)

#Model for Total Mussels
#Note: the subset designated within the brackets is due to the data only being inputted for 2 sites thus far.

TotalMussModel <- lm(TotalMussels[1:405] ~ SizeClass[1:405] + Shelter[1:405] + Microhabitat[1:405] + TideH[1:405] + Site[1:405])
summary(TotalMussModel)

#Model for Density


#Model for Filling


#Model for Association

