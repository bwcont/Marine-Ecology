################################################# 
# Title: Microhabitat & Size Class for M. californianus
# Purpose: Survey Model comparison
# Created by : E. Cruz & L. Pandori
# Created on : 5/21/2018
# Last Edited : 5/22/2018
#################################################
# clear workspace
rm(list=ls())
# Set WD
setwd()
#Set DAta
data <- read.csv("Model_Format_Summary_Temp.csv")
attach(data)

data$TideHeight2 <- revalue(data$TideHeight2,
               c("L"="1", "M"="2", "H"="3"))

data$TideHeight2 <- as.numeric(TideHeight2)
#convert our site and TH into a numeric discrete
data$Site2 <- as.numeric(data$Site)

#TH Graph
#Means
lowmean <- mean(data$TotalMussels[data$TideH == "L"], na.rm = TRUE)
midmean <- mean(data$TotalMussels[data$TideH == "M"], na.rm = TRUE)
highmean <- mean(data$TotalMussels[data$TideH == "H"], na.rm = TRUE)
#Standard Deviation
midsd <- sd(data$TotalMussels[data$TideH == "M"], na.rm = TRUE)
highsd <- sd(data$TotalMussels[data$TideH == "H"], na.rm = TRUE)
lowsd <- sd(data$TotalMussels[data$TideH == "L"], na.rm = TRUE)

plot(0,type='n', #make empty plot 
     xlim=c(0.75,3.25) # x limits
     , ylim=c(0, 27) # y limits 
     , xlab= 'Tide Height', # name x azis
     ylab = 'Average Total Mussels', # name y axis
     main = ' ', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

arrows(c(1,2,3), c(4.9 + 10.78, 7.5 + 18.58, 0.9 + 2.102), 
       c(1,2,3), c(4.9 - 10.78, 7.5 - 18.58, 0.9 - 2.102),
      angle = 90, code = 3, length = 0, lty = 1, lwd = 5, # jelly bean width
       col = c("blue", "darkviolet", "red"))

points(x = c(1,2,3), y = c(4.9, 7.5, 0.91), cex = 1.5, pch = 16 )

axis(1, c(1,2,3), c("Low", "Mid", "High"), cex=2, pch=16, col = "black")
axis(2)

box()

#Model for Total Mussels
#Note: the subset designated within the brackets is due to the data only being inputted for 2 sites thus far.
#Shelter and Microhabitat
TotalMussModel2<-glm(TotalMussels ~ Microhabitat + TideHeight2 + Site +SizeClass, data = data)
summary(TotalMussModel2) 

#Model for Density


#Model for Filling


#Model for Association

