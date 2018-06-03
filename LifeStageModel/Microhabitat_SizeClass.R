################################################# 
# Title: Microhabitat & Size Class for M. californianus
# Purpose: Survey Model comparison
# Created by : E. Cruz & L. Pandori
# Created on : 5/21/2018
# Last Edited : 6/1/2018
#################################################
# Set WD
setwd()
# clear workspace
rm(list=ls())

#Set DAta
data.muss <- read.csv("Model_Format_Summary.csv")

library(plyr)


data.muss$TideH <- revalue(data.muss$TideH, c("L"= '1', "M"='2', "H"='3'))
data.muss$Site <- revalue(data.muss$Site, c("CCSP" = '1', "LCDM" = '2'))
data.muss$Microhabitat <- revalue(data.muss$Microhabitat, c("UnShelt_Solitary" = '1', "UnShelt_Aggregate" = '2', "Sheltered_Solitary" = '3', "Sheltered_Aggregate" = '4', "Tidepool" = '5'))
data.muss$Season <- revalue(data.muss$Season, c("Spring" = '1', "Fall" = '3', "Summer" = '2'))


#Model for Density
Density.Model <- glm(data.muss$Density ~ data.muss$SizeClass, data = data.muss)

summary(Density.Model)

#Model for Filling


#Model for Association

Col.MicroHab <- c('#FF0000','#FF8000', '#00CC00', '#66B2FF', '#B266FF')
#####Graphs#####

###Fig. 2: Average Association of Micro per site####
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
       col = Col.MicroHab)

points(x = c(1,2,3), y = c(4.9, 7.5, 0.91), cex = 1.5, pch = 16 )

axis(1, c(1,2,3), c("Low", "Mid", "High"), cex=2, pch=16, col = "black")
axis(2)

box()
