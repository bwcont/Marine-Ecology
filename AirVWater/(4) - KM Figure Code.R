#~R Code~#
##############################################################
########### Title: KM Figure Code
########### By: L. Pandori & E. Cruz 
########### Based on: N. Silbinger and L. Miller    
########### Created: 3/12/18          
########### Edited: 3/13/18          
##############################################################

###Setup..####
# clear list
rm(list=ls())

#load all packages
library(plyr)
library(dplyr)
library(KMsurv)
library(survival)
library(OIsurv)
library(wesanderson)

#Pull in data
setwd("C:/SaveHere/EC")
LT50Data <- read.csv("DataExtrapolate.csv")
attach(LT50Data)

#Make a row for the start time. In this case I made the start time at 0 which is the start of the temp treatments
LT50Data$TIMESTART <- 0.001
#Check the data looks ok. 
#Note: The  survival assessment times were converted to days
head(LT50Data)
tail(LT50Data)
attach(LT50Data)

###Clumped Water Survival...####
foosurv <- Surv(
  #time = Start of the time interval Note: our mussels could've started at different times. We controlled for this by measuring our mussels on Monday or Tuesday so we should be good on this
  time = TIMESTART[LT50Data$air_water == "Water"], 
  #time 2 = The time we assessed, aka the end of our interval
  time2 = TIMESURV[LT50Data$air_water == "Water"], 
  #event = whether the death happened at time 2 (at time of assessment) We luckily had a "dead" coloumn already so this was covered.
  event = dead[LT50Data$air_water == "Water"]
)

surv.fit.water.clump <- survfit(foosurv ~ 1 #not sure why we do it as a function of "1" but it works?
)

####Clumped Water Survival...####
foosurv <- Surv(
   time = TIMESTART[LT50Data$air_water == "Air"], 
  time2 = TIMESURV[LT50Data$air_water == "Air"], 
  event = dead[LT50Data$air_water == "Air"]
)
surv.fit.air.clump <- survfit(foosurv ~ 1)

plot(surv.fit.water.clump, col='blue')
###Plot Color Set-Up...####
#Get your color palettes
ColorAirSol <- c('','','','')
ColorAirSol <- c('','','','')
ColorAirSol <- c('','','','')
ColorAirSol <- c('','','','')
#Alternative color:
wes.colors<-wes_palette('Cavalcanti', 4)


plot(0,type='n', #make empty plot 
     xlim=c(0,21) # x limits
     , ylim=c(0,1) # y limits 
     , xlab= 'Time (days)', # name x azis
     ylab = 'Survival Function', # name y axis
     main = 'Air Survival', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.5) # make empty plot to fill in 

points(surv.fit.air.clump$time, surv.fit.air.clump$surv, type = 's', col = wes.colors[1], lwd = 2)
points(surv.fit.water.clump$time,surv.fit.water.clump$surv, type = 's', col = wes.colors[2], lwd = 2)

# x-axis labels
axis(1, at=c(1,2,3,4), labels= data.summary2$gen.name, cex.axis=1.2, # text size
     las = 1, tick = FALSE) 
# add in points
points(c(1,2,3,4), data.summary2$mean.lt50, cex = 2, pch = 16, col = 'black')
# 
axis(2, cex.axis=1.2, tick = FALSE)
box()