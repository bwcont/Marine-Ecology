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
####Air Solitary Survival Split by Temp...####
#18C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "18"],  
  time2 = TIMESURV[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "18"], 
  event = dead[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "18"])
surv.fit.air.sol.18 <- survfit(foosurv ~ 1)
#32C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "32"],  
  time2 = TIMESURV[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "32"], 
  event = dead[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "32"])
surv.fit.air.sol.32<- survfit(foosurv ~ 1)
#36C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "36"],  
  time2 = TIMESURV[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "36"], 
  event = dead[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "36"])
surv.fit.air.sol.36 <- survfit(foosurv ~ 1)
#40C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "40"],  
  time2 = TIMESURV[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "40"], 
  event = dead[LT50Data$air_water == "Air" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "40"]
)
surv.fit.air.sol.40 <- survfit(foosurv ~ 1)

####Air TP Survival Split by Temp...####
#18C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "18"],  
  time2 = TIMESURV[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "18"], 
  event = dead[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "18"])
surv.fit.air.tp.18 <- survfit(foosurv ~ 1)
#32C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "32"],  
  time2 = TIMESURV[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "32"], 
  event = dead[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "32"])
surv.fit.air.tp.32<- survfit(foosurv ~ 1)
#36C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "36"],  
  time2 = TIMESURV[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "36"], 
  event = dead[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "36"])
surv.fit.air.tp.36 <- survfit(foosurv ~ 1)
#40C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "40"],  
  time2 = TIMESURV[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "40"], 
  event = dead[LT50Data$air_water == "Air" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "40"]
)
surv.fit.air.tp.40 <- survfit(foosurv ~ 1)

####Water Solitary Survival Split by Temp...####
#18C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "18"],  
  time2 = TIMESURV[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "18"], 
  event = dead[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "18"])
surv.fit.water.sol.18 <- survfit(foosurv ~ 1)
#32C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "32"],  
  time2 = TIMESURV[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "32"], 
  event = dead[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "32"])
surv.fit.water.sol.32<- survfit(foosurv ~ 1)
#36C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "36"],  
  time2 = TIMESURV[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "36"], 
  event = dead[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "36"])
surv.fit.water.sol.36 <- survfit(foosurv ~ 1)
#40C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "40"],  
  time2 = TIMESURV[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "40"], 
  event = dead[LT50Data$air_water == "Water" & LT50Data$uhab == "Solitary" & LT50Data$tmt_temp == "40"]
)
surv.fit.water.sol.40 <- survfit(foosurv ~ 1)

####WAter TP Survival Split by Temp...####
#18C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "18"],  
  time2 = TIMESURV[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "18"], 
  event = dead[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "18"])
surv.fit.water.tp.18 <- survfit(foosurv ~ 1)
#32C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "32"],  
  time2 = TIMESURV[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "32"], 
  event = dead[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "32"])
surv.fit.water.tp.32<- survfit(foosurv ~ 1)
#36C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "36"],  
  time2 = TIMESURV[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "36"], 
  event = dead[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "36"])
surv.fit.water.tp.36 <- survfit(foosurv ~ 1)
#40C Line
foosurv <- Surv(
  time = TIMESTART[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "40"],  
  time2 = TIMESURV[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "40"], 
  event = dead[LT50Data$air_water == "Water" & LT50Data$uhab == "TP" & LT50Data$tmt_temp == "40"]
)
surv.fit.water.tp.40 <- survfit(foosurv ~ 1)





###Plot Color Set-Up...####
#Get your color palettes
ColorAirSol <- c('#F6C7C7','#FD94B4','#FF467E','#F12B6B')
ColorAirTP <- c('#F6EFB4','#35C2BD','#2796CB','#3379E4')
ColorWaterSol <- c('#F4EEC0','#AED09E','#61B292','#7E6752')
ColorWaterTP <- c('#FFF5A5','#FFAA64','#FF8264','#FF6464')
#Alternative color:
wes.colors <- wes.palette('Cavalcanti', 4)
BlueColorGradient <- c('#D9FAFF','#00BBF0','#005792','#00204A') #Light to Dark

###Make Plot General/Example...####
plot(0,type='n', #make empty plot 
     xlim=c(0,21) # x limits
     , ylim=c(0,1) # y limits 
     , xlab= 'Time (days)', # name x azis
     ylab = 'Survival Function', # name y axis
     main = 'Air Survival', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.5) # make empty plot to fill in 

points(surv.fit.air.sol.18$time, surv.fit.air.sol.18$surv, type = 's', col = wes.colors[1], lwd = 2)
points(surv.fit.air.sol.32$time, surv.fit.air.sol.32$surv, type = 's', col = wes.colors[2], lwd = 2)
points(surv.fit.air.sol.36$time, surv.fit.air.sol.36$surv, type = 's', col = wes.colors[3], lwd = 2)
points(surv.fit.air.sol.40$time, surv.fit.air.sol.40$surv, type = 's', col = wes.colors[4], lwd = 2)


# x-axis labels
axis(2, cex.axis=1.2, tick = FALSE)
box()

###Specific Plot(empty at the moment)...#####



######
####Air Solitary Survival Split by Temp...####
####Air TP Survival Split by Temp...####
####Water Solitary Survival Split by Temp...####
####WAter TP Survival Split by Temp...####



###############
#######
######
#####
####
###
##
#

























