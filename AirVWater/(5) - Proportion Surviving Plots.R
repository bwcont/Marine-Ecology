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
setwd("C://Users/Sorte/Desktop/AirVWater")
LT50Data <- read.csv("DataExtrapolate.csv")
attach(LT50Data)

#Make a row for the start time. In this case I made the start time at 0 which is the start of the temp treatments
LT50Data$TIMESTART <- 0.001
#Check the data looks ok. 
#Note: The  survival assessment times were converted to days
head(LT50Data)
tail(LT50Data)
View(LT50Data)

LT50Data$name2<-paste(LT50Data$recovery, LT50Data$air_water, LT50Data$uhab, LT50Data$tmt_temp)
LT50Data$name2

summary<-ddply(LT50Data, c("name2"), summarize,
               num.live = sum(alive), # this is saying to take the mean range and ignore missing data if there is any
               num.total = sum(!is.na(alive)))
summary

summary$prop.surv<-(summary$num.live/summary$num.total)
write.csv(summary, 'summary.data.csv')
summary2<-read.csv('summary.data2.csv')

head(summary2)

###Plot Color Set-Up...####
#Get your color palettes
ColorAirSol <- c('#80ff80','#00e600','#00b300','#008000') #Green levels (75,45,35,25)
ColorAirTP <- c('#f7b6a1','#ee5b2b','#bd3b0f','#76250a')  #Red-Orange
ColorWaterSol <- c('#e8f3fd','#72b9f3','#158aea','#0e61a4') #Blue
ColorWaterTP <- c('#ffe6ff','#ff80ff','#ff00ff','#800080')  #Purple
#Alternative color:
library(wesanderson)
wes.colors <- wes_palette('Cavalcanti', 4)
BlueColorGradient <- c('#D9FAFF','#00BBF0','#005792','#00204A') #Light to Dark

###Make Plot General/Example...####
plot(0,type='n', #make empty plot 
     xlim=c(0,21) # x limits
     , ylim=c(0,1) # y limits 
     , xlab= 'Time (days)', # name x azis
     ylab = 'Proportion Survival', # name y axis
     main = 'Solitary Air Survival', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.5) # make empty plot to fill in 

# x-axis labels
axis(2, cex.axis=1.2, tick = FALSE)
box()
# y-axis labels
axis(1, cex.axis=1.2, tick = FALSE)
box()

###Create unplotted Lines of surv...####

AirSol18 <- points(summary2$time[1:5], summary2$prop.surv[1:5]+0.01, type = 's', col = ColorAirSol[1], lwd = 3)
AirSol32 <- points(summary2$time[6:10]+0.2, summary2$prop.surv[6:10], type = 's', col = ColorAirSol[2], lwd = 3)
AirSol36 <- points(summary2$time[11:15], summary2$prop.surv[11:15]+0.01, type = 's', col = ColorAirSol[3], lwd = 3)
AirSol40 <- points(summary2$time[16:20]+0.2, summary2$prop.surv[16:20], type = 's', col = ColorAirSol[4], lwd = 3)

#
##Incomplete from here on down
#
AirTP18 <- points(summary2$time[21:25], summary2$prop.surv[21:25]+0.01, type = 's', col = ColorAirSol[1], lwd = 3)
AirTP32 <- points(summary2$time[26:30]+0.2, summary2$prop.surv[26:30], type = 's', col = ColorAirSol[2], lwd = 3)
AirTP36 <- points(summary2$time[31:35], summary2$prop.surv[31:35]+0.01, type = 's', col = ColorAirSol[3], lwd = 3)
AirTP40 <- points(summary2$time[36:40]+0.2, summary2$prop.surv[36:40], type = 's', col = ColorAirSol[4], lwd = 3)

WaterSol18 <- points(summary2$time[41:45], summary2$prop.surv[41:45]+0.01, type = 's', col = ColorAirSol[1], lwd = 3)
WaterSol32 <- points(summary2$time[46:50]+0.2, summary2$prop.surv[46:50], type = 's', col = ColorAirSol[2], lwd = 3)
WaterSol36 <- points(summary2$time[51:55], summary2$prop.surv[51:55]+0.01, type = 's', col = ColorAirSol[3], lwd = 3)
WaterSol40 <- points(summary2$time[56:60]+0.2, summary2$prop.surv[56:60], type = 's', col = ColorAirSol[4], lwd = 3)

WaterTP18 <- points(summary2$time[61:65], summary2$prop.surv[61:65]+0.01, type = 's', col = ColorAirSol[1], lwd = 3)
WaterTP32 <- points(summary2$time[66:70]+0.2, summary2$prop.surv[66:70], type = 's', col = ColorAirSol[2], lwd = 3)
WaterTP36 <- points(summary2$time[71:75], summary2$prop.surv[71:75]+0.01, type = 's', col = ColorAirSol[3], lwd = 3)
WaterTP40 <- points(summary2$time[76:80]+0.2, summary2$prop.surv[76:80], type = 's', col = ColorAirSol[4], lwd = 3)

#2####







