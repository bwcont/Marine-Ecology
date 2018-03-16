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

summary

write.csv(summary, 'summary.data.csv')

summary2<-read.csv('summary.data2.csv')

View(summary2)

head(summary2)


###Plot Color Set-Up...####
#Get your color palettes
ColorAirSol <- c('#80ff80','#00e600','#00b300','#008000') #Green levels (75,45,35,25)
ColorAirTP <- c('#f7b6a1','#ee5b2b','#bd3b0f','#76250a')  #Red-Orange
ColorWaterSol <- c('#e8f3fd','#72b9f3','#158aea','#0e61a4') #Blue
ColorWaterTP <- c('#ffe6ff','#ff80ff','#ff00ff','#800080')  #Purple
#Alternative color:
library(wesanderson)
wes.colors <- wes.palette('Cavalcanti', 4)
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

points(c(0,summary2$time[1:4]), c(1,summary2$prop.surv[1:4]), type = 's', col = BlueColorGradient[1], lwd = 2))
points(summary2$time[5:8], summary2$prop.surv[5:8], type = 's', col = BlueColorGradient[2], lwd = 2)
points(summary2$time[9:12], summary2$prop.surv[9:12]+0.1, type = 's', col = BlueColorGradient[3], lwd = 2)
points(summary2$time[13:16]+0.2, summary2$prop.surv[13:16], type = 's', col = BlueColorGradient[4], lwd = 2)

# x-axis labels
axis(2, cex.axis=1.2, tick = FALSE)
box()
# y-axis labels
axis(1, cex.axis=1.2, tick = FALSE)
box()








