#~R Code~#
##############################################################
########### Title: LT50 Comparative Analysis
########### By: L. Pandori & E. Cruz 
########### Based on: N. Silbinger and L. Miller    
########### Created: 3/12/18          
########### Edited: 3/12/18          
##############################################################

###Setup..####
# clear list
rm(list=ls())

#load all packages
library(plyr)
library(MASS)
library(dplyr)
library(KMsurv)
library(survival)
library(OIsurv)

#Pull in data
setwd("C:/SaveHere/EC")
LT50Data <- read.csv("DataExtrapolate.csv")
attach(LT50Data)

#Check the data looks ok
head(LT50Data)
tail(LT50Data)
View(LT50Data)

###Subsets..####
#Air
#Sol
ASBA<-subset(LT50Data, LT50Data$name == 'Air.Solitary.biweek.A')
ASBB<-subset(LT50Data, LT50Data$name == 'Air.Solitary.biweek.B')
ASBC<-subset(LT50Data, LT50Data$name == 'Air.Solitary.biweek.C')
ASDA<-subset(LT50Data, LT50Data$name == 'Air.Solitary.day.A')
ASDB<-subset(LT50Data, LT50Data$name == 'Air.Solitary.day.B')
ASDC<-subset(LT50Data, LT50Data$name == 'Air.Solitary.day.C')
ASWA<-subset(LT50Data, LT50Data$name == 'Air.Solitary.week.A')
ASWB<-subset(LT50Data, LT50Data$name == 'Air.Solitary.week.B')
ASWC<-subset(LT50Data, LT50Data$name == 'Air.Solitary.week.C')
ASTA<-subset(LT50Data, LT50Data$name == 'Air.Solitary.triweek.A')
ASTB<-subset(LT50Data, LT50Data$name == 'Air.Solitary.triweek.B')
ASTC<-subset(LT50Data, LT50Data$name == 'Air.Solitary.triweek.C')
#TP
ATBA<-subset(LT50Data, LT50Data$name == 'Air.TP.biweek.A')
ATBB<-subset(LT50Data, LT50Data$name == 'Air.TP.biweek.B')
ATBC<-subset(LT50Data, LT50Data$name == 'Air.TP.biweek.C')
ATDA<-subset(LT50Data, LT50Data$name == 'Air.TP.day.A')
ATDB<-subset(LT50Data, LT50Data$name == 'Air.TP.day.B')
ATDC<-subset(LT50Data, LT50Data$name == 'Air.TP.day.C')
ATWA<-subset(LT50Data, LT50Data$name == 'Air.TP.week.A')
ATWB<-subset(LT50Data, LT50Data$name == 'Air.TP.week.B')
ATWC<-subset(LT50Data, LT50Data$name == 'Air.TP.week.C')
ATTA<-subset(LT50Data, LT50Data$name == 'Air.TP.triweek.A')
ATTB<-subset(LT50Data, LT50Data$name == 'Air.TP.triweek.B')
ATTC<-subset(LT50Data, LT50Data$name == 'Air.TP.triweek.C')

#Water
#Sol
WSBA<-subset(LT50Data, LT50Data$name == 'Water.Solitary.biweek.A')
WSBB<-subset(LT50Data, LT50Data$name == 'Water.Solitary.biweek.B')
WSBC<-subset(LT50Data, LT50Data$name == 'Water.Solitary.biweek.C')
WSDA<-subset(LT50Data, LT50Data$name == 'Water.Solitary.day.A')
WSDB<-subset(LT50Data, LT50Data$name == 'Water.Solitary.day.B')
WSDC<-subset(LT50Data, LT50Data$name == 'Water.Solitary.day.C')
WSWA<-subset(LT50Data, LT50Data$name == 'Water.Solitary.week.A')
WSWB<-subset(LT50Data, LT50Data$name == 'Water.Solitary.week.B')
WSWC<-subset(LT50Data, LT50Data$name == 'Water.Solitary.week.C')
WSTA<-subset(LT50Data, LT50Data$name == 'Water.Solitary.triweek.A')
WSTB<-subset(LT50Data, LT50Data$name == 'Water.Solitary.triweek.B')
WSTC<-subset(LT50Data, LT50Data$name == 'Water.Solitary.triweek.C')
#TP
WTBA<-subset(LT50Data, LT50Data$name == 'Water.TP.biweek.A')
WTBB<-subset(LT50Data, LT50Data$name == 'Water.TP.biweek.B')
WTBC<-subset(LT50Data, LT50Data$name == 'Water.TP.biweek.C')
WTDA<-subset(LT50Data, LT50Data$name == 'Water.TP.day.A')
WTDB<-subset(LT50Data, LT50Data$name == 'Water.TP.day.B')
WTDC<-subset(LT50Data, LT50Data$name == 'Water.TP.day.C')
WTWA<-subset(LT50Data, LT50Data$name == 'Water.TP.week.A')
WTWB<-subset(LT50Data, LT50Data$name == 'Water.TP.week.B')
WTWC<-subset(LT50Data, LT50Data$name == 'Water.TP.week.C')
WTTA<-subset(LT50Data, LT50Data$name == 'Water.TP.triweek.A')
WTTB<-subset(LT50Data, LT50Data$name == 'Water.TP.triweek.B')
WTTC<-subset(LT50Data, LT50Data$name == 'Water.TP.triweek.C')

###Loop for LT50s...####
small<-list(ASBA, ASBB, ASBC, ASDA, ASDB, ASDC, ASWA, ASWB, ASWC, ASTA, ASTB, ASTC, ATBA, ATBB, ATBC, ATDA, ATDB, ATDC, ATWA, ATWB, ATWC, ATTA, ATTB, ATTC, WSBA, WSBB, WSBC, WSDA, WSDB, WSDC, WSWA, WSWB, WSWC, WSTA, WSTB, WSTC, WTBA, WTBB, WTBC, WTDA, WTDB, WTDC, WTWA, WTWB, WTWC, WTTA, WTTB, WTTC)
unique(small)

# make results matrix
results<-matrix(nrow = 48, ncol = 8)
results<-as.data.frame(results)
colnames(results)<-c('name', 'LT50', 'se', 'n', 'gen.name', 'clump.name', 'uhab', 'air.water', 'recov' )
rownames(results)<-c(1:48)
results

#Loop for LT50
for(i in 1:48) {
  temp <- small[i] 
  temp<-as.data.frame(temp)
  temp.y <- cbind(temp$alive, temp$dead)
  temp.model <- glm(temp.y ~ temp$tmt_temp , binomial)
  results[i,2] <- dose.p(temp.model)
  se<-attr(dose.p(temp.model), 'SE')
  results[i,3]<-matrix(se)
  results[i,4]<-length(temp$dead) }   

View(results)

#Give Names to columns
results[,1]<-c('ASBA', 'ASBB', 'ASBC', "ASDA", 'ASDB', 'ASDC', 'ASWA', 'ASWB', 'ASWC', 'ASTA', 'ASTB', 'ASTC', 'ATBA', 'ATBB', 'ATBC', 'ATDA', 'ATDB', 'ATDC', 'ATWA', 'ATWB', 'ATWC', 'ATTA', 'ATTB', 'ATTC', 'WSBA', 'WSBB', 'WSBC', 'WSDA', 'WSDB', 'WSDC', 'WSWA', 'WSWB', 'WSWC', 'WSTA', 'WSTB', 'WSTC', 'WTBA', 'WTBB', 'WTBC', 'WTDA', 'WTDB', 'WTDC', 'WTWA', 'WTWB', 'WTWC', 'WTTA', 'WTTB', 'WTTC')
results[,5]<-c('ASB', 'ASB', 'ASB', "ASD", 'ASD', 'ASD', 'ASW', 'ASW', 'ASW', 'AST', 'AST', 'AST', 'ATB', 'ATB', 'ATB', 'ATD', 'ATD', 'ATD', 'ATW', 'ATW', 'ATW', 'ATT', 'ATT', 'ATT', 'WSB', 'WSB', 'WSB', 'WSD', 'WSD', 'WSD', 'WSW', 'WSW', 'WSW', 'WST', 'WST', 'WST', 'WTB', 'WTB', 'WTB', 'WTD', 'WTD', 'WTD', 'WTW', 'WTW', 'WTW', 'WTT', 'WTT', 'WTT')
results[,6]<-c('Air Solitary', 'Air Solitary', 'Air Solitary', "Air Solitary", 'Air Solitary', 'Air Solitary', 'Air Solitary', 'Air Solitary', 'Air Solitary', 'Air Solitary', 'Air Solitary', 'Air Solitary', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Air Tidepool', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Solitary', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool', 'Water Tidepool')

###GLM for our different factors affecting the survival...####

AllFactorGLM <- glm(alive ~ tmt_temp + air_water + LT50Data$uhab + LT50Data$recovery)
summary(AllFactorGLM)

###GLM for our different factors affecting the LT50s...####

AllLT50GLM <- glm(results$LT50 ~ results$clump.name)
summary(AllLT50GLM)

###Mann-Witney-Wilcox tests Submerge and uHab...####

wilcox.test(LT50Data$alive[LT50Data$air_water == 'Air'], LT50Data$alive[LT50Data$air_water == 'Water'])
wilcox.test(LT50Data$alive[LT50Data$uhab == 'TP'], LT50Data$alive[LT50Data$uhab == 'Solitary'])

#############
######
#####
####
###
##
#


