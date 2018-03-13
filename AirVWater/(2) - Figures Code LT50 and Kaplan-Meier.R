#~R Code~#
##############################################################
########### Title: LT50 and KM Figure Code
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
library(wesanderson)

#Pull in data
setwd("C:/SaveHere")
LT50Data <- read.csv("EC/DataExtrapolate.csv")
attach(LT50Data)

#Check the data looks ok
head(LT50Data)
tail(LT50Data)
View(LT50Data)

###Subsets..####
#Air
#Sol
ASBA <- subset(LT50Data, LT50Data$ID == 'Air.Solitary.Biweek.A')
ASBB<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Biweek.B')
ASBC<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Biweek.C')
ASDA<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Day.A')
ASDB<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Day.B')
ASDC<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Day.C')
ASWA<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Week.A')
ASWB<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Week.B')
ASWC<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Week.C')
ASTA<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Triweek.A')
ASTB<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Triweek.B')
ASTC<-subset(LT50Data, LT50Data$ID == 'Air.Solitary.Triweek.C')
#TP
ATBA<-subset(LT50Data, LT50Data$ID == 'Air.TP.Biweek.A')
ATBB<-subset(LT50Data, LT50Data$ID == 'Air.TP.Biweek.B')
ATBC<-subset(LT50Data, LT50Data$ID == 'Air.TP.Biweek.C')
ATDA<-subset(LT50Data, LT50Data$ID == 'Air.TP.Day.A')
ATDB<-subset(LT50Data, LT50Data$ID == 'Air.TP.Day.B')
ATDC<-subset(LT50Data, LT50Data$ID == 'Air.TP.Day.C')
ATWA<-subset(LT50Data, LT50Data$ID == 'Air.TP.Week.A')
ATWB<-subset(LT50Data, LT50Data$ID == 'Air.TP.Week.B')
ATWC<-subset(LT50Data, LT50Data$ID == 'Air.TP.Week.C')
ATTA<-subset(LT50Data, LT50Data$ID == 'Air.TP.Triweek.A')
ATTB<-subset(LT50Data, LT50Data$ID == 'Air.TP.Triweek.B')
ATTC<-subset(LT50Data, LT50Data$ID == 'Air.TP.Triweek.C')

#Water
#Sol
WSBA<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Biweek.A')
WSBB<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Biweek.B')
WSBC<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Biweek.C')
WSDA<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Day.A')
WSDB<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Day.B')
WSDC<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Day.C')
WSWA<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Week.A')
WSWB<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Week.B')
WSWC<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Week.C')
WSTA<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Triweek.A')
WSTB<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Triweek.B')
WSTC<-subset(LT50Data, LT50Data$ID == 'Water.Solitary.Triweek.C')
#TP
WTBA<-subset(LT50Data, LT50Data$ID == 'Water.TP.Biweek.A')
WTBB<-subset(LT50Data, LT50Data$ID == 'Water.TP.Biweek.B')
WTBC<-subset(LT50Data, LT50Data$ID == 'Water.TP.Biweek.C')
WTDA<-subset(LT50Data, LT50Data$ID == 'Water.TP.Day.A')
WTDB<-subset(LT50Data, LT50Data$ID == 'Water.TP.Day.B')
WTDC<-subset(LT50Data, LT50Data$ID == 'Water.TP.Day.C')
WTWA<-subset(LT50Data, LT50Data$ID == 'Water.TP.Week.A')
WTWB<-subset(LT50Data, LT50Data$ID == 'Water.TP.Week.B')
WTWC<-subset(LT50Data, LT50Data$ID == 'Water.TP.Week.C')
WTTA<-subset(LT50Data, LT50Data$ID == 'Water.TP.Triweek.A')
WTTB<-subset(LT50Data, LT50Data$ID == 'Water.TP.Triweek.B')
WTTC<-subset(LT50Data, LT50Data$ID == 'Water.TP.Triweek.C')

#####MAke Reults Matrix...####


# make results matrix
results<-matrix(nrow = 48, ncol = 5)
results<-as.data.frame(results)
colnames(results)<-c('name', 'LT50', 'se', 'n', 'gen.name' )
rownames(results)<-c(1:48)
results

#MAke list of subsets
small<-list(ASBA, ASBB, ASBC, ASDA, ASDB, ASDC, ASWA, ASWB, ASWC, ASTA, ASTB, ASTC, ATBA, ATBB, ATBC, ATDA, ATDB, ATDC, ATWA, ATWB, ATWC, ATTA, ATTB, ATTC, WSBA, WSBB, WSBC, WSDA, WSDB, WSDC, WSWA, WSWB, WSWC, WSTA, WSTB, WSTC, WTBA, WTBB, WTBC, WTDA, WTDB, WTDC, WTWA, WTWB, WTWC, WTTA, WTTB, WTTC)
unique(small)
#Loop for LT50
for(i in 1:48) {
  temp <- small[i] 
  temp<-as.data.frame(temp, na.rm = TRUE)
  temp.y <- cbind(SURVIVAL, EVENT)
  temp.model <- glm(temp.y ~  TempTreatment, binomial)
  results[i,2] <- dose.p(temp.model)
  se<-attr(dose.p(temp.model), 'SE')
  results[i,3]<-matrix(se)
  results[i,4]<-length(temp$dead[!is.na(temp$dead)])
}   

View(results)

results[,1]<-c('ASBA', 'ASBB', 'ASBC', "ASDA", 'ASDB', 'ASDC', 'ASWA', 'ASWB', 'ASWC', 'ASTA', 'ASTB', 'ASTC', 'ATBA', 'ATBB', 'ATBC', 'ATDA', 'ATDB', 'ATDC', 'ATWA', 'ATWB', 'ATWC', 'ATTA', 'ATTB', 'ATTC', 'WSBA', 'WSBB', 'WSBC', 'WSDA', 'WSDB', 'WSDC', 'WSWA', 'WSWB', 'WSWC', 'WSTA', 'WSTB', 'WSTC', 'WTBA', 'WTBB', 'WTBC', 'WTDA', 'WTDB', 'WTDC', 'WTWA', 'WTWB', 'WTWC', 'WTTA', 'WTTB', 'WTTC')

results[,5]<-c('ASB', 'ASB', 'ASB', "ASD", 'ASD', 'ASD', 'ASW', 'ASW', 'ASW', 'AST', 'AST', 'AST', 'ATB', 'ATB', 'ATB', 'ATD', 'ATD', 'ATD', 'ATW', 'ATW', 'ATW', 'ATT', 'ATT', 'ATT', 'WSB', 'WSB', 'WSB', 'WSD', 'WSD', 'WSD', 'WSW', 'WSW', 'WSW', 'WST', 'WST', 'WST', 'WTB', 'WTB', 'WTB', 'WTD', 'WTD', 'WTD', 'WTW', 'WTW', 'WTW', 'WTT', 'WTT', 'WTT')

############################################################################################XXXXXXXXXXXXXXXXXXXXXX##############
############################################################################################XXXXXXXXXXXXXXXXXXXXXX##############
#####Make the Jelly Bean groups.....#####
#Groups will be combined across time so we only have four groups:
AirSol <- c()
AirTP <- 
  WaterSol <- 
  WaterTP <- 
  
  
