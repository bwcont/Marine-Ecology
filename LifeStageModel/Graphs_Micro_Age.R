################################################# 
# Title: Graphs for Microhabitat & Size Class for M. californianus
# Purpose: Graphs for Microhabitat and Size Class Work
# Created by : E. Cruz & L. Pandori
# Created on : 5/21/2018
# Last Edited : 6/5/2018
#################################################
# Set WD
setwd()
# clear workspace
rm(list=ls())

#Set DAta
data.muss <- read.csv("Model_Format_Summary.csv")

#Calculating averages using ddply from plyr package 
library(plyr)

Data.summary <- ddply(data1, c('Site', 'TideH','Microhabitat'), summarize,
                      mean.Association = mean(data1$Association), # this is saying to take the mean range and ignore missing data if there is any
                      N = length(data1$Association), # how many samples do you have (code says what is not a missing value and sum the counts)
                      Assoc.SE = sd(data1$Association)/sqrt(N), # this is the standard error
                      min.Assoc = min(data1$Association), # minimum tide height across all transects within a site
                      max.Assoc = max(data1$Association) # max height across all transects
)

Data.summary


#####Graphs#####
#Colors
Col.MicroHab <- c('#FF0000','#FF8000', '#00CC00', '#66B2FF', '#B266FF')
Col.TH <- c("blue", "darkviolet", "red")

###Fig1. TH Graph####
#(Using Both Sites)#
#Means
lowmean <- mean(data.muss$TotalMussels[data.muss$TideH == "1"], na.rm = TRUE)
midmean <- mean(data.muss$TotalMussels[data.muss$TideH == "2"], na.rm = TRUE)
highmean <- mean(data.muss$TotalMussels[data.muss$TideH == "3"], na.rm = TRUE)
#Standard Deviation
  midsd <- sd(data.muss$TotalMussels[data.muss$TideH == "1"], na.rm = TRUE)
  highsd <- sd(data.muss$TotalMussels[data.muss$TideH == "2"], na.rm = TRUE)
  lowsd <- sd(data.muss$TotalMussels[data.muss$TideH == "3"], na.rm = TRUE)

plot(0,type='n', #make empty plot 
     xlim=c(0.5,3.5) # x limits
     , ylim=c(1, 27) # y limits 
     , xlab= 'Tide Height', # name x azis
     ylab = 'Average Total Mussel', # name y axis
     main = ' ', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

lines(c(1,2,3), c(4.9, 7.5, 0.91), type="h",lwd=55,lend="butt", col = Col.TH)

arrows(c(1,2,3), c(4.9 + 10.78, 7.5 + 18.58, 0.9 + 2.102),    
       c(1,2,3), c(0, 0, 0),
angle = 90, code = 3, length = 0, lty = 1, lwd = 2, # jelly bean width
col = 'black')

points(x = c(1,2,3), y = c(4.9, 7.5, 0.91), cex = 1, pch = 16 )
axis(1, c(1,2,3), c('Low', 'Mid', 'High'), cex=2, pch=16, col = "black")
axis(2)
box()

###Fig. 2: Average Association of Micro per site####
plot(0,type='n', #make empty plot 
     xlim=c(0.75,11.25) # x limits
     , ylim=c(-1.15,1) # y limits 
     , xlab= ' ', # name x azis
     ylab = 'Association', # name y axis
     main = ' ', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

arrows(c(1,2,3,4,5,7,8,9,10,11), c(-0.95-0.08, -0.67-0.53 , -0.90-0.21, -0.62-0.43, -0.87-0.18, -0.97-0.07, -0.73-0.28, -0.92-0.19, -0.72-0.32 ,-0.76-0.38), 
       c(1,2,3,4,5,7,8,9,10,11), c(-0.95+0.08, -0.67+0.53 , -0.90+0.21, -0.62+0.43, -0.87+0.18, -0.97+0.07, -0.73+0.28, -0.92+0.19, -0.72+0.32 ,-0.76+0.38),
       angle = 90, code = 3, length = 0, lty = 1, lwd = 12, # jelly bean width
       col = Col.MicroHab)

points(x = c(1,2,3,4,5,7,8,9,10,11), y = c(-0.95, -0.67 , -0.90,-0.62,-0.87,-0.97,-0.73,-0.92,-0.72 ,-0.76), cex = 1.4, pch = 16 )

axis(1, c(3, 9), c("LCDM", 'CCSP'), cex=2, pch=16, col = "black")
axis(2)
abline(h=0)

##May need to remove this legend, keep for now
legend("topright", legend=c('UnShelt_Solitary',
                            'UnShelt_Aggregate',
                            'Sheltered_Solitary',
                            'Sheltered_Aggregate',
                            'Tidepool'), fill = Col.MicroHab, cex=0.9)

box()


###Fig. 3-7: Size Class Distribution Association With Microhabitat####
#(Using CCSP)#

##Unsheltered Solitary
plot(0,type='n', #make empty plot 
     xlim=c(0.75,9.25) # x limits
     , ylim=c(-1,1) # y limits 
     , xlab= 'Age Class', # name x azis
     ylab = 'Association', # name y axis
     main = 'Unsheltered Solitary', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

arrows(c(1,2,3,4,5,6,7,8,9), c(-1, -0.8578148452-0.03, -0.9619047619-0.066, -1, -0.9134949081-0.15, -0.9775533109-0.039, -0.9867724868-0.023, -1, -1), 
       c(1,2,3,4,5,6,7,8,9), c(-1, -0.8578148452+0.03, -0.9619047619+0.066, -1, -0.9134949081+0.15, -0.9775533109+0.039, -0.9867724868+0.023, -1, -1),
       angle = 90, code = 3, length = 0, lty = 1, lwd = 13, # jelly bean width
       #Change to single color
       col = "red")

points(x = c(1,2,3,4,5,6,7,8,9), y = c(-1, -0.8578148452, -0.9619047619, -1, -0.9134949081, -0.9775533109, -0.9867724868, -1, -1)
       , cex = 1.4, pch = 16 )

abline(h=0)
axis(1, c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), c('0','10','20','30','40','50','60','70','80'), cex=2, pch=16, col = "black")
axis(2)
box()

##Unsheltered Aggregate
plot(0,type='n', #make empty plot 
     xlim=c(0.75,9.25) # x limits
     , ylim=c(-1,1) # y limits 
     , xlab= 'Age Class', # name x azis
     ylab = 'Association', # name y axis
     main = 'Unsheltered Aggregate', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

arrows(c(1,2,3,4,5,6,7,8,9), c(-0.788-0.219,-0.449-0.29, -0.348-0.289, -0.650-0.358, -0.770-0.203, -0.840-0.162, -0.908-0.132,-0.940-0.09, -0.851-0.25), 
       c(1,2,3,4,5,6,7,8,9), c(-0.788+0.219, -0.449+0.29, -0.348+0.289, -0.650+0.358, -0.770+0.203,  -0.840+0.162, -0.908+0.132,  -0.940+0.09,-0.851+0.25),
       angle = 90, code = 3, length = 0, lty = 1, lwd = 13, # jelly bean width
       #Change to single color
       col = "#FF8000")

points(x = c(1,2,3,4,5,6,7,8,9), y = c(-0.788,-0.449,-0.348,-0.650,-0.770,-0.840,-0.908,-0.940,-0.851)
       , cex = 1.4, pch = 16 )

abline(h=0)
axis(1, c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), c('0','10','20','30','40','50','60','70','80'), cex=2, pch=16, col = "black")
axis(2)
box()

##Sheltered Solitary
plot(0,type='n', #make empty plot 
     xlim=c(0.75,9.25) # x limits
     , ylim=c(-1,1) # y limits 
     , xlab= 'Age Class', # name x azis
     ylab = 'Association', # name y axis
     main = 'Sheltered Solitary', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

arrows(c(1,2,3,4,5,6,7,8,9), c(-0.928-0.124,
                               -0.849-0.132,
                               -0.873-0.22,
                               -0.641-0.478,
                               -1.000,
                               -1.000,
                               -1.000,
                               -1.000,
                               -1.000), 
       c(1,2,3,4,5,6,7,8,9), c(-0.928+0.124, -0.849+0.132, -0.873+0.22, -0.641+0.478, -1.000, -1.000,
-1.000, -1.000, -1.000),
       angle = 90, code = 3, length = 0, lty = 1, lwd = 13, # jelly bean width
       #Change to single color
       col = "#00CC00")

points(x = c(1,2,3,4,5,6,7,8,9), y = c(-0.928, -0.849, -0.873, -0.641,  -1.000, -1.000,-1.000, -1.000,-1.000)
       , cex = 1.4, pch = 16 )

abline(h=0)
axis(1, c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), c('0','10','20','30','40','50','60','70','80'), cex=2, pch=16, col = "black")
axis(2)
box()

##Sheltered Aggregate
plot(0,type='n', #make empty plot 
     xlim=c(0.75,9.25) # x limits
     , ylim=c(-1,1) # y limits 
     , xlab= 'Age Class', # name x azis
     ylab = 'Association', # name y axis
     main = 'Sheltered Aggregate', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

arrows(c(1,2,3,4,5,6,7,8,9), c(-0.653-0.305,
                               -0.476-0.185,
                               -0.280-0.468,
                               -0.570-0.398,
                               -0.879-0.21,
                               -0.879-0.210,
                               -0.879-0.21,
                               -1.000,
                               -0.897-0.178), 
       c(1,2,3,4,5,6,7,8,9), c(-0.653+0.305,
                               -0.476+0.185,
                               -0.280+0.468,
                               -0.570+0.398,
                               -0.879+0.21,
                               -0.879+0.210,
                               -0.879+0.21,
                               -1.000,
                               -0.897+0.178),
       angle = 90, code = 3, length = 0, lty = 1, lwd = 13, # jelly bean width
       #Change to single color
       col = "#66B2FF")

points(x = c(1,2,3,4,5,6,7,8,9), y = c(-0.653,
                                       -0.476,
                                       -0.280,
                                       -0.570,
                                       -0.879,
                                       -0.879,
                                       -0.879,
                                       -1.000,
                                       -0.897)
       , cex = 1.4, pch = 16 )

abline(h=0)
axis(1, c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), c('0','10','20','30','40','50','60','70','80'), cex=2, pch=16, col = "black")
axis(2)
box()

##Tidepool
plot(0,type='n', #make empty plot 
     xlim=c(0.75,9.25) # x limits
     , ylim=c(-1,1) # y limits 
     , xlab= 'Age Class', # name x azis
     ylab = 'Association', # name y axis
     main = 'Tidepool', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

arrows(c(1,2,3,4,5,6,7,8,9), c(-0.748-0.437, -0.740-0.276, -0.672-0.493, -0.510-0.464,-0.658-0.593, -0.693-0.532, -1.000,-1.000,-0.945-0.095), 
       c(1,2,3,4,5,6,7,8,9), c(-0.748+0.437, -0.740+0.276, -0.672+0.493, -0.510+0.464,-0.658+0.593, -0.693+0.532, -1.000,-1.000,-0.945+0.095),
       angle = 90, code = 3, length = 0, lty = 1, lwd = 13, # jelly bean width
       #Change to single color
       col = "#B266FF")

points(x = c(1,2,3,4,5,6,7,8,9), y = c(-0.748,-0.740,-0.672,-0.510,-0.658,-0.693,-1.000,-1.000,-0.945)
       , cex = 1.4, pch = 16 )

abline(h=0)
axis(1, c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), c('0','10','20','30','40','50','60','70','80'), cex=2, pch=16, col = "black")
axis(2)
box()

####Fig. 8 Bar of Proportion (Using Totals for Size Classes)#########
#(Using CCSP)#

plot(0,type='n', #make empty plot 
     xlim=c(0.75,9.25) # x limits
     , ylim=c(0,100) # y limits 
     , xlab= 'Age Class', # name x azis
     ylab = 'Percent Total Mussels', # name y axis
     main = ' ', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in

#Maybe use arrows for top and bottom portions of the bars?

axis(1, c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), c('0','10','20','30','40','50','60','70','80'), cex=2, pch=16, col = "black")
axis(2)
box()








