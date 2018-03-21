plot(0,type='n', #make empty plot 
     xlim=c(0.75,4.25) # x limits
     , ylim=c(24, 36) # y limits 
     , xlab= '', # name x azis
     ylab = 'LT50', # name y axis
     main = 'Air Solitary', # main title (on middle of plot)
     pch=19, yaxt='n', axes=F, # removed axes
     cex.lab=1.05) # make empty plot to fill in 

arrows(c(1,2,3,4), data.summary2$se.plus, # bottom of arrows
       c(1,2,3,4), data.summary2$se.minus, # top of arrows
       angle = 90, code = 3, length = 0, lty = 1, lwd = 30, # jelly bean width
       col = palette(rainbow(4, s = 0.9, v = 1)))
# x-axis labels
axis(1, at=c(1,2,3,4), labels= data.summary2$gen.name, cex.axis=1.2, # text size
     las = 1, tick = FALSE) 

# add in points
points(c(1,2,3,4), data.summary2$mean.lt50, cex = 2, pch = 16, col = 'black')

# 
axis(2, cex.axis=1.2, tick = FALSE)
box()

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
TubeTapeColors <- c('#0ce2f3', '#00ff00', '#e6e600', '#ff8000' ) #blue, green, yellow, orange
