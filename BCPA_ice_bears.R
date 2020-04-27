######################################################
#####     BCPA ICE BEARS    ##########################
######################################################

rm(list = ls())

library(bcpa)
library(proj4)
library(dplyr)

load('all_v2.RData')
load('ows_land.RData')
load('bcpa.RData')

# Create land bear & ice bear distinction in database

#ows.land$id.datetime <- paste(ows.land$id, ows.land$datetime, sep = " ")
#all.v2$land_bear_ows <- ifelse(all.v2$id.datetime %in% ows.land$id.datetime, 1, 0) # column for land bear during ows
#save(all.v2, file = 'all_v2.RData')

ice <- subset(all.v2, ows==1 & land_bear_ows == 0) # Separate ice bears during OWS

noData <- ice %>% # remove bears with < 20 data points
  add_count(id) %>%
  filter(n < 100)

ice <- anti_join(ice, noData)

length(unique(ice$id)) # 157 bears with > 100 GPS points

# ------  BCPA  -------------------------------------------------------------------------- #

unique(ice$id)

# Test one bear

pb <- subset(ice, id == 'pb_06336.2006')
pb <- select(pb, -Y)

M <- as.matrix(cbind(pb$gps_lon, pb$gps_lat)) #create a matrix to project
xy <- project(M, '+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') #project to polar stereographic
X <- xy[,1]
Y <- xy[,2]
pb <- cbind(pb,X,Y)


track <- MakeTrack(X=pb$X, Y=pb$Y, Time=pb$datetime) #create track object
plot(track)

vt <- GetVT(track, units = "day") # get step length and turning angles

# plot 

hist(vt$V, breaks=20, col="grey") #persistence of movement (velocity)
hist(vt$Theta, breaks=20, col="grey") # turning angle


# WindowSweep

#Sweep a moving window over the data to detect change points. Windowsize is in number of fixes. For a sampling schedule of 1 fix every 10 minutes, then a window size of 12 means 12 fixes = 2hrs (assuming perfect fix aqcuisition every 10mins)
#You can input whatever variable you like "V", "V*cos(Theta)", "V*sin(Theta)" etc. See Guarie et al for explanations of the difference
#"V*Cos(Theta) =  Persistence Velocity, The tendency and magnitude of a movement to persist in a given direction
#System time is just for counting how long the operation takes. "progress" give progress bar."Plotme" must be FALSE if using R studio
#K is a measure of how sensitive the analysis is. A value of 2 is normal, 0.5 is very insensitive

pb.ws<-WindowSweep(vt,"V*cos(Theta)", windowsize=10, progress=TRUE, plotme=FALSE, K=2)

#Plot the output. Threshold= number of models that detected a significant change
#see attached R.script for tweaked code for plotting function "bcpa.plot"

#plot outputs as "flat", ie.: without any smoothing between each point. Time needs to be a POSIXct object
#clusterwidth tells the function to group change points that were detected within a certain distance from each other. default =1
#"rho.where" and "mu.where" are for placing the legend if you want one.


plot.bcpa(pb.ws, type="flat",ylab="Persistence Velocity",xlab="Date", threshold=7,clusterwidth=5, rho.where='nowhere', mu.where="nowhere")

#The function "ChangePointSummary" will output a table of changepoints and a table of phases. 
#The "changes" object tells you what points in time the behaviour changed and what model was selected in classifying that change.
# The change points are selected based on changes in three key parameters beteween each window and the previous one

# mu.hat = Mean PV= mean persistence velocity  within the window
# s.hat = Variance in PV =  variability of PV within the windo
# rho.hat = Spatial autocorrelation within the window

#The 7 types of possible model to be selected at each change point are as follows:
# M0 = No change in any property (null hyptothesis)
# M1= mean PV changes only
# M2= variance changes only
# M3= autocorellation changes only
# M4= mean PV and variance change
# M5= mean PV and autocorellation change
# M6 = variance and autocorellation change
# M7 = All three properties change

#The 'phases' object outlines the phases that are between the change points and what the properties are inside each phase 

#interpreting what thsese changes and phases mean for the animal will require looking at the graph of the time series
#and some knowledge of your study species' movement behaviour
#BCPA identifies change points, it does not tell you what they mean but gives you some indications
#for example a big increas in 'mu.hat' means a change to faster more directed movement. 

####Read Gurarie et al, 2011 for detailed explanations


#be aware that all of these measures can be affected by the number of points that there are within each window.
#e.g. If there are only two points to work with the variance could be a lot higher than expected if they are very different.

changes<-ChangePointSummary(pb.ws,clusterwidth=5, tau=TRUE)
length(changes$breaks$size)
changes

# mu.hat = mean
# s.hat = sd
# rho.hat = autocorrelation


#Plots trajectory with Mean Persistence velocity
#the legend is a bit crazy but I am not sure how to fix it!
#(see code for additonal functions attached R.script)

PathPlot(track, pb.ws, plotlegend = TRUE)

# Create list

breaks<-changes$breaks
breaks<-breaks[,2:5]

Start<-data.frame(middle=9999, size=9999,modelmode=9999,middle.POSIX=vt$T.POSIX[1])

breaks<-rbind(Start,breaks)
phases <- changes$phases

Breaks_and_Phases<-cbind(phases, breaks)
Breaks_and_Phases$id <- 'pb_06336.2006'

bcpa <- rbind(bcpa,Breaks_and_Phases)

save(bcpa, file = 'bcpa.RData')

#############################################################
###############################################################

#tweaked plotting functions

bcpa.plot<-function (x, type = c("smooth", "flat")[1], threshold = 3, clusterwidth = 1, 
                     col.cp = rgb(0.5, 0, 0.5, 0.5), pt.cex = 0.5, legend = TRUE, xaxt="n",
                     rho.where = "topleft", mu.where = "nowhere", ...) 
{
  windowsweep <- x
  x <- windowsweep$x
  t.POSIX <- windowsweep$t.POSIX
  t <- windowsweep$t
  ws <- windowsweep$ws
  plot(t.POSIX, x,axes=FALSE, type = "n", ...)
  lines(t.POSIX, x, col = "grey")
  if (type == "smooth") {
    if ("pp.smooth" %in% names(windowsweep)) 
      pp <- windowsweep$pp.smooth
    else pp <- PartitionParameters(windowsweep, type = "smooth")
    GoodBreaks <- ws$Break[ws$Model > 0]
    GoodBreaks <- as.data.frame(table(GoodBreaks))
    GoodBreaks <- data.frame(GoodBreaks, t.POSIX = t.POSIX[match(GoodBreaks[, 
                                                                            1], windowsweep$t)])
    GoodBreaks[, 1] <- as.numeric(as.character(GoodBreaks[, 
                                                          1]))
    GoodBreaks <- GoodBreaks[GoodBreaks$Freq >= threshold, 
                             ]
    abline(v = GoodBreaks[, 3], lwd = GoodBreaks[, 2]/threshold * 
             2, col = col.cp)
    rho.scaled <- pp$rho.hat/max(pp$rho.hat, na.rm = 1)
    rho.int <- round(rho.scaled * 999 + 1)
    palette(topo.colors(1000))
    points(t.POSIX, x, pch = 21, col = "darkgrey", bg = rho.int, 
           cex = pt.cex, lwd = 0.5)
    lines(t.POSIX, pp$mu.hat, lwd = 1.5)
    lines(t.POSIX, pp$mu.hat + pp$s.hat, col = "red", lwd = 1.5)
    lines(t.POSIX, pp$mu.hat - pp$s.hat, col = "red", lwd = 1.5)
    rho.hat <- pp$rho.hat
  }
  if (type == "flat") {
    cpsummary <- ChangePointSummary(windowsweep, clusterwidth = clusterwidth)
    phases <- cpsummary$phases
    breaks <- cpsummary$breaks
    whichphase <- findInterval(t, phases$t0)
    rho.hat <- phases$rho.hat[whichphase]
    rho.int <- round(rho.hat/max(rho.hat, na.rm = TRUE) * 
                       999 + 1)
    palette(topo.colors(1000))
    points(t.POSIX, x, pch = 21, col = "darkgrey", bg = rho.int, 
           cex = pt.cex, lwd = 0.5)
    closematch <- rep(NA, length = nrow(phases))
    for (i in 1:nrow(phases)) closematch[i] <- which(abs(t - 
                                                           phases$t0[i]) == min(abs(t - phases$t0[i])))[1]
    phases$t0.POSIX <- t.POSIX[closematch]
    phases$t1.POSIX <- t.POSIX[c(closematch[-1], length(t))]
    t.mid <- (windowsweep$t[-1] + windowsweep$t[-length(windowsweep$t)])/2
    segments(phases$t0.POSIX, phases$mu.hat, phases$t1.POSIX, 
             phases$mu.hat, lwd = 1.5)
    segments(phases$t0.POSIX, phases$mu.hat - phases$s.hat, 
             phases$t1.POSIX, phases$mu.hat - phases$s.hat, col = "red", 
             lwd = 1.5)
    segments(phases$t0.POSIX, phases$mu.hat + phases$s.hat, 
             phases$t1.POSIX, phases$mu.hat + phases$s.hat, col = "red", 
             lwd = 1.5)
    abline(v = phases$t0.POSIX[-1], lwd = breaks$size/max(breaks$size) * 
             4, col = col.cp)
  }
  if (legend) {
    legend.cols <- topo.colors(1000)[seq(1, 1000, length = 5)]
    legend.rhos <- signif(seq(0, max(rho.hat, na.rm = TRUE), 
                              length = 5), 2)
    if (rho.where != "nowhere") 
      legend(rho.where, bg = "white", legend = c(expression(hat(rho)), 
                                                 legend.rhos),cex=0.8, bty="o",pch = 19, ncol = 3, col = c(0, 
                                                                                                           legend.cols), xjust = 0.2, yjust = 0.2)
    if (mu.where != "nowhere") 
      legend(mu.where, bg = "white", legend = c(expression(hat(mu)), 
                                                expression(hat(mu) %+-% hat(sigma))), lty = 1, 
             lwd = 2:1, col = c("black", "red"), xjust = 0.5, 
             yjust = 0.5, cex=0.8, bty="o")
  }
  palette("default")
  x<-round(x,3)
  
  axis(side=2,at=seq(min(x),max(x), by=0.001 ))
  box()
  
}

#tweaked PathPlot function to display colour by Persistence velocity instead of autocorrelation

PathPlot.Persistence<-function (Data, windowsweep, type = c("smooth", "flat")[1], clusterwidth = 1, 
                                plotlegend = TRUE, tauwhere = "topright", n.legend = 5, ncol.legend = 1, 
                                bty.legend = "n", ...) 
{
  if (!("Z" %in% names(Data))) 
    z <- Data$X + (0+1i) * Data$Y
  else z <- Data$Z
  if (type == "flat") 
    pp <- PartitionParameters(windowsweep, type = type, clusterwidth = clusterwidth)
  if (type == "smooth") 
    if ("pp.smooth" %in% names(windowsweep)) 
      pp <- windowsweep$pp.smooth
    else pp <- PartitionParameters(windowsweep, type = type)
    Segments <- function(z, col = col, lwd = lwd) {
      n <- length(z)
      segments(Re(z[-n]), Im(z[-n]), Re(z[-1]), Im(z[-1]), 
               col = col, lwd = lwd)
    }
    mu.hat <- pp$mu.hat
    rho.hat <- pp$rho.hat
    rho.max <- max(rho.hat, na.rm = 1)
    rho.int <- round(rho.hat/rho.max * 999 + 1)
    mu.max <- max(mu.hat, na.rm = 1)
    mu.int <- abs(round(mu.hat/mu.max * 999 + 1))
    
    
    palette(rev(heat.colors(1000)))
    plot(z, asp = 1, pch = 19, cex = 0.5, col = "grey", ...)
    points(z[c(1, length(z))], pch = c(24, 23), bg = c("green", "blue"), cex = 1, lwd = 1.5, col = "darkgrey")
    
    
    
    Segments(z, col = mu.int, lwd = abs(mu.hat/max(mu.hat, na.rm = TRUE)) *4)
    if (plotlegend) 
      legend(tauwhere, lty = 1, title = ("Persistence Velocity"), 
             ncol = ncol.legend, bty = bty.legend, lwd = 2, col = seq(0, 
                                                                      999, length = n.legend) + 1, legend = signif(seq(0, 
                                                                                                                       max(mu.hat), length = n.legend), 2)) 
    palette("default")
}



