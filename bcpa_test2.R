rm(list = ls())
library(bcpa)

load('swimHMM.RData')
pb <- subset(swimHMM, ID=='pb_20446.2009') #subset one animal
pb$datetime <- as.POSIXct(pb$datetime) #change class to POSIXct

library(proj4)
M <- as.matrix(cbind(pb$gps_lon, pb$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]
pb <- cbind(pb,X,Y)

track <- MakeTrack(X=pb$gps_lon, Y=pb$gps_lat, Time=pb$datetime) #create track object
plot(track)

pb.VT <- GetVT(track, units='hour')
hist(pb.VT$V, breaks=20, col="grey") #persistence of movement
hist(pb.VT$Theta, breaks=20, col="grey")

#WindowSweep
pb.ws<-WindowSweep(pb.VT,"V*cos(Theta)", windowsize=60, progress=TRUE, plotme=FALSE, K=2)

#Get Best Break
BB <- GetBestBreak(x=pb.ws$x, t=pb.ws$t.POSIX, range=0.6)
plot(pb.ws$t.POSIX, pb.ws$x, type='l')
abline(v=BB, col=2, lwd=2, lty=3)


plot.bcpa(pb.ws, type="flat",ylab="Persistence Velocity",xlab="Date", threshold=10,clusterwidth=3, rho.where='nowhere', mu.where="nowhere")
axis.POSIXct(side=1,at=cut(mydata$Time, breaks="month"),format="%m-%Y" )

PathPlot.Persistence(track, pb.ws, plotlegend = TRUE)

###############################################################################################################
### Function to plot Persistence Velocity ###
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