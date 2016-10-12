#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  ridership with fuel and fare prices                                    |
#   |                                                                         |
#   |  By:                                                                    |
#   |  Renato Schwambach Vieira                                               |
#   |  Big Data for Environmental Economics and Policy                        |
#   |  University of Illinois at Urbana Chamapaign                            |
#   |                                                                         |
#     ------------------------------------------------------------------------

# Prelims -------------------------------------------------------------------------------------
  rm(list=ls())
  source("environment.R")

  # Inputs files:
    rffp <- generatePath("stores/auxiliary/rider fuel fare.csv")

    # required packages
    packages <- c("ggplot2")
    # install and load packages
    lapply(packages, pkgTest)
	
  # Outputs files:
    # png figure
	  	out.path <- generatePath("views/figures/ridership/ridership and prices.png")
	  	out.path2 <- generatePath("views/figures/ridership/ridership and gas prices.png")
	  	out.path3 <- generatePath("views/figures/ridership/ridership and fare prices.png")


# read data -----------------------------------------------------------------------------------

  df <- read.csv(rffp)
  
 
  df$date <- as.Date(df$Date)
  
  x <- df$date
  xl <- df$Date
  
  daterange <- c(min(df$date), max(df$date))
  
	 y1 <- df$gasolina
  y2 <- df$Pass/1000000
  y1.1 <- df$fare
  
  
  png(out.path, width = 11, height = 8, units = "in", res = 144)
  
  par(mar=c(6,6,6,6), xpd=TRUE, new =F)
  #par(mar=c(10,6,4,6), xpd=TRUE, new =F)
  plot(x,y1,type="l",col="red",xlab="",ylab="",ylim = c(2,5), axes=FALSE)
  par(new=TRUE)
  plot(x,y1.1,type="l",col="darkred",xlab="",ylab="",ylim = c(2,5), axes=FALSE)
  par(new=TRUE)
  axis(2, col = 'red', col.axis = 'red', col.ticks = 'red', ylim = c(0,5))
  par(new=TRUE)
  plot(x, y2,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
       ylim = c(0,10), axes=FALSE)
  par(new=TRUE)
  axis(4, col = 'blue', col.axis = 'blue', col.ticks = 'blue', ylim = c(0,15))
  axis.Date(1, at=seq(as.Date("2013-07-01"), as.Date("2016-07-30"),by="3 months"),
            format = "%b %y", las=2)
  par(new=TRUE, las = 0)
  mtext("Transit Ridership \n (million)",side=4,line=4, col="blue")
  mtext("Price \n (R$)",side=2,line=3.5, col="red")
  legend("top", col=c("red", "darkred", "blue"), ncol =3,
         lty=1, bty = "n", inset=c(0,-0.2),
         legend=c("gasoline price","transit fare", "bus ridership"))
  dev.off()
  
  
  png(out.path2, width = 11, height = 8, units = "in", res = 144)
  par(mar=c(6,6,6,6), xpd=TRUE, new =F)
  #par(mar=c(10,6,4,6), xpd=TRUE, new =F)
  plot(x,y1,type="l",col="red",xlab="",ylab="",ylim = c(2,5), axes=FALSE)
  par(new=TRUE)
  axis(2, col = 'red', col.axis = 'red', col.ticks = 'red', ylim = c(0,5))
  par(new=TRUE)
  plot(x, y2,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
       ylim = c(0,10), axes=FALSE)
  par(new=TRUE)
  axis(4, col = 'blue', col.axis = 'blue', col.ticks = 'blue', ylim = c(0,15))
  axis.Date(1, at=seq(as.Date("2013-07-01"), as.Date("2016-07-30"),by="3 months"),
            format = "%b %y", las=2)
  par(new=TRUE, las = 0)
  mtext("Transit Ridership \n (million)",side=4,line=4, col="blue")
  mtext("Price \n (R$)",side=2,line=3.5, col="red")
  legend("top", col=c("red", "blue"), ncol =2,
         lty=1, bty = "n", inset=c(0,-0.2),
         legend=c("gasoline price", "bus ridership"))
  dev.off()
  
  
  png(out.path3, width = 11, height = 8, units = "in", res = 144)
  par(mar=c(6,6,6,6), xpd=TRUE, new =F)
  #par(mar=c(10,6,4,6), xpd=TRUE, new =F)
  plot(x,y1.1,type="l",col="darkred",xlab="",ylab="",ylim = c(2,5), axes=FALSE)
  par(new=TRUE)
  axis(2, col = 'red', col.axis = 'red', col.ticks = 'red', ylim = c(0,5))
  par(new=TRUE)
  plot(x, y2,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
       ylim = c(0,10), axes=FALSE)
  par(new=TRUE)
  axis(4, col = 'blue', col.axis = 'blue', col.ticks = 'blue', ylim = c(0,15))
  axis.Date(1, at=seq(as.Date("2013-07-01"), as.Date("2016-07-30"),by="3 months"),
            format = "%b %y", las=2)
  par(new=TRUE, las = 0)
  mtext("Transit Ridership \n (million)",side=4,line=4, col="blue")
  mtext("Price \n (R$)",side=2,line=3.5, col="red")
  legend("top", col=c("darkred", "blue"), ncol =2,
         lty=1, bty = "n", inset=c(0,-0.2),
         legend=c("transit fare", "bus ridership"))
  dev.off()