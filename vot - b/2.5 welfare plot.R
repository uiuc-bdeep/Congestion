#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  generates vot plot                                                     |
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
    welfare.results <- generatePath("intermediate/vot - b/welfare/welfare_by_week.csv")
    ext_craw_week <- generatePath("stores/auxiliary/ext_craw_week.csv")
    
    # USD / BRL
    exch <- 2.048298
    
    # required packages
    packages <- c("ggplot2", "gtable", "grid")
    # install and load packages
    lapply(packages, pkgTest)
	   # Valid normal-crawler weeks
    w <- c(1:13)
    
    
  # Outputs files:
    # png figure
	  	out.path <- generatePath("views/figures/vot/welfare by week.png")


# read data -----------------------------------------------------------------------------------

  df <- read.csv(welfare.results)
	 ecw <- read.csv(ext_craw_week)
	 
	 df<- merge(df, ecw, by.x = "weeks", by.y = "ext.craw.week")
	 
	 df$CS.w <- -(df$CS.w/1000000)
	 df$time.w <- df$time.w/1000
	 
	 df <- subset(df, weeks %in% w)
	 
	 df$CS.w <- df$CS.w - mean(df$CS.w)
	 df$time.w <- df$time.w - mean(df$time.w)
	 df$datew <- as.Date(df$date)
	 df$datew <- format(df$datew, "%b-%d")
	 
	 
	 x <- df$weeks
	 xl <- df$datew
  y1 <- df$CS.w/exch
  y2 <- df$time.w
  
  
  png(out.path, width = 11, height = 8, units = "in", res = 144)
  
  par(mar=c(0,0,0,0), xpd=TRUE, pin=c(8,4), new =F)
  #par(mar=c(10,6,4,6), xpd=TRUE, new =F)
  plot(x,y1,type="l",col="red",xlab="",ylab="", ylim = c(-150,150), axes=FALSE)
  par(new=TRUE)
  plot(x,y1,type="p",col="red",xlab="",ylab="", ylim = c(-150,150), axes=F)
  axis(2, col = 'red', col.axis = 'red', col.ticks = 'red', ylim = c(-300,300))
  par(new=TRUE)
  plot(x, y2,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
       ylim = c(-1500,1500), axes=FALSE)
  par(new=TRUE)
  plot(x,y2,type="p",col="blue",yaxt="n",xlab="",ylab="",
       ylim = c(-1500,1500), axes=FALSE)
  axis(4, col = 'blue', col.axis = 'blue', col.ticks = 'blue', ylim = c(-1500,1500))
  axis(1, at = x, labels = xl, tck=-1, tcl = -0.5, cex.axis=1.05, par(las = 2))
  par(new=TRUE, las = 0)
  mtext("travel time \n (thousand hours)",side=4,line=4, col="blue")
  mtext("welfare loss \n (million US$)",side=2,line=3.5, col="red")
  mtext("week",side=1,line=6)
  legend("top", col=c("red","blue"), ncol =2,
         lty=1, bty = "n", inset=c(0,-0.2),
         legend=c("welfare loss","travel time"))
  dev.off()