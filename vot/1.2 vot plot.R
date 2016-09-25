#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  vot plot                                                               |
#   |                                                                         |
#   |  By:                                                                    |
#   |  Renato Schwambach Vieira                                               |    
#   |  Big Data for Environmental Economics and Policy                        |
#   |  University of Illinois at Urbana Chamapaign                            |
#   |                                                                         |
#     ------------------------------------------------------------------------

# Prelims -------------------------------------------------------------------------------------
  rm(list=ls())

  # Inputs files:
    #   vot estimation results
    #     "stream/vot/extended crawler - vot.csv"
    
  # Outputs files:
    # png figure
    out.path <- Sys.getenv("BDEEP_votOutPath")
    if (out.path == ""){
      out.path <- "views/figures/vot/median vot.png" 
    }
  
  # Working directory
    wd.folder <- Sys.getenv("BDEEP_PROD")
    serv <- Sys.getenv("BDEEP_SERV")
    
    if (wd.folder == ""){
      if (serv == ""){
        setwd("//141.142.209.255/share/projects/Congestion")
      }
      else {
        setwd("~/share/projects/Congestion")
      }
    }
    
# Required packages
 
  # required packages
    packages <- c("ggplot2")

# function to verify packages and install missing ones ------------------------
  pkgTest <- function(x)
  {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  # run the function for all required packages
  lapply(packages, pkgTest)

# read data -----------------------------------------------------------------------------------

   df <- read.csv("stream/vot/extended crawler - vot.csv")
   
  ggplot() +
     geom_point(data = df, aes(y=vot.I1.m, x=central.hour)) +
     geom_errorbar(data = df,
                   aes(ymax = vot.I1.m + vot.I1.sd.m,
                       ymin = vot.I1.m - vot.I1.sd.m,
                       x=central.hour),
                   width=0.2) +
     scale_y_continuous(limits = c(0, 25)) +
     scale_x_continuous(breaks = 6:22) +
     labs(x = "departure time", y = "Median VOT (R$/hour)") + 
     ggtitle("VOT estimation")
  ggsave(file=out.path, width = 8, height = 5)