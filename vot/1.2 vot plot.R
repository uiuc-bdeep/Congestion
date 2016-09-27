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
  source("environment.R")

  # Inputs files:
    vot.results <- generatePath("stream/vot/extended crawler - vot.csv")
    
    # required packages
    packages <- c("ggplot2")
    # install and load packages
    lapply(packages, pkgTest)
	
  # Outputs files:
    # png figure
	  	out.path <- generatePath("views/figures/vot/median vot.png")


# read data -----------------------------------------------------------------------------------

  df <- read.csv(vot.results)

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
