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
    vot.results <- generatePath("intermediate/vot - b/choice model outputs/baseline.csv")
    
    # Hours evaluated
    initial.h <- 6
    final.h <- 22
    
    
    # required packages
    packages <- c("ggplot2")
    # install and load packages
    lapply(packages, pkgTest)
	
  # Outputs files:
    # png figure
	  	out.path <- generatePath("views/figures/vot/median vot - b.png")


# read data -----------------------------------------------------------------------------------

  df <- read.csv(vot.results)
  lim.inf <- min(min(df$vot.I1.m - df$vot.I1.sd.m), 0)
  lim.sup <- max(max(df$vot.I1.m + df$vot.I1.sd.m), 0)*1.1
  ggplot() +
     geom_point(data = df, aes(y=vot.I1.m, x=central.hour)) +
     geom_errorbar(data = df,
                   aes(ymax = vot.I1.m + vot.I1.sd.m,
                       ymin = vot.I1.m - vot.I1.sd.m,
                       x=central.hour),
                   width=0.2) +
     scale_y_continuous(limits = c(lim.inf, lim.sup)) +
     scale_x_continuous(breaks = initial.h:final.h) +
     labs(x = "departure time", y = "Median VOT (R$/hour)") +
     ggtitle("VOT estimation") +
     theme(panel.grid.minor.x = element_blank() ,
           panel.grid.minor.y = element_blank())
  ggsave(file=out.path, width = 8, height = 5)
