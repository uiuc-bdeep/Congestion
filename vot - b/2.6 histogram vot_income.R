#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  vot income histogram                                                   |
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
    ext_craw_trips <- generatePath("intermediate/vot - b/welfare/welfare_by_trip.csv")
    
    # required packages
    packages <- c("ggplot2", "gtable", "grid")
    # install and load packages
    lapply(packages, pkgTest)
	   
  # Outputs files:
    # png figure
	  	out.path <- generatePath("views/figures/vot/hitogram vot income.png")


# read data -----------------------------------------------------------------------------------
  df <- read.csv(ext_craw_trips)
	 df <- df[!duplicated(df$ID_PESS),]
	 df$hour.ipc <- df$RENDA_FA/(21*8)
	 
	 dfs <- subset(df, hour.ipc > 0)
	 
	 dfs$vot_income <- dfs$vot/(dfs$hour.ipc)
	 
	 dfs <- subset(dfs, vot_income <= quantile(df$vot_income, 0.975))
	 
	 #df$vot_income <- ifelse(df$vot_income > 5, 5, df$vot_income)
	 ggplot() +
	  	 geom_histogram(data = dfs, aes(x =vot_income),
	  	                fill = "darkolivegreen",
	  	                alpha = 0.7,
	  	                binwidth = 0.1) +
	    labs(x = "VOT / Income") +
     ggtitle("Histogram of VOT / Income") +
	    scale_x_continuous(breaks = seq(0, 5,0.5)) +
     theme(panel.grid.minor.x = element_blank() ,
           panel.grid.minor.y = element_blank())
	 ggsave(file=out.path, width = 8, height = 5)
	 
	    