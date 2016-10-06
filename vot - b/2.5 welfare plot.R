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
    
    # required packages
    packages <- c("ggplot2")
    # install and load packages
    lapply(packages, pkgTest)
	   # Valid normal-crawler weeks
    w <- c(1:13)
    
    
  # Outputs files:
    # png figure
	  	out.path <- generatePath("views/figures/vot/welfare by week.png")


# read data -----------------------------------------------------------------------------------

  df <- read.csv(welfare.results)
	 df$CS.w <- df$CS.w/1000000
	 df <- subset(df, weeks %in% w)
	 df$CS.w <- df$CS.w - df$CS.w[1]
  ggplot() +
     geom_point(data = df, aes(y=CS.w, x=weeks)) +
     geom_line(data = df, aes(y=CS.w, x=weeks)) +
     scale_x_continuous(breaks = w[1]:w[length(w)]) +
     labs(x = "week", y = "(R$ Millions)") +
     ggtitle("Welfare Variation per Week") +
     theme(panel.grid.minor.x = element_blank() ,
           panel.grid.minor.y = element_blank())
  ggsave(file=out.path, width = 8, height = 5)
