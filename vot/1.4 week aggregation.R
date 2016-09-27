#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Transportation Mode Discrete Choice Estimation for Sao Paulo 2012                      |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Renato Schwambach Vieira                                                               |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Prelims -------------------------------------------------------------------------------------
  rm(list=ls())

  # Inputs files:
    #   vot estimation results by trip
    #     "stream/vot/vot_by_trip.csv"
    # household survey
    #    "stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv"
    # auxiliary currency convert
    #    "stores/auxiliary/exchange rates.csv"
    # normal crawler files
    #    "stream/normal-crawler"

	source("environment.R")

  # Outputs files:
    # csv tables
		out.path <- generatePath("intermediate/vot/travel time and vot - daily.csv")
		out.path2 <- generatePath("intermediate/vot/travel time and vot - weekly.csv")

# Required packages -----------------------------------------------------------
  packages <- c("stargazer",
                "ggplot2",
                "mlogit",
                "data.table",
                "reshape")

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

  # read data from original household survey
  HH12 <- read.csv(generatePath("stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv"))

  # read vot by trip
  VOT <- read.csv(generatePath("stream/vot/vot_by_trip.csv"))

# convert values to 2012 USD
  EX <- read.csv(generatePath("stores/auxiliary/exchange rates.csv"))

  # Sao Paulo 2012
  HH12_EX <- subset(EX, Date == "28-Dec-12")
  HH12_b2u <- HH12_EX[1,"X"]

# read normal-crawler trips -----------------------------------------------------------------
  files <- list.files(path = generatePath("stream/normal-crawler"), full.names = T)

  # merge all those files into a single data frame
  NC <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
  rm(files)

  NC <- merge(NC, VOT, by.x = "trip_id", by.y = "ID_ORDEM")

  # vot per trip
  HH12 <- merge(HH12, VOT, by.x = "ID_ORDEM", by.y = "ID_ORDEM")

# Count valid trips per week ------------------------------------------------------------------

  # keep only valid crawls
  NCp <- subset(NC, traffic > 0)

  # count valid per week
  NCp$trips <- ave(NCp$traffic, NCp$week, FUN = length)

  # subset of weeks
  NCw <- NCp[!duplicated(NCp$weeks ), c("weeks","trips")]
  NCw <- NCw[order(NCw$weeks),]


# add weekly estimation to HH12 ----------------------------------------------------------------
  for (i in c(4,5,7,8,9)){
   #i<-7
   # subset to a particular week
   NCps <- subset(NCp, weeks == i)

   # add travel time (convert to minutes)
   NCps[,paste("time",i,sep="")] <- (NCps$traffic)/60
   NCps <- NCps[,c("trip_id", paste("time",i,sep=""))]

   # merge to Household data
   HH12 <- merge(HH12, NCps, by.x = "ID_ORDEM", by.y = "trip_id")
   HH12[,paste("travel.time_", i, sep = "")] <- ifelse(HH12$TIPOVG == 2,
                                                     HH12[,paste("time",i,sep="")],
                                                     HH12$DURACAO)
   # exclude non positive travel time
   HH12$ti <- HH12[,paste("travel.time_", i, sep = "")]
   HH12 <- subset(HH12, ti > 0)

   # Add VTT per trip
   HH12[,paste("VTT_", i, sep="")] <- ((HH12[,paste("travel.time_", i, sep="")])*(HH12$vot))/60

   # convert to 2012 USD
   HH12[,paste("VTT_", i, sep="")] <- HH12[,paste("VTT_", i, sep="")]/HH12_b2u

   # multiply by weights
   HH12[,paste("VTT.fe_", i, sep="")] <- HH12[,paste("VTT_", i, sep="")]*(HH12$FE_VIA)
  }

  # subset to car trips ------------------------------------------------------------------------
  week.cost_car <- matrix(nrow =0, ncol = 6)
  colnames(week.cost_car) <- c("source",
                               "c4", "c5", "c7", "c8","c9")

  HH12 <- subset(HH12, TIPOVG == 2)

  # export daily and weekly data
  HH12.gd <- HH12[,c("ID_ORDEM", "FE_VIA", "vot",
                     "travel.time_4","travel.time_5","travel.time_7","travel.time_8","travel.time_9",
                     "VTT_4", "VTT_5", "VTT_7", "VTT_8", "VTT_9")]

  HH12.wgd <- matrix(nrow =5, ncol=3)
  colnames(HH12.wgd) <- c("Week", "Cost", "Time")
  HH12.wgd[1,] <- c(4, 5*sum(HH12.gd$VTT_4*HH12.gd$FE_VIA), 5*sum(HH12.gd$travel.time_4*HH12.gd$FE_VIA)/60)
  HH12.wgd[2,] <- c(5, 5*sum(HH12.gd$VTT_5*HH12.gd$FE_VIA), 5*sum(HH12.gd$travel.time_5*HH12.gd$FE_VIA)/60)
  HH12.wgd[3,] <- c(7, 5*sum(HH12.gd$VTT_7*HH12.gd$FE_VIA), 5*sum(HH12.gd$travel.time_7*HH12.gd$FE_VIA)/60)
  HH12.wgd[4,] <- c(8, 5*sum(HH12.gd$VTT_8*HH12.gd$FE_VIA), 5*sum(HH12.gd$travel.time_8*HH12.gd$FE_VIA)/60)
  HH12.wgd[5,] <- c(9, 5*sum(HH12.gd$VTT_9*HH12.gd$FE_VIA), 5*sum(HH12.gd$travel.time_9*HH12.gd$FE_VIA)/60)

  write.csv(HH12.gd, out.path, row.names = F)
  write.csv(HH12.wgd, out.path2, row.names = F)
