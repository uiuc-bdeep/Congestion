#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Save weekly and individual results for vizualization                                   |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Renato Schwambach Vieira                                                               |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Prelims -------------------------------------------------------------------------------------
  rm(list=ls())
  source("environment.R")

  # Inputs files:
    vot.results <- generatePath("intermediate/vot - b/extended-crawler trips/vot_by_trip.csv")
    hous.survey_path <- generatePath("stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv")
    exch.rates <- generatePath("stores/auxiliary/exchange rates.csv")
    norm.crawler_path <- generatePath("stream/normal-crawler")
    
    
  # Valid weeks
  w <- c(4,5,7,8,9)
    
  # Required packages
  packages <- c("stargazer",
                "ggplot2",
                "mlogit",
                "data.table",
                "reshape")
  # install and load packages
  lapply(packages, pkgTest)

  # Outputs files:
  # csv tables
		out.path <- generatePath("intermediate/vot - b/travel time and vot - daily.csv")
		out.path2 <- generatePath("intermediate/vot - b/travel time and vot - weekly.csv")

# read data -----------------------------------------------------------------------------------

  # read data from original household survey
  HH12 <- read.csv(hous.survey_path)

  # read vot by trip
  VOT <- read.csv(vot.results)

# convert values to 2012 USD
  EX <- read.csv(exch.rates)

  # Sao Paulo 2012
  HH12_EX <- subset(EX, Date == "28-Dec-12")
  HH12_b2u <- HH12_EX[1,"X"]

# read normal-crawler trips -----------------------------------------------------------------
  files <- list.files(path = norm.crawler_path, full.names = T)

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
  for (i in 1:length(w)){
   #i<-7
   # subset to a particular week
   NCps <- subset(NCp, weeks == w[i])

   # add travel time (convert to minutes)
   NCps[,paste("time",w[i],sep="")] <- (NCps$traffic)/60
   NCps <- NCps[,c("trip_id", paste("time",w[i],sep=""))]

   # merge to Household data
   HH12 <- merge(HH12, NCps, by.x = "ID_ORDEM", by.y = "trip_id")
   HH12[,paste("travel.time_", w[i], sep = "")] <- ifelse(HH12$TIPOVG == 2,
                                                     HH12[,paste("time",w[i],sep="")],
                                                     HH12$DURACAO)
   # exclude non positive travel time
   HH12$ti <- HH12[,paste("travel.time_", w[i], sep = "")]
   HH12 <- subset(HH12, ti > 0)

   # Add VTT per trip
   HH12[,paste("VTT_", w[i], sep="")] <- ((HH12[,paste("travel.time_", w[i], sep="")])*(HH12$vot))/60

   # convert to 2012 USD
   HH12[,paste("VTT_", w[i], sep="")] <- HH12[,paste("VTT_", w[i], sep="")]/HH12_b2u

   # multiply by weights
   HH12[,paste("VTT.fe_", w[i], sep="")] <- HH12[,paste("VTT_", w[i], sep="")]*(HH12$FE_VIA)
  }

  # subset to car trips ------------------------------------------------------------------------
  week.cost_car <- matrix(nrow =0, ncol = length(w) + 1)
  colnames(week.cost_car) <- c("source", paste("c", w, sep =""))
  
  HH12 <- subset(HH12, TIPOVG == 2)

  # export daily and weekly data
  HH12.gd <- HH12[,c("ID_ORDEM", "FE_VIA", "vot",
                     paste("travel.time_", w, sep =""),
                     paste("VTT_", w, sep =""))]

  HH12.wgd <- matrix(nrow =length(w), ncol=3)
  colnames(HH12.wgd) <- c("Week", "Cost", "Time")
  
  for (i in 1:length(w)){
  HH12.wgd[i,] <- c(w[i], 5*sum(HH12.gd[,paste("VTT_",w[i], sep="")]*
                                HH12.gd$FE_VIA),
                          5*sum(HH12.gd[,paste("travel.time_",w[i], sep ="")]*
                                HH12.gd$FE_VIA)/60)
  }
  write.csv(HH12.gd, out.path, row.names = F)
  write.csv(HH12.wgd, out.path2, row.names = F)
