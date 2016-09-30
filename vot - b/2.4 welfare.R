#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Assign observed travel time variation per week                                         |
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
    vot.results <- generatePath("intermediate/vot - b/choice model outputs/baseline.csv")
    wide.data <- generatePath("intermediate/vot - b/extended-crawler trips/wide data/wide data.csv")
    norm.crawler_path <- generatePath("stream/normal-crawler")
    
    # Hours evaluated
    initial.h <- 5
    final.h <- 22
    # Valid normal-crawler weeks
    w <- c(4,5,7,8,9)
  
    # required packages
    packages <- c("plyr")
    # install and load packages
    lapply(packages, pkgTest)
    
    
	 # Outputs files:
  # csv table
		out.path <- generatePath("intermediate/vot - b/welfare/welfare_by_trip.csv")
		out.path2 <- generatePath("intermediate/vot - b/welfare/welfare_by_week.csv")

# read data -----------------------------------------------------------------------------------
  # read VOT estimates
  VOT <- read.csv(vot.results)

  # read extended crawler aggregate
  WD <- read.csv(wide.data)
  
  # read Normal crawler
  files <- list.files(path = norm.crawler_path, full.names = T)
  NC <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
  rm(files)
  
  
# Discrete choice results -------------------------------------------------------------------
  WD$dep.hour <- ifelse(WD$dep.hour < initial.h | WD$dep.hour > final.h,
                        final.h, WD$dep.hour)
  
  TD <- merge(WD, VOT, by.x = "dep.hour", by.y = "central.hour")

# Normal-crawler data -------------------------------------------------------------------------
  # keep only valid crawls
  NCp <- subset(NC, traffic > 0)

  for (i in 1:length(w)){
    # subset to a particular week
    NCps <- subset(NCp, weeks == w[i])

    # add travel time (convert to minutes)
    NCps[,paste("time_w",w[i],sep="")] <- (NCps$traffic)/60
    NCps <- NCps[,c("trip_id", paste("time_w",w[i],sep=""))]

    # merge to Household data
    TD <- merge(TD, NCps, by.x = "ID_ORDEM", by.y = "trip_id")
   
    w.i <- paste("time_w", w[i], sep = "") 
    # set non positive travel time to NA
    TD[,w.i] <- ifelse(TD[,w.i] <= 0, NA, TD[,w.i])
  }

  # calculate marginal utility of money
  TD$MUM <- abs(TD$cost.m + TD$cost.I1.m*TD$I1 + TD$cost.I2.m*TD$I2)

# calculate V (observed utility) --------------------------------------------------------------
    for (i in 1:length(w)){
     
    TD$Time.car_w <- TD[,paste("time_w", w[i], sep ="")]
    
    TD$t
    V_walk.week <- paste("V.walk.", w[i], sep ="")
    TD[,V_walk.week] <- with(TD,
                               cost.m*Cost.walk +
                               cost.I1.m*Cost.walk*I1 +
                               cost.I2.m*Cost.walk*I2 +
                               time.m*Time.walk/60)
    
    V_car.week <- paste("V.car.", w[i], sep ="")
    TD[,V_car.week] <- with(TD,
                               car_c.m +
                               cost.m*Cost.car +
                               cost.I1.m*Cost.car*I1 +
                               cost.I2.m*Cost.car*I2 +
                               time.m*Time.car_w/60 +
                               car_age1.m*age.30_49 +
                               car_age2.m*age.50_99 +
                               car_f.m*female)
    
    V_pub.week <- paste("V.pub.", w[i], sep ="")
    TD[,V_pub.week] <- with(TD,
                               pub_c.m +
                               cost.m*Cost.pub +
                               cost.I1.m*Cost.pub*I1 +
                               cost.I2.m*Cost.pub*I2 +
                               time.m*Time.pub/60 +
                               pub_age1.m*age.30_49 +
                               pub_age2.m*age.50_99 +
                               pub_f.m*female)
    
    CS_week <- paste("CS_", w[i], sep ="")
    TD[,CS_week] <- (log(exp(TD[,V_pub.week])+
                         exp(TD[,V_car.week])+
                         exp(TD[,V_walk.week]))/TD$MUM)*TD$FE_VIA
                             
                             
    }
  
  # remove obs with NAs
  for (i in 1:length(w)){
  CSW <- paste("CS_",w[i],sep="")
  TD <- subset(TD, (!is.na(TD[,CSW])))
  }

  write.csv(TD, out.path, row.names=FALSE)
  

# weekly aggregates --------------------------------------------------------------
  week.agg <- matrix(nrow = length(w), ncol = 3)
  colnames(week.agg) <- c("week",
                         "car_travel.time",
                         "welfare")
  for (i in 1:length(w)){
   week.agg[i,1] <- w[i]
   week.agg[i,2] <- sum(TD[,paste("time_w", w[i], sep="")]*TD[,"FE_VIA"])
   week.agg[i,3] <- sum(TD[,paste("CS_", w[i], sep="")]*TD[,"FE_VIA"])
    
  }
  
  write.csv(week.agg, out.path2, row.names=FALSE)