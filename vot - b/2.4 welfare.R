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
    wide.data <- generatePath("intermediate/vot - b/extended-crawler trips/wide data/")
    norm.crawler_path <- generatePath("stream/normal-crawler")
    cs_path <- generatePath("intermediate/vot - b/welfare/welfare_by_hour/")
    
    # Hours evaluated
    initial.h <- 6
    final.h <- 22
    # Valid normal-crawler weeks
    w <- c(4,5,7,8,9,10)
  
    # required packages
    packages <- c("plyr")
    # install and load packages
    lapply(packages, pkgTest)
    
    
	 # Outputs files:
  # csv table
		out.path <- generatePath("intermediate/vot - b/welfare/welfare_by_hour/")
		out.path1 <- generatePath("intermediate/vot - b/welfare/welfare_by_trip.csv")
		out.path2 <- generatePath("intermediate/vot - b/welfare/welfare_by_week.csv")

# read data -----------------------------------------------------------------------------------
  # read VOT estimates
  VOT <- read.csv(vot.results)

  # read Normal crawler
  files <- list.files(path = norm.crawler_path, full.names = T)
  NC <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
  rm(files)
  
  
for (h in initial.h:final.h){
  
  # read extended crawler aggregate
  WD <- read.csv(paste(wide.data,"wide data - ", h, ".csv", sep = ""))
  WD <- subset(WD, dep.hour == h)
  
# Discrete choice results -------------------------------------------------------------------
  
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
  # baseline V
  TD$Time.car_base <- TD[,paste("time_w", w[1], sep ="")]
  
  for (i in 1:length(w)){
  
  TD$Time.car_w <- TD[,paste("time_w", w[i], sep ="")]
  TD$time.car_var <-  TD$Time.car_w/TD$Time.car_base
    
  
  TD$V_walk <- with(TD, cost.m*Cost.walk +
                        cost.I1.m*Cost.walk*I1 +
                        cost.I2.m*Cost.walk*I2 +
                        time.m*Time.walk/60)
  TD$V_pub <- with(TD,
                   pub_c.m +
                   cost.m*Cost.pub +
                   cost.I1.m*Cost.pub*I1 +
                   cost.I2.m*Cost.pub*I2 +
                   time.m*Time.pub/60 +
                   pub_age1.m*age.30_49 +
                   pub_age2.m*age.50_99 +
                   pub_f.m*female)
  
# car V ---------------------------------------------------------------------------------------
  TD$Cost.car.6l <- TD[,paste("Cost.car", h-2, "_00", sep = "")]
  TD$Time.car.6l <- TD[,paste("Time.car", h-2, "_00", sep = "")]*TD$time.car_var
  TD$V_car.6l <- with(TD,
                      car.6l_c.m +
                      cost.m*Cost.car.6l +
                      cost.I1.m*Cost.car.6l*I1 +
                      cost.I2.m*Cost.car.6l*I2 +
                      time.m*Time.car.6l/60 +
                      car.6l_age1.m*age.30_49 +
                      car.6l_age2.m*age.50_99 +
                      car.6l_f.m*female)
  TD$V_car.6l[is.na(TD$V_car.6l)] <- 0
  
  TD$Cost.car.5l <- TD[,paste("Cost.car", h-2, "_20", sep = "")]
  TD$Time.car.5l <- TD[,paste("Time.car", h-2, "_20", sep = "")]*TD$time.car_var
  TD$V_car.5l <- with(TD,
                      car.5l_c.m +
                      cost.m*Cost.car.5l +
                      cost.I1.m*Cost.car.5l*I1 +
                      cost.I2.m*Cost.car.5l*I2 +
                      time.m*Time.car.5l/60 +
                      car.5l_age1.m*age.30_49 +
                      car.5l_age2.m*age.50_99 +
                      car.5l_f.m*female)
  TD$V_car.5l[is.na(TD$V_car.5l)] <- 0
  
  TD$Cost.car.4l <- TD[,paste("Cost.car", h-2, "_40", sep = "")]
  TD$Time.car.4l <- TD[,paste("Time.car", h-2, "_40", sep = "")]*TD$time.car_var
  TD$V_car.4l <- with(TD,
                      car.4l_c.m +
                      cost.m*Cost.car.4l +
                      cost.I1.m*Cost.car.4l*I1 +
                      cost.I2.m*Cost.car.4l*I2 +
                      time.m*Time.car.4l/60 +
                      car.4l_age1.m*age.30_49 +
                      car.4l_age2.m*age.50_99 +
                      car.4l_f.m*female)
  TD$V_car.4l[is.na(TD$V_car.4l)] <- 0
  
   
  
  
  
  
  
  
  TD$Cost.car.3l <- TD[,paste("Cost.car", h-1, "_00", sep = "")]
  TD$Time.car.3l <- TD[,paste("Time.car", h-1, "_00", sep = "")]*TD$time.car_var
  TD$V_car.3l <- with(TD,
                      car.3l_c.m +
                      cost.m*Cost.car.3l +
                      cost.I1.m*Cost.car.3l*I1 +
                      cost.I2.m*Cost.car.3l*I2 +
                      time.m*Time.car.3l/60 +
                      car.3l_age1.m*age.30_49 +
                      car.3l_age2.m*age.50_99 +
                      car.3l_f.m*female)
  TD$V_car.3l[is.na(TD$V_car.3l)] <- 0
  
  TD$Cost.car.2l <- TD[,paste("Cost.car", h-1, "_20", sep = "")]
  TD$Time.car.2l <- TD[,paste("Time.car", h-1, "_20", sep = "")]*TD$time.car_var
  TD$V_car.2l <- with(TD,
                      car.2l_c.m +
                      cost.m*Cost.car.2l +
                      cost.I1.m*Cost.car.2l*I1 +
                      cost.I2.m*Cost.car.2l*I2 +
                      time.m*Time.car.2l/60 +
                      car.2l_age1.m*age.30_49 +
                      car.2l_age2.m*age.50_99 +
                      car.2l_f.m*female)
  TD$V_car.2l[is.na(TD$V_car.2l)] <- 0
  
  TD$Cost.car.1l <- TD[,paste("Cost.car", h-1, "_40", sep = "")]
  TD$Time.car.1l <- TD[,paste("Time.car", h-1, "_40", sep = "")]*TD$time.car_var
  TD$V_car.1l <- with(TD,
                      car.1l_c.m +
                      cost.m*Cost.car.1l +
                      cost.I1.m*Cost.car.1l*I1 +
                      cost.I2.m*Cost.car.1l*I2 +
                      time.m*Time.car.1l/60 +
                      car.1l_age1.m*age.30_49 +
                      car.1l_age2.m*age.50_99 +
                      car.1l_f.m*female)
  TD$V_car.1l[is.na(TD$V_car.1l)] <- 0
  
  
  
  
  
  
  
  
  
  
  TD$Cost.car.0l <- TD[,paste("Cost.car", h, "_00", sep = "")]
  TD$Time.car.0l <- TD[,paste("Time.car", h, "_00", sep = "")]*TD$time.car_var
  TD$V_car.0l <- with(TD,
                      car.0l_c.m +
                      cost.m*Cost.car.0l +
                      cost.I1.m*Cost.car.0l*I1 +
                      cost.I2.m*Cost.car.0l*I2 +
                      time.m*Time.car.0l/60 +
                      car.0l_age1.m*age.30_49 +
                      car.0l_age2.m*age.50_99 +
                      car.0l_f.m*female)
  TD$V_car.0l[is.na(TD$V_car.0l)] <- 0
  
  
  TD$Cost.car.1m <- TD[,paste("Cost.car", h, "_20", sep = "")]
  TD$Time.car.1m <- TD[,paste("Time.car", h, "_20", sep = "")]*TD$time.car_var
  TD$V_car.1m <- with(TD,
                      car.1m_c.m +
                      cost.m*Cost.car.1m +
                      cost.I1.m*Cost.car.1m*I1 +
                      cost.I2.m*Cost.car.1m*I2 +
                      time.m*Time.car.1m/60 +
                      car.1m_age1.m*age.30_49 +
                      car.1m_age2.m*age.50_99 +
                      car.1m_f.m*female)
  TD$V_car.1m[is.na(TD$V_car.1m)] <- 0
  
  TD$Cost.car.2m <- TD[,paste("Cost.car", h, "_40", sep = "")]
  TD$Time.car.2m <- TD[,paste("Time.car", h, "_40", sep = "")]*TD$time.car_var
  TD$V_car.2m <- with(TD,
                      car.2m_c.m +
                      cost.m*Cost.car.2m +
                      cost.I1.m*Cost.car.2m*I1 +
                      cost.I2.m*Cost.car.2m*I2 +
                      time.m*Time.car.2m/60 +
                      car.2m_age1.m*age.30_49 +
                      car.2m_age2.m*age.50_99 +
                      car.2m_f.m*female)
  TD$V_car.2m[is.na(TD$V_car.2m)] <- 0
  
  
  
  
  
  
  
  
  
  TD$Cost.car.3m <- TD[,paste("Cost.car", h+1, "_00", sep = "")]
  TD$Time.car.3m <- TD[,paste("Time.car", h+1, "_00", sep = "")]*TD$time.car_var
  TD$V_car.3m <- with(TD,
                      car.3m_c.m +
                      cost.m*Cost.car.3m +
                      cost.I1.m*Cost.car.3m*I1 +
                      cost.I2.m*Cost.car.3m*I2 +
                      time.m*Time.car.3m/60 +
                      car.3m_age1.m*age.30_49 +
                      car.3m_age2.m*age.50_99 +
                      car.3m_f.m*female)
  TD$V_car.3m[is.na(TD$V_car.3m)] <- 0
  
  TD$Cost.car.4m <- TD[,paste("Cost.car", h+1, "_20", sep = "")]
  TD$Time.car.4m <- TD[,paste("Time.car", h+1, "_20", sep = "")]*TD$time.car_var
  TD$V_car.4m <- with(TD,
                      car.4m_c.m +
                      cost.m*Cost.car.4m +
                      cost.I1.m*Cost.car.4m*I1 +
                      cost.I2.m*Cost.car.4m*I2 +
                      time.m*Time.car.4m/60 +
                      car.4m_age1.m*age.30_49 +
                      car.4m_age2.m*age.50_99 +
                      car.4m_f.m*female)
  TD$V_car.4m[is.na(TD$V_car.4m)] <- 0
  
  TD$Cost.car.5m <- TD[,paste("Cost.car", h+1, "_40", sep = "")]
  TD$Time.car.5m <- TD[,paste("Time.car", h+1, "_40", sep = "")]*TD$time.car_var
  TD$V_car.5m <- with(TD,
                      car.5m_c.m +
                      cost.m*Cost.car.5m +
                      cost.I1.m*Cost.car.5m*I1 +
                      cost.I2.m*Cost.car.5m*I2 +
                      time.m*Time.car.5m/60 +
                      car.5m_age1.m*age.30_49 +
                      car.5m_age2.m*age.50_99 +
                      car.5m_f.m*female)
  TD$V_car.5m[is.na(TD$V_car.5m)] <- 0
  
  
  
# cs ------------------------------------------------------------------------------------------
  
    CS_week <- paste("CS_", w[i], sep ="")
    TD[,CS_week] <- ((log(exp(TD[,"V_pub"])+
                          exp(TD[,"V_walk"])+
                          exp(TD[,"V_car.6l"])+
                          exp(TD[,"V_car.5l"])+
                          exp(TD[,"V_car.4l"])+
                          exp(TD[,"V_car.3l"])+
                          exp(TD[,"V_car.2l"])+
                          exp(TD[,"V_car.1l"])+
                          exp(TD[,"V_car.0l"])+
                          exp(TD[,"V_car.1m"])+
                          exp(TD[,"V_car.2m"])+
                          exp(TD[,"V_car.3m"])+
                          exp(TD[,"V_car.4m"])+
                          exp(TD[,"V_car.5m"])))/(TD$MUM))*TD$FE_VIA
                             
                             
    }
  
  # remove obs with NAs
  for (i in 1:length(w)){
  CSW <- paste("CS_",w[i],sep="")
  TD <- subset(TD, (!is.na(TD[,CSW])))
  }
  cs_weeks <- c(paste("CS_", w, sep =""))
  TDH <- TD[,c("ID_ORDEM", cs_weeks)]
  
  a <- 
  write.csv(TDH, paste(out.path, "hour - ", h, ".csv", sep = ""), row.names=FALSE)
}

# aggregate all trips in a single file ------------------------------------------------------------
  
  files <- list.files(path = cs_path, full.names = T)
  CS <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
  rm(files)
  write.csv(TDH, out.path1, row.names=FALSE)
  
# weekly aggregates --------------------------------------------------------------
  week.agg <- matrix(nrow = length(w), ncol = 2)
  colnames(week.agg) <- c("week",
                         "welfare")
  for (i in 1:length(w)){
   week.agg[i,1] <- w[i]
   week.agg[i,2] <- sum(CS[,paste("CS_", w[i], sep="")]) - sum(CS[,paste("CS_", w[1], sep="")])
    
  }
  
  write.csv(week.agg, out.path2, row.names=FALSE)