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
    ext.crawler <- generatePath("intermediate/vot - b/extended-crawler trips/extended crawler - all.csv")
    cs_path <- generatePath("intermediate/vot - b/welfare/welfare_by_hour/")
    hous.survey_path <- generatePath("stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv")
    dept <- generatePath("stores/auxiliary/dept.csv")
    
    
    # Hours evaluated
    initial.h <- 6
    final.h <- 22
    # Valid normal-crawler weeks
    w <- c(2,3,4,10,11,12,13)
    
    # monetary cost per kilometer for cars
    #   source: Basso et al (2011): http://www.sciencedirect.com/science/article/pii/S0967070X1100014X
    #   exchange rate: http://usd.lookly.com/Average-Analytics/BRL/
    CarKmCost <- 0.3804*1.952
    
    # required packages
    packages <- c("plyr")
    # install and load packages
    lapply(packages, pkgTest)
    
    
	 # Outputs files:
  # csv table
		out.path <- generatePath("intermediate/vot - b/welfare/welfare_by_hour/")
		out.path1 <- generatePath("intermediate/vot - b/welfare/welfare_by_trip.csv")
		out.path1.5 <- generatePath("intermediate/vot - b/welfare/welfare_by_trip - all.csv")
		out.path2 <- generatePath("intermediate/vot - b/welfare/welfare_by_week.csv")

# read data -----------------------------------------------------------------------------------
  # read VOT estimates
  VOT <- read.csv(vot.results)

		# HOusehold Survey
		HH12 <- read.csv(hous.survey_path)

  # keep only relevant variables
  hh.vars <- c("ID_ORDEM", "TIPOVG", "FE_VIA", "MOTIVO_D", "NO_MORAF",
               "QT_MOTO", "QT_AUTO", "RENDA_FA", "IDADE", "SEXO", "DURACAO",
               "ID_PESS")
  HH12 <- HH12[hh.vars]
  rm(hh.vars)
  
  HH12$FE_VIA[is.na(HH12$FE_VIA)] <- 0
  FE_week <- sum(HH12$FE_VIA)*5
  # read ext crawler
  EC <- read.csv(ext.crawler)
  
  EC <- merge(EC, HH12, by.x = "trip_id", by.y = "ID_ORDEM")
  # keep only valid crawls
  EC <- subset(EC, walking_time > 0 & biking_time > 0 & Time.car > 0)
  
  #add data -----------------------------------------------------------------------------------
  # create age bins
  EC$age.0_29 <- ifelse(EC$IDADE < 30, 1,0)
  EC$age.30_49 <- ifelse(EC$IDADE < 50 & EC$IDADE >= 30, 1, 0)
  EC$age.50_99 <- ifelse(EC$IDADE >= 50, 1, 0)

  # Household income per capita
  EC$HH.IpC <- EC$RENDA_FA/EC$NO_MORAF

  # create income bins
  EC$I0 <- ifelse(EC$HH.IpC <= 500, 1,0)
  EC$I1 <- ifelse(EC$HH.IpC <= 1500 & EC$HH.IpC > 500, 1, 0)
  EC$I2 <- ifelse(EC$HH.IpC > 1500, 1, 0)

  # creates female dummy
  EC$female <- EC$SEXO - 1

  # calculates dep.hour
  EC$dep.hour <- ifelse(EC$timestamp_minutes > 30,
                        EC$timestamp_hours + 1,
                        EC$timestamp_hours)
  
# Discrete choice results -------------------------------------------------------------------
  
  TD <- merge(EC, VOT, by.x = "dep.hour", by.y = "central.hour")
  
  # calculate marginal utility of money
  TD$MUM <- abs(TD$cost.m + TD$cost.I1.m*TD$I1 + TD$cost.I2.m*TD$I2)
  
  
# calculate V (observed utility) --------------------------------------------------------------
  # baseline V
  
  TD$Cost.walk <- 0
  TD$Time.walk <- TD$walking_time/60
  TD$V_walk <- with(TD, cost.m*Cost.walk +
                        cost.I1.m*Cost.walk*I1 +
                        cost.I2.m*Cost.walk*I2 +
                        time.m*Time.walk/60)
  
  TD$Cost.pub <- 3
  TD$Time.pub <- TD$public_transit_time/60
  TD$V_pub <- with(TD,
                   pub_c.m +
                   cost.m*Cost.pub +
                   cost.I1.m*Cost.pub*I1 +
                   cost.I2.m*Cost.pub*I2 +
                   time.m*Time.pub/60 +
                   pub_age1.m*age.30_49 +
                   pub_age2.m*age.50_99 +
                   pub_f.m*female)
  
# V ---------------------------------------------------------------------------------------
 # create the Choice variable in the format modeH_MM
  TD$min_bin <- ifelse(TD$timestamp_minutes < 20, "_00",
                ifelse(TD$timestamp_minutes < 40, "_20",
                ifelse(TD$timestamp_minutes < 60, "_40")))

  TD$dep.time <- paste(TD$timestamp_hours, TD$min_bin, sep = "")
  dept <- read.csv(dept)
 
  TD <- merge(TD, dept, by = "dep.time")
 
  for(i in 1:nrow(dept)){
  TDD <- subset(TD, dep.time == dept$dep.time[i])
  
  TDD$Distance.car.6l <- TDD[,paste("Distance.car", dept$l6l[i], sep ="")]
  TDD$Distance.car.5l <- TDD[,paste("Distance.car", dept$l5l[i], sep ="")]
  TDD$Distance.car.4l <- TDD[,paste("Distance.car", dept$l4l[i], sep ="")]
  
  TDD$Distance.car.3l <- TDD[,paste("Distance.car", dept$l3l[i], sep ="")]
  TDD$Distance.car.2l <- TDD[,paste("Distance.car", dept$l2l[i], sep ="")]
  TDD$Distance.car.1l <- TDD[,paste("Distance.car", dept$l1l[i], sep ="")]
  
  TDD$Distance.car.0l <- TDD[,paste("Distance.car", dept$l0l[i], sep ="")]
  TDD$Distance.car.1m <- TDD[,paste("Distance.car", dept$l1m[i], sep ="")]
  TDD$Distance.car.2m <- TDD[,paste("Distance.car", dept$l2m[i], sep ="")]
  
  TDD$Distance.car.3m <- TDD[,paste("Distance.car", dept$l3m[i], sep ="")]
  TDD$Distance.car.4m <- TDD[,paste("Distance.car", dept$l4m[i], sep ="")]
  TDD$Distance.car.5m <- TDD[,paste("Distance.car", dept$l5m[i], sep ="")]
  
  if(i == 1){
  FD <- TDD 
  } else {
   FD <- rbind(FD,TDD)
   }
}
  
  TD <- FD
  
  TD$Cost.car.6l <- 5 + (TD$Distance.car.6l/1000)*CarKmCost
  TD$Cost.car.5l <- 5 + (TD$Distance.car.5l/1000)*CarKmCost
  TD$Cost.car.4l <- 5 + (TD$Distance.car.4l/1000)*CarKmCost
  
  TD$Cost.car.3l <- 5 + (TD$Distance.car.3l/1000)*CarKmCost
  TD$Cost.car.2l <- 5 + (TD$Distance.car.2l/1000)*CarKmCost
  TD$Cost.car.1l <- 5 + (TD$Distance.car.1l/1000)*CarKmCost
  
  TD$Cost.car.0l <- 5 + (TD$Distance.car.0l/1000)*CarKmCost
  TD$Cost.car.1m <- 5 + (TD$Distance.car.1m/1000)*CarKmCost
  TD$Cost.car.2m <- 5 + (TD$Distance.car.2m/1000)*CarKmCost
  
  TD$Cost.car.3m <- 5 + (TD$Distance.car.3m/1000)*CarKmCost
  TD$Cost.car.4m <- 5 + (TD$Distance.car.4m/1000)*CarKmCost
  TD$Cost.car.5m <- 5 + (TD$Distance.car.5m/1000)*CarKmCost
  
  
for(i in 1:nrow(dept)){
  TDD <- subset(TD, dep.time == dept$dep.time[i])
  
  TDD$Time.car.6l <- TDD[,paste("Time.car", dept$l6l[i], sep ="")]
  TDD$Time.car.5l <- TDD[,paste("Time.car", dept$l5l[i], sep ="")]
  TDD$Time.car.4l <- TDD[,paste("Time.car", dept$l4l[i], sep ="")]
  
  TDD$Time.car.3l <- TDD[,paste("Time.car", dept$l3l[i], sep ="")]
  TDD$Time.car.2l <- TDD[,paste("Time.car", dept$l2l[i], sep ="")]
  TDD$Time.car.1l <- TDD[,paste("Time.car", dept$l1l[i], sep ="")]
  
  TDD$Time.car.0l <- TDD[,paste("Time.car", dept$l0l[i], sep ="")]
  TDD$Time.car.1m <- TDD[,paste("Time.car", dept$l1m[i], sep ="")]
  TDD$Time.car.2m <- TDD[,paste("Time.car", dept$l2m[i], sep ="")]
  
  TDD$Time.car.3m <- TDD[,paste("Time.car", dept$l3m[i], sep ="")]
  TDD$Time.car.4m <- TDD[,paste("Time.car", dept$l4m[i], sep ="")]
  TDD$Time.car.5m <- TDD[,paste("Time.car", dept$l5m[i], sep ="")]
  
  if(i == 1){
  FD <- TDD 
  } else {
   FD <- rbind(FD,TDD)
   }
}
  
  TD <- FD
  
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
  
    TD$CS <- ((log(exp(TD[,"V_pub"])+
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
                          exp(TD[,"V_car.5m"])))/(TD$MUM))*TD$FE_VIA*5
                             
  
  
  # remove obs with NAs
  TD <- subset(TD, (!is.na(TD$CS)))
  
  # Calculate time
  TD$time <- ifelse(TD$TIPOVG == 1, TD$Time.pub,
             ifelse(TD$TIPOVG == 2, TD$Time.car.0l/60,
             ifelse(TD$TIPOVG == 3, TD$Time.walk, NA)))
  TD$distance <- ifelse(TD$TIPOVG == 1, TD$public_transit_distance,
                 ifelse(TD$TIPOVG == 2, TD$Distance.car.0l,
                 ifelse(TD$TIPOVG == 3, TD$walking_distance , NA)))
  TD$distance <- TD$distance/1000
  
  TD$time <- TD$time/60
  TD$speed <- TD$distance/TD$time
  
  TD <- subset(TD, (!is.na(TD$time)))
  TD <- subset(TD, (!is.na(TD$distance)))
  TD <- subset(TD, time > 0 & distance > 0)
  
  # welfare by trip file
  TDH <- TD
  TDH$vot <- TDH$vot.I0.m*TDH$I0 + TDH$vot.I1.m*TDH$I1 + TDH$vot.I2.m*TDH$I2
  
  write.csv(TDH[,c("trip_id", "CS", "FE_VIA", "time", "speed", "HH.IpC",
                   "RENDA_FA", "vot", "ID_PESS")],
            out.path1,
            row.names=FALSE)
  
  write.csv(TDH,
            out.path1.5,
            row.names=FALSE)
  
  # aggregate by week
  TD$CS.w <- ave(TD$CS, TD$weeks, FUN = sum)
  TD$FE.w <- ave(TD$FE_VIA, TD$weeks, FUN = sum)
  TD$FE.w_factor <- TD$FE.w/(FE_week/5)
  
  TD$time.fe <- TD$time*TD$FE_VIA
  TD$time.w <- ave(TD$time.fe, TD$weeks, FUN = sum)
  
  TW <- TD[!duplicated(TD$weeks), ]
  TW <- TW[order(TW$weeks),] 
  TW <- TW[,c("weeks", "CS.w", "FE.w","FE.w_factor", "time.w")]
  
  TW$CS.w <- (TW$CS.w/TW$FE.w_factor)
  TW$time.w <- (TW$time.w/TW$FE.w_factor)
  
  write.csv(TW, out.path2, row.names=FALSE)