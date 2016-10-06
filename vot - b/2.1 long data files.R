#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Creates Long Data files for Logit Estimations                                          |
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
    ext.crawler_path <- generatePath("intermediate/vot - b/extended-crawler trips/extended crawler - all.csv")
    hous.survey_path <- generatePath("stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv")

    # Hours evaluated
    initial.h <- 6
    final.h <- 22

    # Mode costs
    transit.fare <- 3   # baseline transit fare
    walking.cost <- 0   # monetary cost of walking

    # monetary cost per kilometer for cars
    #   source: Basso et al (2011): http://www.sciencedirect.com/science/article/pii/S0967070X1100014X
    #   exchange rate: http://usd.lookly.com/Average-Analytics/BRL/
    CarKmCost <- 0.3804*1.952

    # Required packages
    packages <- c("stargazer",
                  "ggplot2",
                  "mlogit",
                  "data.table",
                  "gmnl",
                  "plyr")

    # install and load packages
    lapply(packages, pkgTest)
    
  # Outputs files:
    # csv tables
		  out.path <- generatePath("intermediate/vot - b/extended-crawler trips/long data/")
		  out.path2 <- generatePath("intermediate/vot - b/extended-crawler trips/wide data/")
		  
# read and manipulate data from original household survey ------------------------------------
  HH12 <- read.csv(hous.survey_path)

  # keep only relevant variables
  hh.vars <- c("ID_ORDEM", "TIPOVG", "FE_VIA", "MOTIVO_D", "NO_MORAF",
               "QT_MOTO", "QT_AUTO", "RENDA_FA", "IDADE", "SEXO", "DURACAO")
  HH12 <- HH12[hh.vars]
  rm(hh.vars)

  # creates "mode" categorical variable
  HH12$mode <- with(HH12, ifelse(TIPOVG == 1, "pub",
                          ifelse(TIPOVG == 2, "car",
                          ifelse(TIPOVG == 3, "walk",
                          ifelse(TIPOVG == 4, "bike", NA)))))

  # create age bins
  HH12$age.0_29 <- ifelse(HH12$IDADE < 30, 1,0)
  HH12$age.30_49 <- ifelse(HH12$IDADE < 50 & HH12$IDADE >= 30, 1, 0)
  HH12$age.50_99 <- ifelse(HH12$IDADE >= 50, 1, 0)

  # Household income per capita
  HH12$HH.IpC <- HH12$RENDA_FA/HH12$NO_MORAF

  # create income bins
  HH12$I0 <- ifelse(HH12$HH.IpC <= 500, 1,0)
  HH12$I1 <- ifelse(HH12$HH.IpC <= 1500 & HH12$HH.IpC > 500, 1, 0)
  HH12$I2 <- ifelse(HH12$HH.IpC > 1500, 1, 0)

  # creates female dummy
  HH12$female <- HH12$SEXO - 1

  # Substitute NA vehicle quantities with 0s
    # motorcycles
    HH12$QT_MOTO[is.na(HH12$QT_MOTO)] <- 0
    # cars
    HH12$QT_AUTO[is.na(HH12$QT_AUTO)] <- 0
    # vehicles = motorcycles + cars
    HH12$vehicles <- HH12$QT_MOTO + HH12$QT_AUTO

# read extended crawler -----------------------------------------------------------------------
  DF.o <- read.csv(ext.crawler_path)


# start loop over different hours of the day
  for (i in initial.h:final.h){
    # i <- 6
    n <- 1+i-initial.h

    # create dataset that will be manipulated
    DF.ec <- DF.o

    # keep only relevant variables
    ec.vars <- c("trip_id",	"timestamp_day",	"timestamp_hours",
                 "timestamp_minutes",	"walking_time",
                 "public_transit_time", "Time.car", "Distance.car")
    
    m.lab <- c("_00","_20","_40")
    
    # create Time.car and Cost.car labels
    ec.car_vars_time <- ""
    for (h in (i-2):(i+1)){
      for (m in 1:3){
        ec.car_vars_time <- c(ec.car_vars_time, paste("Time.car", h, m.lab[m], sep =""))
      }
    }
    ec.car_vars_time <- ec.car_vars_time[2:length(ec.car_vars_time)]

    ec.car_vars_cost <- ""
    for (h in (i-2):(i+1)){
      for (m in 1:3){
        ec.car_vars_cost <- c(ec.car_vars_cost, paste("Cost.car", h, m.lab[m], sep =""))
      }
    }
    ec.car_vars_cost <- ec.car_vars_cost[2:length(ec.car_vars_cost)]
    
    
    # Calculate times
    DF.ec <- DF.ec[,c(ec.vars,ec.car_vars_time)]
    
    # substitute errors and 0s with NA
    DF.ec$walking_time[DF.ec$walking_time <= 0] <- NA
    DF.ec$public_transit_time[DF.ec$public_transit_time <= 0] <- NA
    DF.ec$Time.car[DF.ec$Time.car <= 0] <- NA
    
    DF.car <- DF.ec[,ec.car_vars_time]
    DF.car[DF.car <= 0] <- NA
    DF.ec[,ec.car_vars_time] <- DF.car
    
    # take average of observed times
    DF.ec$walking_time_ave <- ave(DF.ec$walking_time, DF.ec$trip_id,
                                  FUN=function(x) mean(x, na.rm=T))
    DF.ec$public_transit_time_ave <- ave(DF.ec$public_transit_time, DF.ec$trip_id,
                                         FUN=function(x) mean(x, na.rm=T))
    DF.ec$car_time_ave <- ave(DF.ec$Time.car, DF.ec$trip_id,
                              FUN=function(x) mean(x, na.rm=T))
    for (h in (i-2):(i+1)){
      for (m in 1:3){
        DF.ec[,paste("time", h, m.lab[m], "_ave", sep ="")] <- ave(DF.ec[,paste("Time.car", h , m.lab[m], sep ="")],
                                                                   DF.ec$trip_id,
                                                                   FUN=function(x) mean(x, na.rm=T))
      }
    }
    
    DF.ec$car_dist_ave <- ave(DF.ec$Distance.car, DF.ec$trip_id,
                              FUN=function(x) mean(x, na.rm=T))
    
    # remove repeated observations
    DF.ec <- DF.ec[!duplicated(DF.ec$trip_id), ]

    # merge to original hosehold data
    DF <- merge(HH12, DF.ec, by.x = "ID_ORDEM", by.y = "trip_id", all.x = F, all.y = T)

    
 # Manipulate original data

    # create the Choice variable in the format modeH_MM
    DF$min_bin <- ifelse(DF$timestamp_minutes < 20, "_00",
                  ifelse(DF$timestamp_minutes < 40, "_20",
                  ifelse(DF$timestamp_minutes < 60, "_40")))

    DF$Choice <- ifelse(DF$mode == "car",
                        paste(DF$mode, DF$timestamp_hours, DF$min_bin, sep = ""),
                        DF$mode)
    

    # mode specific "travel time" (converts to minutes)
    DF$Time.pub <- (DF$public_transit_time_ave)/60
    DF$Time.walk <- (DF$walking_time_ave)/60
    DF$Time.car <- (DF$car_time_ave)/60
    for (h in (i-2):(i+1)){
      DF[,paste("Time.car",h,"_00", sep ="")] <- DF[,paste("time",h,"_00_ave", sep ="")]/60
      DF[,paste("Time.car",h,"_20", sep ="")] <- DF[,paste("time",h,"_20_ave", sep ="")]/60
      DF[,paste("Time.car",h,"_40", sep ="")] <- DF[,paste("time",h,"_40_ave", sep ="")]/60
    }



    # mode specific "cost"
    DF$Cost.pub <- transit.fare
    DF$Cost.walk <- walking.cost
    DF$Cost.car <- 5 + (DF$Distance.car/1000)*CarKmCost
    for (h in (i-2):(i+1)){
      DF[,paste("Cost.car",h,"_00", sep ="")] <- 5 + (DF$Distance.car/1000)*CarKmCost
      DF[,paste("Cost.car",h,"_20", sep ="")] <- 5 + (DF$Distance.car/1000)*CarKmCost
      DF[,paste("Cost.car",h,"_40", sep ="")] <- 5 + (DF$Distance.car/1000)*CarKmCost
    }
    
    
    # Creates subset dataframe
    DS <- DF

    # variables required for discrete choice estimation
    vars <- c("ID_ORDEM", "FE_VIA", "timestamp_hours", "timestamp_minutes",
              "age.0_29",	"age.30_49",	"age.50_99",
              "I0", "I1", "I2",
              "female", "HH.IpC",
              "Distance.car",
              "vehicles", "mode",
              "Choice", "DURACAO",
              "Time.car", "Cost.car",
              "Time.pub", "Cost.pub",
              "Time.walk", "Cost.walk")
    
    DS <- DS[,c(vars, ec.car_vars_time, ec.car_vars_cost)]
    DS$dep.hour <- ifelse(DS$timestamp_minutes > 30,
                          DS$timestamp_hours + 1,
                          DS$timestamp_hours)
    
    write.csv(DS, paste(out.path2, "wide data - ", i, ".csv", sep =""), row.names = F)
    
    
    # Additional Subseting
    #   travel time > 0
    DS <- subset(DS, Time.pub > 0 &
                     Time.car > 0 &
                     Time.walk > 0)


    #   travel cost > 0
    DS <- subset(DS, Cost.pub > 0 &
                     Cost.car > 0 &
                     Cost.walk == 0)

    #   remove bike trips
    DS <- subset(DS, mode != "bike")

    # calculate survey/crawler error  (minutes to minutes)
    DS$err <- ifelse(DS$mode == "pub", (DS$Time.pub - DS$DURACAO)/DS$DURACAO,
              ifelse(DS$mode == "car", (DS$Time.car - DS$DURACAO)/DS$DURACAO,
              ifelse(DS$mode == "walk", (DS$Time.walk - DS$DURACAO)/DS$DURACAO, 0)))


    # Subset where err <50%
    DS <- subset(DS, err > -0.5 & err < 0.5)

    # Subset where choice within range
    DS <- subset(DS, timestamp_hours < i+2 & timestamp_hours > i-3)

    # Set Choice variable as factor (so the missing levels are deleted)
    DS$Choice <- factor(DS$Choice)

    # Create logit wide dataframe
    LD <- mlogit.data(DS, shape = "wide", varying = 20:47,
                        choice = "Choice", sep = ".")
    
    LD <- LD[!is.nan(LD$Time),]
    # convert time from minute to hours
    LD$Time <- (LD$Time)/60

    # Interaction terms Cost_Income
    LD$Cost_I1 <- (LD$Cost)*(LD$I1)
    LD$Cost_I2 <- (LD$Cost)*(LD$I2)

    # Save Data
    write.csv(LD, paste(out.path, "Long_Data_", i, ".csv", sep =""), row.names=FALSE)
    
  }

