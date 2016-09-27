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
    #   extended crawler output from script 1.0
    #     "intermediate/vot/intermediate store/Extended Crawler_all.csv"
    #   household survey
    #     "stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv"

	source("environment.R")

  # Outputs files:
    # csv table
		out.path <- generatePath("intermediate/vot/intermediate store/extended crawler - vot.csv")

    # tex table
		out.path2 <- generatePath("views/tables/vot_extended crawler.tex")

  # Input values ------------------------------------------------------------------------------
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


# Required packages -----------------------------------------------------------
  packages <- c("stargazer",
                "ggplot2",
                "mlogit",
                "data.table",
                "gmnl")

# function to verify packages and install missing packages -------------------
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


# read and manipulatye data from original household survey ------------------------------------
  HH12 <- read.csv(generatePath("stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv"))

  # keep only relevant variables
  hh.vars <- c("ID_ORDEM", "TIPOVG", "MOTIVO_O", "MOTIVO_D", "NO_MORAF",
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
  DF.o <- read.csv(generatePath("intermediate/vot/intermediate store/Extended Crawler_all.csv"))


# run model -----------------------------------------------------------------

  # start output matrix
  results <- matrix(nrow =0, ncol = 18)
  colnames(results) <- c("central.hour",
                         "vot.I0", "vot.I1","vot.I2",
                         "time.m",
                         "cost.m", "cost.I1.m", "cost.I2.m",
                         "time.sd.m",
                         "cost.sd.m", "cost.I1.sd.m", "cost.I2.sd.m",
                         "vot.I0.m", "vot.I1.m", "vot.I2.m",
                         "vot.I0.sd.m", "vot.I1.sd.m", "vot.I2.sd.m")

  # list of regression outputs
    list.mml <- list()
    list.labels <- list()

  # start loop over different hours of the day
  for (i in initial.h:final.h){
    # i <- 6
    n <- 1+i-initial.h

    # create dataset that will be manipulated
    DF.ec <- DF.o

    m.lab <- c("_00","_20","_40")

    # keep only relevant variables
    ec.vars <- c("trip_id",	"timestamp_day",	"timestamp_hours",
                 "timestamp_minutes",	"walking_time",	"biking_time",
                 "public_transit_time", "Time.car", "Distance.car")

    # create Time.car labels
    ec.car_vars <- ""
    for (h in (i-2):(i+1)){
      for (m in 1:3){
        ec.car_vars <- c(ec.car_vars, paste("Time.car", h, m.lab[m], sep =""))
      }
    }
    ec.car_vars <- ec.car_vars[2:length(ec.car_vars)]

    # create Cost.car labels
    ec.car_vars_cost <- ""
    for (h in (i-2):(i+1)){
      for (m in 1:3){
        ec.car_vars_cost <- c(ec.car_vars_cost, paste("Cost.car", h, m.lab[m], sep =""))
      }
    }
    ec.car_vars_cost <- ec.car_vars_cost[2:length(ec.car_vars_cost)]

    DF.ec <- DF.ec[,c(ec.vars,ec.car_vars)]
    rm(ec.vars)

    #substitute errors and 0s with NA
    DF.ec$walking_time[DF.ec$walking_time == -1 |
                       DF.ec$walking_time == 0] <- NA

    DF.ec$public_transit_time[DF.ec$public_transit_time == -1 |
                              DF.ec$public_transit_time == 0] <- NA

    DF.car <- DF.ec[,ec.car_vars]
    DF.car[DF.car == -1 | DF.car == 0] <- NA
    DF.ec[,ec.car_vars] <- DF.car

    # take average of observed times
    DF.ec$walking_time_ave <- ave(DF.ec$walking_time, DF.ec$trip_id,
                                  FUN=function(x) mean(x, na.rm=T))

    DF.ec$public_transit_time_ave <- ave(DF.ec$public_transit_time, DF.ec$trip_id,
                                         FUN=function(x) mean(x, na.rm=T))

    for (h in (i-2):(i+1)){
      for (m in 1:3){
        DF.ec[,paste("time", h, m.lab[m], "_ave", sep ="")] <- ave(DF.ec[,paste("Time.car", h , m.lab[m], sep ="")],
                                                                   DF.ec$trip_id,
                                                                   FUN=function(x) mean(x, na.rm=T))
      }
    }
    DF.ec$car_time_ave <- ave(DF.ec$Time.car, DF.ec$trip_id,
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

    # mode specific "travel time" for public transit and walking  (from seconds to minutes)
    DF$Time.pub <- (DF$public_transit_time_ave)/60
    DF$Time.walk <- (DF$walking_time_ave)/60
    DF$Time.car <- (DF$car_time_ave)/60

    for (h in (i-2):(i+1)){
      DF[,paste("Time.car",h,"_00", sep ="")] <- DF[,paste("time",h,"_00_ave", sep ="")]/60
      DF[,paste("Time.car",h,"_20", sep ="")] <- DF[,paste("time",h,"_20_ave", sep ="")]/60
      DF[,paste("Time.car",h,"_40", sep ="")] <- DF[,paste("time",h,"_40_ave", sep ="")]/60
    }


    # mode specific "cost" time for public transit and walking  (R$)
    DF$Cost.pub <- transit.fare
    DF$Cost.walk <- walking.cost

    for (h in (i-2):(i+1)){
      DF[,paste("Cost.car",h,"_00", sep ="")] <- 5 + (DF$Distance.car/1000)*CarKmCost
      DF[,paste("Cost.car",h,"_20", sep ="")] <- 5 + (DF$Distance.car/1000)*CarKmCost
      DF[,paste("Cost.car",h,"_40", sep ="")] <- 5 + (DF$Distance.car/1000)*CarKmCost
    }
    DF$Cost.car <- 5 + (DF$Distance.car/1000)*CarKmCost


    # Creates subset dataframe
    DS <- DF

    # variables required for discrete choice estimation
    vars <- c("MOTIVO_O", "timestamp_hours",
              "age.0_29",	"age.30_49",	"age.50_99",
              "I0", "I1", "I2",
              "female", "HH.IpC",
              "Distance.car",
              "vehicles", "mode",
              "Choice", "DURACAO",
              "Time.car", "Cost.car",
              "Time.pub", "Cost.pub",
              "Time.walk", "Cost.walk")
    DS <- DS[,c(vars, ec.car_vars, ec.car_vars_cost)]

    # Additional Subseting exclusions
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

    # Logit Estimation -----------------------------------------------------------------
      #  set logit wide dataframe
      LD <- mlogit.data(DS, shape = "wide", varying = 18:45,
                        choice = "Choice", sep = ".")

      # convert time from minute to hours
      LD$Time <- (LD$Time)/60

      # Interaction terms Cost_Income
      LD$Cost_I1 <- (LD$Cost)*(LD$I1)
      LD$Cost_I2 <- (LD$Cost)*(LD$I2)

      #  logit formula
      f1 <- mFormula(Choice ~ Cost + Cost_I1 + Cost_I2 +
                              Time |
                              age.30_49 + age.50_99 + female)
      #  Multinomial Logit --------------------------------------------------------------------
      ml.Sim <- mlogit(f1, LD, reflevel="walk",
                       shape = "long", chid.var = "chid",
                       alt.var="alt", choice = "Choice")
      # save coeficients
      coefs <- as.data.frame(summary(ml.Sim)$CoefTable)

      # vot by income group
      vot.I0 <- (coefs["Time",1])/(coefs["Cost",1])
      vot.I1 <- (coefs["Time",1])/(coefs["Cost",1] + coefs["Cost_I1",1])
      vot.I2 <- (coefs["Time",1])/(coefs["Cost",1] + coefs["Cost_I2",1])

    # Mixed Logit -----------------------------------------------------------------------------
      mml.Sim <- mlogit(f1, LD, reflevel="walk",
                        rpar = c(Time = "n"),
                        R = 100, halton = NA,
                        shape = "long", chid.var = "chid",
                        alt.var="alt", choice = "Choice")

      # save regression output
      list.mml[[i-initial.h+1]] <- mml.Sim
      list.labels[[i-initial.h+1]] <- paste(i,":00", sep ="")


      # save regression coefficients
      coefs.m <- as.data.frame(summary(mml.Sim)$CoefTable)

      time.m <- coefs.m["Time",1]
      cost.m <- coefs.m["Cost",1]
      cost.I1.m <- coefs.m["Cost_I1",1]
      cost.I2.m <- coefs.m["Cost_I2",1]

      time.sd.m <- coefs.m["Time",2]
      cost.sd.m <- coefs.m["Cost",2]
      cost.I1.sd.m <- coefs.m["Cost_I1",2]
      cost.I2.sd.m <- coefs.m["Cost_I2",2]

      # Calculate VOTs
      vot.I0.m <- coefs.m["Time",1]/coefs.m["Cost",1]
      vot.I1.m <- coefs.m["Time",1]/(coefs.m["Cost",1] + coefs.m["Cost_I1",1])
      vot.I2.m <- coefs.m["Time",1]/(coefs.m["Cost",1] + coefs.m["Cost_I2",1])

      vcov <- vcov(mml.Sim)

      a <- vcov["Time","Time"]/(coefs.m["Time",1]*coefs.m["Time",1])
      b <- vcov["Cost","Cost"]/(coefs.m["Cost",1]*coefs.m["Cost",1])
      c <- vcov["Time","Cost"]/(coefs.m["Cost",1]*coefs.m["Time",1])
      d <- (coefs.m["Time",1]/coefs.m["Cost",1])^2
      vot.I0.sd.m <- sqrt((a+b-(2*c))*d)

      a <- vcov["Time","Time"]/(coefs.m["Time",1]*coefs.m["Time",1])
      b <- (vcov["Cost","Cost"] + vcov["Cost_I1","Cost_I1"] + 2*vcov["Cost","Cost_I1"])
      c <- (coefs.m["Cost",1]^2) + (coefs.m["Cost_I1",1]^2) + 2*(coefs.m["Cost_I1",1])*(coefs.m["Cost",1])
      d <- vcov["Time","Cost"] + vcov["Time","Cost_I1"]
      e <- (coefs.m["Time",1])*(coefs.m["Cost",1])+(coefs.m["Time",1])*(coefs.m["Cost_I1",1])
      f <- (coefs.m["Time",1]/(coefs.m["Cost",1] + coefs.m["Cost_I1",1]))^2
      vot.I1.sd.m <- sqrt((a+(b/c)-2*(d/e))*f)


      a <- vcov["Time","Time"]/(coefs.m["Time",1]*coefs.m["Time",1])
      b <- (vcov["Cost","Cost"] + vcov["Cost_I2","Cost_I2"] + 2*vcov["Cost","Cost_I2"])
      c <- (coefs.m["Cost",1]^2) + (coefs.m["Cost_I2",1]^2) + 2*(coefs.m["Cost_I2",1])*(coefs.m["Cost",1])
      d <- vcov["Time","Cost"] + vcov["Time","Cost_I2"]
      e <- (coefs.m["Time",1])*(coefs.m["Cost",1])+(coefs.m["Time",1])*(coefs.m["Cost_I2",1])
      f <- (coefs.m["Time",1]/(coefs.m["Cost",1] + coefs.m["Cost_I2",1]))^2
      vot.I2.sd.m <- sqrt((a+(b/c)-2*(d/e))*f)


      # save hour
      central.hour <- i

      hour.results <- c(central.hour,
                        vot.I0, vot.I1, vot.I2,
                        time.m,
                        cost.m, cost.I1.m, cost.I2.m,
                        time.sd.m,
                        cost.sd.m, cost.I1.sd.m, cost.I2.sd.m,
                        vot.I0.m, vot.I1.m, vot.I2.m,
                        vot.I0.sd.m, vot.I1.sd.m, vot.I2.sd.m)

      results <- rbind(results, hour.results)
  }



  write.csv(results, out.path, row.names=FALSE)


  labels = as.character(list.labels)

  stargazer(list.mml,
            keep = c("Time", "Cost", "Cost_I1", "Cost_I2"),
            out = out.path2,
            title = "VOT Coefficients from Mixed Logit Estimation",
            column.labels = labels,
            model.numbers = FALSE)



