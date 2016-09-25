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

# Input values
  # Hours evaluated
  initial.h <- 7
  final.h <- 11

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

  # set working directory
  # setwd("//141.142.209.255/share/projects/Congestion")


  # read data from original household survey
  HH12 <- read.csv("stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv")

  # keep only relevant variables
  hh.vars <- c("ID_ORDEM", "TIPOVG", "MOTIVO_O", "MOTIVO_D", "NO_MORAF",
               "QT_MOTO", "QT_AUTO", "RENDA_FA", "IDADE", "SEXO", "DURACAO")
  HH12 <- HH12[hh.vars]
  rm(hh.vars)


# merge extended crawler --------------------------------------------------------------------
  # list all files in the folder "stream/extended-crawler"
  files <- list.files(path = "stream/extended-crawler", full.names = T)

  # merge all those files into a single data frame
  DF.o <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
  rm(files)

  # creates list of minute labels
  m.lab <- c("_00","_20","_40")

  # calculate t0 travel time by car
  DF.o$min <- ifelse(DF.o$timestamp_minutes < 20, m.lab[1],
              ifelse(DF.o$timestamp_minutes < 40, m.lab[2],
              ifelse(DF.o$timestamp_minutes < 60, m.lab[3],"")))

  DF.o$T.car_name <- paste("Time.car", DF.o$timestamp_hours, DF.o$min, sep ="")

  DF.o$Time.car <- sapply(seq_along(DF.o[[1]]), function(i)
   {
     DF.o[i, sprintf("Time.car%s%s", DF.o$timestamp_hours[i], DF.o$min[i])]
   })


  DF.o$D.car_name <- paste("Distance.car", DF.o$timestamp_hours, DF.o$min, sep ="")

  DF.o$Distance.car <- sapply(seq_along(DF.o[[1]]), function(i)
   {
     DF.o[i, sprintf("Distance.car%s%s", DF.o$timestamp_hours[i], DF.o$min[i])]
   })

# run model -----------------------------------------------------------------

  # start output variables
    # mixed logit time and cost coeficients standard deviation
    central.hour <- 0

    # multinomial logit vot by income group
    vot.I0 <- 0
    vot.I1 <- 0
    vot.I2 <- 0

    # mixed logit vot by income group
    vot.I0.m <- 0
    vot.I1.m <- 0
    vot.I2.m <- 0

    # mixed logit time and cost coeficients
    time.m <- 0
    cost.m <- 0
    cost.I1.m <- 0
    cost.I2.m <- 0

    # mixed logit time and cost coeficients standard deviation
    time.sd.m <- 0
    cost.sd.m <- 0
    cost.I1.sd.m <- 0
    cost.I2.sd.m <- 0

  # list of regression outputs
    list.mml <- list()
    list.labels <- list()


  # start loop over different hours of the day
  for (i in initial.h:final.h){

    # create dataset that will be manipulated
    DF.ec <- DF.o

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

      # car variables
      DF.car <- DF.ec[,ec.car_vars]
      DF.car[DF.car == -1 | DF.car == 0] <- NA
      DF.ec[,ec.car_vars] <- DF.car

    # take average of observed times
    DF.ec$walking_time_ave <- ave(DF.ec$walking_time, DF.ec$trip_id,
                                  FUN=function(x) mean(x, na.rm=T))

    DF.ec$public_transit_time_ave <- ave(DF.ec$public_transit_time, DF.ec$trip_id,
                                         FUN=function(x) mean(x, na.rm=T))

    #  cars
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

    # merge to original data
    DF <- merge(HH12, DF.ec, by.x = "ID_ORDEM", by.y = "trip_id", all.x = F, all.y = T)


    # Manipulate original data
    # creates "mode" categorical variable
    DF$mode <- with(DF, ifelse(TIPOVG == 1, "pub",
                        ifelse(TIPOVG == 2, "car",
                        ifelse(TIPOVG == 3, "walk",
                        ifelse(TIPOVG == 4, "bike", NA)))))

    # create age bins
    DF$age.0_29 <- ifelse(DF$IDADE < 30, 1,0)
    DF$age.30_49 <- ifelse(DF$IDADE < 50 & DF$IDADE >= 30, 1, 0)
    DF$age.50_99 <- ifelse(DF$IDADE >= 50, 1, 0)

    # Household income per capita
    DF$HH.IpC <- DF$RENDA_FA/DF$NO_MORAF
    # create income bins
    DF$Income.0_500 <- ifelse(DF$HH.IpC <= 500, 1,0)
    DF$Income.500_1500 <- ifelse(DF$HH.IpC <= 1500 & DF$HH.IpC > 500, 1, 0)
    DF$Income.1500_M <- ifelse(DF$HH.IpC > 1500, 1, 0)

    # creates female dummy
    DF$female <- DF$SEXO - 1

    # create the Choice variable in the format modeH_MM
    DF$min_bin <- ifelse(DF$timestamp_minutes < 20, "_00",
                  ifelse(DF$timestamp_minutes < 40, "_20",
                  ifelse(DF$timestamp_minutes < 60, "_40", "")))

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

    # Substitute NA vehicle quantities with 0s
      # motorcycles
      DF$QT_MOTO[is.na(DF$QT_MOTO)] <- 0
      # cars
      DF$QT_AUTO[is.na(DF$QT_AUTO)] <- 0

    # vehicles = motorcycles + cars
    DF$vehicles <- DF$QT_MOTO + DF$QT_AUTO

    # Creates subset dataframe
    DS <- DF

    # variables required for discrete choice estimation
    vars <- c("MOTIVO_O", "timestamp_hours",
              "age.0_29",	"age.30_49",	"age.50_99",
              "Income.0_500", "Income.500_1500", "Income.1500_M",
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

    # Subset for trips originated at home
    DS <- subset(DS, MOTIVO_O == 8)

    # Set Choice variable as factor (so the missing levels are deleted)
    DS$Choice <- factor(DS$Choice)

    # Logit Estimation -----------------------------------------------------------------
      #  set logit wide dataframe
      LD <- mlogit.data(DS, shape = "wide", varying = 18:45,
                        choice = "Choice", sep = ".")

      # convert time from minute to hours
      LD$Time <- (LD$Time)/60

      # Interaction terms Cost_Income
      LD$Cost_Income.500_1500 <- (LD$Cost)*(LD$Income.500_1500)
      LD$Cost_Income.1500_M <- (LD$Cost)*(LD$Income.1500_M)

      #  logit formula
      f1 <- mFormula(Choice ~ Cost + Cost_Income.500_1500 + Cost_Income.1500_M +
                              Time |
                              age.30_49 + age.50_99 + female)
      #  Multinomial Logit
      ml.Sim <- mlogit(f1, LD, reflevel="walk",
                       shape = "long", chid.var = "chid",
                       alt.var="alt", choice = "Choice")
      # save coeficients
      coefs <- as.data.frame(summary(ml.Sim)$CoefTable)

      # vot by income group
      vot.I0 <- c(vot.I0, (coefs["Time",1])/(coefs["Cost",1]))
      vot.I1 <- c(vot.I1, (coefs["Time",1])/(coefs["Cost",1] + coefs["Cost_Income.500_1500",1]))
      vot.I2 <- c(vot.I2, (coefs["Time",1])/(coefs["Cost",1] + coefs["Cost_Income.1500_M",1]))

    # Mixed Logit
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

      time.m <- c(time.m, coefs.m["Time",1])
      cost.m <- c(cost.m, coefs.m["Cost",1])
      cost.I1.m <- c(cost.I1.m,  coefs.m["Cost_Income.500_1500",1])
      cost.I2.m <- c(cost.I2.m,  coefs.m["Cost_Income.1500_M",1])

      time.sd.m <- c(time.sd.m, coefs.m["Time",2])
      cost.sd.m <- c(cost.sd.m, coefs.m["Cost",2])
      cost.I1.sd.m <- c(cost.I1.sd.m,  coefs.m["Cost_Income.500_1500",2])
      cost.I2.sd.m <- c(cost.I2.sd.m,  coefs.m["Cost_Income.1500_M",2])

      # Calculate VOTs
      vot.I0.m <- c(vot.I0.m, (coefs.m["Time",1]/coefs.m["Cost",1]))
      vot.I1.m <- c(vot.I1.m, (coefs.m["Time",1]/(coefs.m["Cost",1] + coefs.m["Cost_Income.500_1500",1])))
      vot.I2.m <- c(vot.I2.m, (coefs.m["Time",1]/(coefs.m["Cost",1] + coefs.m["Cost_Income.1500_M",1])))

      # save hour
      central.hour <- c(central.hour, i)

  }

  output <- as.data.frame(cbind(central.hour, vot.I0, vot.I1, vot.I2,
                                vot.I0.m, vot.I1.m, vot.I2.m,
                                time.m, cost.m, cost.I1.m, cost.I2.m,
                                time.sd.m, cost.sd.m, cost.I1.sd.m, cost.I2.sd.m))
  output <- output[-1,]
  write.csv(output, "analysis/output.csv")


  labels = as.character(list.labels)

	dir.create("analysis/views/tables", recursive=TRUE)
  stargazer(list.mml,
            keep = c("Time", "Cost", "Cost_Income.500_1500", "Cost_Income.1500_M"),
            out = "analysis/views/tables/vot_extended crawler.tex",
            title = "VOT Coefficients from Mixed Logit Estimation",
            column.labels = labels,
            model.numbers = FALSE)
