#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Assign vot estimate to each trip                                                       |
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
    hous.survey_path <- generatePath("stores/household survey/Sao Paulo 2012/Mobilidade_2012_v2.csv")

    # Hours evaluated
    initial.h <- 5
    final.h <- 22
    
    # required packages
    packages <- c("plyr")
    # install and load packages
    lapply(packages, pkgTest)
    
    
	 # Outputs files:
  # csv table
		out.path <- generatePath("intermediate/vot - b/extended-crawler trips/vot_by_trip.csv")

# read data -----------------------------------------------------------------------------------

  # read VOT estimates
  VOT <- read.csv(vot.results)

  # read data from original household survey
  HH12 <- read.csv(hous.survey_path)

  # subset of trips
  TD <- subset(HH12, TIPOVG >= 0)

  # Household income per capita
  TD$HH.IpC <- TD$RENDA_FA/TD$NO_MORAF
  # create income bins
  TD$Income.bin <- ifelse(TD$HH.IpC <= 500, 0,
                   ifelse(TD$HH.IpC <= 1500 & TD$HH.IpC > 500, 1,
                   ifelse(TD$HH.IpC > 1500, 2, NA)))

  # Departure time
  TD$hour <- ifelse(TD$MIN_SAIDA > 30,
                    TD$H_SAIDA + 1,
                    TD$H_SAIDA)
  TD$hour <- ifelse(TD$hour > final.h | TD$hour < initial.h, final.h, TD$hour)

  # Income_Departure.Time ID
  TD$Inc.DepT_ID <- paste(TD$Income.bin, TD$hour, sep = " ")

  # subset income bins
  TD0 <- subset(TD, Income.bin == 0)
  VOT0 <- VOT[,c("central.hour", "vot.I0.m", "vot.I0.sd.m")]
  TD0 <- merge(TD0, VOT0, by.x = "hour", by.y = "central.hour")
  TD0 <- rename(TD0, c(vot.I0.m="vot"))
  TD0 <- rename(TD0, c(vot.I0.sd.m="vot.se"))

  TD1 <- subset(TD, Income.bin == 1)
  VOT1 <- VOT[,c("central.hour", "vot.I1.m", "vot.I1.sd.m" )]
  TD1 <- merge(TD1, VOT1, by.x = "hour", by.y = "central.hour")
  TD1 <- rename(TD1, c(vot.I1.m="vot"))
  TD1 <- rename(TD1, c(vot.I1.sd.m="vot.se"))

  TD2 <- subset(TD, Income.bin == 2)
  VOT2 <- VOT[,c("central.hour", "vot.I2.m", "vot.I2.sd.m")]
  TD2 <- merge(TD2, VOT2, by.x = "hour", by.y = "central.hour")
  TD2 <- rename(TD2, c(vot.I2.m="vot"))
  TD2 <- rename(TD2, c(vot.I2.sd.m="vot.se"))

  TD <- rbind(TD0, TD1, TD2)

  TD <- TD[,c("ID_ORDEM", "vot", "vot.se")]
  write.csv(TD, out.path, row.names=FALSE)

