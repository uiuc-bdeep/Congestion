#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Simulate a fare hike - 3 -> 3.50                                                       |
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
    ext_craw_trips <- generatePath("intermediate/vot - b/welfare/welfare_by_trip - all.csv")
  
    # USD / BRL
    exch <- 2.048298
    
    # required packages
    packages <- c("ggplot2", "gtable", "grid", "ggthemes")
    # install and load packages
    lapply(packages, pkgTest)
	   
  # Outputs files:
    out.path <- generatePath("intermediate/vot - b/welfare/simulation/fare hike 1.csv")
    out.path2 <- generatePath("views/figures/vot/simulation/mode demand by fare.png")
    out.path3 <- generatePath("views/figures/vot/simulation/CS and Revenue by fare.png")
	
    out.path2.1 <- generatePath("views/figures/vot/simulation/mode demand by fare (3-3.5).png")
    out.path3.1 <- generatePath("views/figures/vot/simulation/CS and Revenue by fare (3-3.5).png")
	  	
# read data -----------------------------------------------------------------------------------
  # read VOT estimates
  TD <- read.csv(ext_craw_trips)
  ID <- subset(TD, !duplicated(TD$trip_id))

  InD <- subset(TD, !duplicated(TD$ID_PESS))
  Tot.Pop <- sum(InD$FE_VIA)
  
  # calculate V1 (simulated utility) --------------------------------------------------------------
  # baseline V
  
  # mode use
  ID$car.trip <- ifelse(ID$TIPOVG == 2,1,0)
  ID$pub.trip <- ifelse(ID$TIPOVG == 1,1,0)
  ID$walk.trip <- ifelse(ID$TIPOVG == 3,1,0)
  
  Y.car0 <- sum(ID$car.trip*ID$FE_VIA)*5
  Y.pub0 <- sum(ID$pub.trip*ID$FE_VIA)*5
  Y.walk0 <- sum(ID$walk.trip*ID$FE_VIA)*5
  CS0 <- sum(ID$CS)
  rev0 <- Y.pub0*3
  
  ID$Prob.denom <- exp(ID[,"V_pub"])+
                   exp(ID[,"V_walk"])+
                   exp(ID[,"V_car.6l"])+
                   exp(ID[,"V_car.5l"])+
                   exp(ID[,"V_car.4l"])+
                   exp(ID[,"V_car.3l"])+
                   exp(ID[,"V_car.2l"])+
                   exp(ID[,"V_car.1l"])+
                   exp(ID[,"V_car.0l"])+
                   exp(ID[,"V_car.1m"])+
                   exp(ID[,"V_car.2m"])+
                   exp(ID[,"V_car.3m"])+
                   exp(ID[,"V_car.4m"])+
                   exp(ID[,"V_car.5m"])
  
  ID$Prob.pub <- (exp(ID[,"V_pub"])/ID$Prob.denom)*ID$FE_VIA*5
  ID$Prob.walk <- (exp(ID[,"V_walk"])/ID$Prob.denom)*ID$FE_VIA*5
  ID$Prob.car <- (ID$FE_VIA*5 - ID$Prob.pub - ID$Prob.walk)
  
  
  res <- as.data.frame(matrix(nrow = 0, ncol = 7))
  colnames(res) <- c("fare", "Y.pub", "Y.car", "Y.walk", "CS", "rev", "diff")
  
  for(i in seq(0,6,0.01)){
    #i<-0  
    ID$Cost.pub_1 <- i
  
    ID$V_pub_1 <- with(ID,
                       pub_c.m +
                       cost.m*Cost.pub_1 +
                       cost.I1.m*Cost.pub_1*I1 +
                       cost.I2.m*Cost.pub_1*I2 +
                       time.m*Time.pub/60 +
                       pub_age1.m*age.30_49 +
                       pub_age2.m*age.50_99 +
                       pub_f.m*female)
  
    ID$Prob.denom_1 <- ID$Prob.denom - exp(ID[,"V_pub"]) + exp(ID[,"V_pub_1"])
    ID$CS_1 <- (log(ID$Prob.denom_1)/ID$MUM)*ID$FE_VIA*5
    # Mode use
    ID$Prob.pub_1 <- (exp(ID[,"V_pub_1"])/ID$Prob.denom_1)*ID$FE_VIA*5
    ID$Prob.walk_1 <- (exp(ID[,"V_walk"])/ID$Prob.denom_1)*ID$FE_VIA*5
    ID$Prob.car_1 <- (ID$FE_VIA*5 - ID$Prob.pub_1 - ID$Prob.walk_1)
  
    res_1 <- as.data.frame(matrix(nrow = 1, ncol = 7))
    colnames(res_1) <- c("fare", "Y.pub", "Y.car", "Y.walk", "CS", "rev", "diff")
  
    res_1$fare[1] <- i
    res_1$Y.pub[1] <- Y.pub0*(1+(sum(ID$Prob.pub_1) - sum(ID$Prob.pub))/sum(ID$Prob.pub))
    res_1$Y.car[1] <- Y.car0*(1+(sum(ID$Prob.car_1) - sum(ID$Prob.car))/sum(ID$Prob.car))
    res_1$Y.walk[1] <- Y.walk0*(1+(sum(ID$Prob.walk_1) - sum(ID$Prob.walk))/sum(ID$Prob.walk))
    res_1$CS[1] <- (sum(ID$CS_1) - CS0)/(exch*Tot.Pop/52)
    res_1$rev[1] <- ((res_1$Y.pub*res_1$fare) - rev0)/(exch*Tot.Pop/52)
    res_1$diff[1] <- (res_1$CS + res_1$rev)
  
  
    res <- rbind(res, res_1)
  }
  
  
  write.csv(res, out.path, row.names=FALSE)
  
  res.g <- res
  res.g <- res.g/1000000
  res.g$fare <- res.g$fare*1000000
  scaleFUN <- function(x) sprintf("%.2f", x)
  
  
  
  ggplot(data = res.g) +
   geom_line(aes(x = fare, y = Y.pub, colour = "public transit"), size=1) +
   geom_line(aes(x = fare, y = Y.car, colour = "private vehicle"), size=1) +
   geom_line(aes(x = fare, y = Y.walk, colour = "walking"), size=1) +
   scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20),
                      expand = c(0,0)) +
   scale_x_continuous(limits = c(0,6), breaks = seq(0,6,1),
                      expand = c(0,0), labels=scaleFUN) +
   labs(x = "\n transit fare \n (R$)", y = "Trips per week \n (million) \n",
        title = "Weekly Mode Demand by Transit Fare \n") +
   scale_color_manual(values =c("public transit" = "red",
                                "private vehicle" = "blue",
                                "walking" = "darkolivegreen4"),
                      name = "Mode") +
   geom_vline(xintercept = 3, linetype = "longdash", alpha = 0.5) +
   theme_minimal()
 ggsave(file=out.path2, width = 8, height = 5)
  
 
 
ggplot(data = res.g) +
   geom_line(aes(x = fare, y = rev, colour = " transit revenue change"), size=1) +
   geom_line(aes(x = fare, y = CS, colour = " consumer surplus change"), size=1) +
   geom_line(aes(x = fare, y = diff, colour = "total welfare change"), size=1) +
   scale_y_continuous(limits = c(-600,600), breaks = seq(-600,600,200),
                      expand = c(0,0)) +
   scale_x_continuous(limits = c(0,6), breaks = seq(0,6,1),
                      expand = c(0,0), labels=scaleFUN) +
   labs(x = "\n transit fare \n (R$)", y = "U$/year per capita \n",
        title = "Welfare Variation with Alternative Transit Fares \n (U$/year per capita) \n") +
   scale_color_manual(values = c(" transit revenue change" = "blue",
                                 " consumer surplus change" = "red",
                                 "total welfare change" = "gray"),
                      name = "Legend") +
   geom_vline(xintercept = 3, linetype = "longdash", alpha = 0.5) +
   theme_minimal() 
  ggsave(file=out.path3, width = 8, height = 5)
 
 
 