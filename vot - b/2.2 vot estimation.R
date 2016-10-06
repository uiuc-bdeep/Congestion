#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Runs the Discrete choice logit and mixed logit models                                  |
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
    long.data_path <- generatePath("intermediate/vot - b/extended-crawler trips/long data/")
    
    # Hours evaluated
    initial.h <- 6
    final.h <- 22

    # Required packages
    packages <- c("stargazer",
                  "ggplot2",
                  "mlogit",
                  "data.table",
                  "gmnl")

    # install and load packages
    lapply(packages, pkgTest)
    
  # Outputs files:
    # csv table
		  out.path <- generatePath("intermediate/vot - b/choice model outputs/baseline.csv")
    # tex table
		  out.path2 <- generatePath("views/tables/vot_extended crawler - b.tex")

  
# read and manipulate data from original household survey ------------------------------------

  # start output matrix
  results <- matrix(nrow =0, ncol = 70)
  colnames(results) <- c("central.hour",
                         "vot.I0", "vot.I1","vot.I2",
                         "time.m",
                         "cost.m", "cost.I1.m", "cost.I2.m",
                         "time.sd.m",
                         "cost.sd.m", "cost.I1.sd.m", "cost.I2.sd.m",
                         "vot.I0.m", "vot.I1.m", "vot.I2.m",
                         "vot.I0.sd.m", "vot.I1.sd.m", "vot.I2.sd.m",
                         
                         "car.6l_c.m", "car.5l_c.m", "car.4l_c.m",
                         "car.3l_c.m", "car.2l_c.m", "car.1l_c.m",
                         "car.0l_c.m", "car.1m_c.m", "car.2m_c.m",
                         "car.3m_c.m", "car.4m_c.m", "car.5m_c.m", "pub_c.m",
                         
                         "car.6l_age1.m", "car.5l_age1.m", "car.4l_age1.m",
                         "car.3l_age1.m", "car.2l_age1.m", "car.1l_age1.m",
                         "car.0l_age1.m", "car.1m_age1.m", "car.2m_age1.m",
                         "car.3m_age1.m", "car.4m_age1.m", "car.5m_age1.m", "pub_age1.m",
                         
                         "car.6l_age2.m", "car.5l_age2.m", "car.4l_age2.m",
                         "car.3l_age2.m", "car.2l_age2.m", "car.1l_age2.m",
                         "car.0l_age2.m", "car.1m_age2.m", "car.2m_age2.m",
                         "car.3m_age2.m", "car.4m_age2.m", "car.5m_age2.m", "pub_age2.m",
                         
                         "car.6l_f.m", "car.5l_f.m", "car.4l_f.m",
                         "car.3l_f.m", "car.2l_f.m", "car.1l_f.m",
                         "car.0l_f.m", "car.1m_f.m", "car.2m_f.m",
                         "car.3m_f.m", "car.4m_f.m", "car.5m_f.m", "pub_f.m")

  # list of regression outputs
    list.mml <- list()
    list.labels <- list()

  # start loop over different hours of the day
  for (i in initial.h:final.h){
    #i<-initial.h
    # read data
    LD <- read.csv(paste(long.data_path, "Long_Data_", i, ".csv", sep =""))
   
    #  logit formula
    f1 <- mFormula(Choice ~ Cost + Cost_I1 + Cost_I2 +
                           Time |
                           age.30_49 + age.50_99 + female)
    # Multinomial Logit ------------------------------------------------------------------------
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

      
      car.6l_c.m <- coefs.m[paste("car", i-2,"_00:(intercept)", sep = ""),1]
      car.5l_c.m <- coefs.m[paste("car", i-2,"_20:(intercept)", sep = ""),1]
      car.4l_c.m <- coefs.m[paste("car", i-2,"_40:(intercept)", sep = ""),1]
      
      car.3l_c.m <- coefs.m[paste("car", i-1,"_00:(intercept)", sep = ""),1]
      car.2l_c.m <- coefs.m[paste("car", i-1,"_20:(intercept)", sep = ""),1]
      car.1l_c.m <- coefs.m[paste("car", i-1,"_40:(intercept)", sep = ""),1]
      
      car.0l_c.m <- coefs.m[paste("car", i,"_00:(intercept)", sep = ""),1]
      car.1m_c.m <- coefs.m[paste("car", i,"_20:(intercept)", sep = ""),1]
      car.2m_c.m <- coefs.m[paste("car", i,"_40:(intercept)", sep = ""),1]
      
      car.3m_c.m <- coefs.m[paste("car", i+1,"_00:(intercept)", sep = ""),1]
      car.4m_c.m <- coefs.m[paste("car", i+1,"_20:(intercept)", sep = ""),1]
      car.5m_c.m <- coefs.m[paste("car", i+1,"_40:(intercept)", sep = ""),1]
      
      pub_c.m <- coefs.m["pub:(intercept)",1]
      
      
      
      
      car.6l_age1.m <- coefs.m[paste("car", i-2,"_00:age.30_49", sep = ""),1]
      car.5l_age1.m <- coefs.m[paste("car", i-2,"_20:age.30_49", sep = ""),1]
      car.4l_age1.m <- coefs.m[paste("car", i-2,"_40:age.30_49", sep = ""),1]
      
      car.3l_age1.m <- coefs.m[paste("car", i-1,"_00:age.30_49", sep = ""),1]
      car.2l_age1.m <- coefs.m[paste("car", i-1,"_20:age.30_49", sep = ""),1]
      car.1l_age1.m <- coefs.m[paste("car", i-1,"_40:age.30_49", sep = ""),1]
      
      car.0l_age1.m <- coefs.m[paste("car", i,"_00:age.30_49", sep = ""),1]
      car.1m_age1.m <- coefs.m[paste("car", i,"_20:age.30_49", sep = ""),1]
      car.2m_age1.m <- coefs.m[paste("car", i,"_40:age.30_49", sep = ""),1]
      
      car.3m_age1.m <- coefs.m[paste("car", i+1,"_00:age.30_49", sep = ""),1]
      car.4m_age1.m <- coefs.m[paste("car", i+1,"_20:age.30_49", sep = ""),1]
      car.5m_age1.m <- coefs.m[paste("car", i+1,"_40:age.30_49", sep = ""),1]
      
      pub_age1.m <- coefs.m["pub:age.30_49",1]
      
      
      
      
      
      car.6l_age2.m <- coefs.m[paste("car", i-2,"_00:age.50_99", sep = ""),1]
      car.5l_age2.m <- coefs.m[paste("car", i-2,"_20:age.50_99", sep = ""),1]
      car.4l_age2.m <- coefs.m[paste("car", i-2,"_40:age.50_99", sep = ""),1]
      
      car.3l_age2.m <- coefs.m[paste("car", i-1,"_00:age.50_99", sep = ""),1]
      car.2l_age2.m <- coefs.m[paste("car", i-1,"_20:age.50_99", sep = ""),1]
      car.1l_age2.m <- coefs.m[paste("car", i-1,"_40:age.50_99", sep = ""),1]
      
      car.0l_age2.m <- coefs.m[paste("car", i,"_00:age.50_99", sep = ""),1]
      car.1m_age2.m <- coefs.m[paste("car", i,"_20:age.50_99", sep = ""),1]
      car.2m_age2.m <- coefs.m[paste("car", i,"_40:age.50_99", sep = ""),1]
      
      car.3m_age2.m <- coefs.m[paste("car", i+1,"_00:age.50_99", sep = ""),1]
      car.4m_age2.m <- coefs.m[paste("car", i+1,"_20:age.50_99", sep = ""),1]
      car.5m_age2.m <- coefs.m[paste("car", i+1,"_40:age.50_99", sep = ""),1]
      
      pub_age2.m <- coefs.m["pub:age.50_99",1]
      
      
      
      
      car.6l_f.m <- coefs.m[paste("car", i-2,"_00:female", sep = ""),1]
      car.5l_f.m <- coefs.m[paste("car", i-2,"_20:female", sep = ""),1]
      car.4l_f.m <- coefs.m[paste("car", i-2,"_40:female", sep = ""),1]
      
      car.3l_f.m <- coefs.m[paste("car", i-1,"_00:female", sep = ""),1]
      car.2l_f.m <- coefs.m[paste("car", i-1,"_20:female", sep = ""),1]
      car.1l_f.m <- coefs.m[paste("car", i-1,"_40:female", sep = ""),1]
      
      car.0l_f.m <- coefs.m[paste("car", i,"_00:female", sep = ""),1]
      car.1m_f.m <- coefs.m[paste("car", i,"_20:female", sep = ""),1]
      car.2m_f.m <- coefs.m[paste("car", i,"_40:female", sep = ""),1]
      
      car.3m_f.m <- coefs.m[paste("car", i+1,"_00:female", sep = ""),1]
      car.4m_f.m <- coefs.m[paste("car", i+1,"_20:female", sep = ""),1]
      car.5m_f.m <- coefs.m[paste("car", i+1,"_40:female", sep = ""),1]
      
      pub_f.m <- coefs.m["pub:female",1]
      
     
      
      
      
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
                        vot.I0.sd.m, vot.I1.sd.m, vot.I2.sd.m,
                        
                        car.6l_c.m, car.5l_c.m, car.4l_c.m,
                        car.3l_c.m, car.2l_c.m, car.1l_c.m,
                        car.0l_c.m, car.1m_c.m, car.2m_c.m,
                        car.3m_c.m, car.4m_c.m, car.5m_c.m, pub_c.m,
                        
                        car.6l_age1.m, car.5l_age1.m, car.4l_age1.m,
                        car.3l_age1.m, car.2l_age1.m, car.1l_age1.m,
                        car.0l_age1.m, car.1m_age1.m, car.2m_age1.m,
                        car.3m_age1.m, car.4m_age1.m, car.5m_age1.m, pub_age1.m,
                        
                        car.6l_age2.m, car.5l_age2.m, car.4l_age2.m,
                        car.3l_age2.m, car.2l_age2.m, car.1l_age2.m,
                        car.0l_age2.m, car.1m_age2.m, car.2m_age2.m,
                        car.3m_age2.m, car.4m_age2.m, car.5m_age2.m, pub_age2.m,
                        
                        car.6l_f.m, car.5l_f.m, car.4l_f.m,
                        car.3l_f.m, car.2l_f.m, car.1l_f.m,
                        car.0l_f.m, car.1m_f.m, car.2m_f.m,
                        car.3m_f.m, car.4m_f.m, car.5m_f.m, pub_f.m)

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



