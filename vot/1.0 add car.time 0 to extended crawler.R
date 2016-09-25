#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Merge all extended crawler data and add car.time0 and car.distance0                    |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Renato Schwambach Vieira                                                               |    
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Prelims -------------------------------------------------------------------------------------
  rm(list=ls())

# inputs and outputs

  # input:
    # extended crawler csvs (stream/extended-crawler)
    
  # output:
    # csv with all extended craler data
    out_path <- "intermediate/vot/intermediate store/Extended Crawler_all.csv"

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
  
# output --------------------------------------------------------------------------------------
  write.csv(DF.o, out_path, row.names=FALSE)
  write.csv(DF.o, paste(out_path, 4, sep=""), row.names=FALSE)
  
  