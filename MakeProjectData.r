MakeProjectData <- function() {
  source("~/wmata-bus-sampling-in-r/MakeSample.r")
  LoadAPIToolsAndPopulationData()
  ss <- 8
  file = "~/home"
  execute <- c(seq.POSIXt(from=as.POSIXlt(paste0(Sys.Date()," 07:30:00 EDT")),to=as.POSIXlt(paste0(Sys.Date()," 09:30:00 EDT")),by=60),
               seq.POSIXt(from=as.POSIXlt(paste0(Sys.Date()," 16:00:00 EDT")),to=as.POSIXlt(paste0(Sys.Date()," 20:30:00 EDT")),by=60), #change back to 18:30:00
               as.POSIXct(paste0(Sys.Date()," 04:00:00 EDT")),
               as.POSIXct(paste0(Sys.Date()," 05:00:00 EDT")))
  #go get population data and make sure it is there before rush hour
  while(T) {
    Sys.sleep(0.5)
    t <- Sys.time()
    if (as.POSIXlt(t) %in% execute) {
      if ((as.POSIXlt(t)$hour == 4 & as.POSIXlt(t)$min == 00 & as.POSIXlt(t)$min == 00)|(!file.exists(paste0("~/wmata-bus-sampling-in-r/NumberOfBusesByRouteIDAndTime.",Sys.Date(),".csv")))) {
        LoadAPIToolsAndPopulationData()
      }
      if ((as.POSIXlt(t)$hour == 5 & as.POSIXlt(t)$min == 00 & as.POSIXlt(t)$min == 00)|(!file.exists(paste0("~/wmata-bus-sampling-in-r/NumberOfBusesByRouteIDAndTime.",Sys.Date(),".csv")))) {
        LoadAPIToolsAndPopulationData()
        if (!file.exists(paste0("~/wmata-bus-sampling-in-r/NumberOfBusesByRouteIDAndTime.",Sys.Date(),".csv"))) {
          stop("NO Population Data For the Day!!!!!!!")
        }
      }
      #start sampling during rush hour
      if (as.POSIXlt(t) %in% seq.POSIXt(from=as.POSIXlt(paste0(Sys.Date()," 07:30:00 EDT")),to=as.POSIXlt(paste0(Sys.Date()," 09:30:00 EDT")),by=60)) {
        ToWriteToFile <- GetDataForSampleOfBusesNow(SampleSize=ss,SamplingFrame=getSamplingFrameNow())
        if (file.exists(paste0("~/wmata-bus-sampling-in-r/BusSample.",Sys.Date(),".csv"))) {
          write.csv(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".morning.csv"),append=T,row.names=F)
        }
        else {
          write.csv(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".morning.csv"),append=F,row.names=F)
        }
      }
      if (as.POSIXlt(t) %in% seq.POSIXt(from=as.POSIXlt(paste0(Sys.Date()," 16:00:00 EDT")),to=as.POSIXlt(paste0(Sys.Date()," 20:30:00 EDT")),by=60)) { #change back to 18:30:00
        ToWriteToFile <- GetDataForSampleOfBusesNow(SampleSize=ss,SamplingFrame=getSamplingFrameNow())
        if (file.exists(paste0("~/wmata-bus-sampling-in-r/BusSample.",Sys.Date(),".csv"))) {
          write.csv(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".evening.csv"),append=T,row.names=F)
        }
        else {
          write.csv(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".evening.csv"),append=F,row.names=F)
        }
      }
    } 
  }
}
MakeProjectData()