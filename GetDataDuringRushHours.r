#!/usr/bin/env Rscript
source("~/wmata-bus-sampling-in-r/MakeSample.r")
ss <- 30
LoadAPIToolsAndPopulationData()
rout <- c("90","X2","S1","D4","L2","64","D8","H1","S9","53",
          "31","70","42","V7","P6","N4","D5","V9","16X","5A",
          "G2","K2","A9","74","M4")
#ToWriteToFile <- GetDataForSampleOfBusesNow(SampleSize=ss,SamplingFrame=getSamplingFrameNow(),stratify=T,option="Number.Of.Stops") #for stratified cluster sampling
ToWriteToFile <- GetDataNow(rout) #forsimple data creation with select routes "rt" defined in "MakeSample.r"
if (file.exists(paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".csv"))) {
  write.table(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".csv"),append=T,row.names=F,col.names=F,sep=",",eol="\n")
}
if (!file.exists(paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".csv"))) {
  write.table(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".csv"),append=F,row.names=F,sep=",",eol="\n")
}
