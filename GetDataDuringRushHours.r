#!/usr/bin/env Rscript
source("~/wmata-bus-sampling-in-r/MakeSample.r")
ss <- 10
LoadAPIToolsAndPopulationData()
ToWriteToFile <- GetDataForSampleOfBusesNow(SampleSize=ss,SamplingFrame=getSamplingFrameNow())
if (file.exists(paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".csv"))) {
  write.table(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".csv"),append=T,row.names=F,col.names=F,sep=",",eol="\n")
}
if (!file.exists(paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".csv"))) {
  write.table(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/BusSample.",Sys.Date(),".csv"),append=F,row.names=F,sep=",",eol="\n")
}