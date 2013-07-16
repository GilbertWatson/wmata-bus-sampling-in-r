#####install packages to read data#####
if("sampling" %in% rownames(installed.packages()) == FALSE) {install.packages("sampling")}
require(sampling)

######source population builders and api access#####
LoadAPIToolsAndPopulationData <- function() {
  source("~/wmata-bus-sampling-in-r/PopulationBuilder.r")
}

######define sampling frame on the bus route dimension#####
getSamplingFrameNow <- function() {
  RouteSamplingFrame <- busroutes$RouteID[which(busroutes$Has.Downtown.Stops.On.Route == T & busroutes$RouteID %in% unique(NumberOfActiveTripsByRouteIdAndTime$RouteID[which(NumberOfActiveTripsByRouteIdAndTime$Buses.On.Route > 0)]))]
  time <- as.POSIXlt(Sys.time())
  time$sec <- 00
  RouteSamplingFrameNow <- RouteSamplingFrame[which(RouteSamplingFrame %in% NumberOfActiveTripsByRouteIdAndTime$RouteID[which(NumberOfActiveTripsByRouteIdAndTime$Time == time & NumberOfActiveTripsByRouteIdAndTime$Buses.On.Route != 0)])]
  return(RouteSamplingFrameNow)
}

#####Compute Identifiers for Strata fron getSamplingFrameNow#####
getStrataDataNow <- function(strata) {
  if (strata == "Number.Of.Stops") {
    return(busroutes[which(busroutes$RouteID %in% getSamplingFrameNow()),c("RouteID","Number.Of.Stops")])
  }
  if (strata == "Frequency") {
    data <- AllFrequencies[which(AllFrequencies$RouteID %in% getSamplingFrameNow() & as.POSIXlt(AllFrequencies$StartTime)$hour == as.POSIXlt(Sys.time())$hour),]
    data <- aggregate(x=data$Frequency,by=list(data$RouteID),FUN=function(x) {return(mean(as.numeric(x),na.rm=T))})
    names(data) <- c("RouteID","Average.Frequency.This.Hour")
    return(data)
  }
}

#####function that randomly selects from the sampling frame, all units are sampled over the course of one minute#####
GetDataForSampleOfBusesNow <- function(SampleSize,SamplingFrame,stratify,option) {
  if (SampleSize > 60) {
    stop("Sample Size Will Break API limits")
  }
  else {
    if (stratify==T) {
      stratadata <- getStrataDataNow(option)
      if (option == "Number.Of.Stops") {
        hlist <- stratadata$RouteID[which(stratadata$Number.Of.Stops >= mean(stratadata$Number.Of.Stops))]
        llist <- stratadata$RouteID[which(stratadata$Number.Of.Stops < mean(stratadata$Number.Of.Stops))]
        hp <- SampleSize*(length(hlist)/length(c(hlist,llist)))
        lp <- SampleSize*(length(llist)/length(c(hlist,llist)))
      }
      if (option == "Frequency") {
        hlist <- stratadata$RouteID[which(stratadata$Average.Frequency.This.Hour >= mean(stratadata$Average.Frequency.This.Hour))]
        llist <- stratadata$RouteID[which(stratadata$Average.Frequency.This.Hour < mean(stratadata$Average.Frequency.This.Hour))]
        hp <- SampleSize*(length(hlist)/length(c(hlist,llist)))
        lp <- SampleSize*(length(llist)/length(c(hlist,llist)))
      }
      if ((hp-floor(hp)) >= (lp-floor(lp))) {
        hp <- ceiling(hp)
        lp <- floor(lp)
      }
      if ((hp - floor(hp)) < (lp-floor(lp))) {
        hp <- floor(hp)
        lp <- ceiling(hp)
      }
      BusesToSample <- c(sample(x=hlist,size=hp,replace=F),sample(x=llist,size=lp,replace=F))
      BusSampleData <- NULL
      for (n in BusesToSample) {
        BusSampleData <- rbind(BusSampleData,getbuspositiondata(n))
        Sys.sleep(50/SampleSize)
      }
      BusSampleData$SystemTime <- as.POSIXlt(Sys.time())
      BusSampleData$little_n.Route <- SampleSize
      BusSampleData$big_N.Route <- length(SamplingFrame)
      return(BusSampleData)
      test <- data.frame(BusesToSample=BusesToSample,stringsAsFactors=F)
      test$System.Time <- Sys.time()
      write.table(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/desiredsamples.csv"),append=T,row.names=F,col.names=F,sep=",",eol="\n")
    }
    else {
      BusesToSample <- sample(x=SamplingFrame,size=SampleSize,replace=F)
      BusSampleData <- NULL
      for (n in BusesToSample) {
        BusSampleData <- rbind(BusSampleData,getbuspositiondata(n))
        Sys.sleep(50/SampleSize)
      }
      BusSampleData$SystemTime <- as.POSIXlt(Sys.time())
      BusSampleData$little_n.Route <- SampleSize
      BusSampleData$big_N.Route <- length(SamplingFrame)
      return(BusSampleData)
      test <- data.frame(BusesToSample=BusesToSample,stringsAsFactors=F)
      test$System.Time <- Sys.time()
      write.table(ToWriteToFile,file=paste0("~/wmata-bus-sampling-in-r/sampledata/desiredsamples.csv"),append=T,row.names=F,col.names=F,sep=",",eol="\n")
    }
  }
}

#get simple data
rout <- c("90","X2","S1","D4","L2","64","D8","H1","S9","53",
        "31","70","42","V7","P6","N4","D5","V9","16X","5A")

GetDataNow <- function(buses=rout) {
  BusesToSample <- buses
  SampleSize <- length(BusesToSample)
  BusSampleData <- NULL
  for (n in BusesToSample) {
    BusSampleData <- rbind(BusSampleData,getbuspositiondata(n))
    Sys.sleep(50/SampleSize)
  }
  BusSampleData$SystemTime <- as.POSIXlt(Sys.time())
  return(BusSampleData)
}
