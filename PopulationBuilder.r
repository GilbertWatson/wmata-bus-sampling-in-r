#####install packages to read data#####
if("rjson" %in% rownames(installed.packages()) == FALSE) {install.packages("rjson")}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("sampling" %in% rownames(installed.packages()) == FALSE) {install.packages("sampling")}
require(rjson)
require(RCurl)
require(plyr)

######load api functions#####
source(file="~/wmata-bus-sampling-in-r/getbusdata.r")

######my api key#####
source(file="~/apikey.txt")

######get all the bus routes to get bus route codes#####
if (!file.exists("~/wmata-bus-sampling-in-r/busroutes.csv")) {
  getbusroutes()
}
busroutes <- read.csv(file="~/wmata-bus-sampling-in-r/busroutes.csv",stringsAsFactors=F)
busroutes <- busroutes[-which(grepl(pattern="[a-z]",x=busroutes$RouteID,ignore.case=F) | !grepl(pattern="[0-9]",x=busroutes$RouteID,ignore.case=F)),] #get rid of variation routes

######get stops#####
if (!file.exists("~/wmata-bus-sampling-in-r/stops.csv")) {
  stops <- getstops()
  write.csv(stops,file="~/wmata-bus-sampling-in-r/stops.csv",row.names=F)
}
stops <- read.csv("~/wmata-bus-sampling-in-r/stops.csv",stringsAsFactors=F)

######get routes that travel through downtown#####
lower <- c(38.883216, -77.057690)
upper <- c(38.903508, -77.009657)
downtownroutes <- unique(stops$Routes[which(stops$Lat < upper[1] & stops$Lat > lower[1] & stops$Lon < upper[2] & stops$Lon >  lower[2])])
busroutes$Has.Downtown.Stops.On.Route <- busroutes$RouteID %in% downtownroutes

######get today's schedule#####
if (!file.exists(paste0("~/wmata-bus-sampling-in-r/Schedule.",Sys.Date(),".csv"))) {
  TodaysSchedule <- getallschedulesfortoday(option="downtown")
  write.csv(TodaysSchedule,file=paste0("~/wmata-bus-sampling-in-r/Schedule.",Sys.Date(),".csv"),row.names=F)
}
TodaysSchedule <- read.csv(file=paste0("~/wmata-bus-sampling-in-r/Schedule.",Sys.Date(),".csv"),stringsAsFactors=F)

#get number of buses running by 1 minute interval for routeID
getnumberofbusesrunning <- function(routeID) {
  #get today's schedule and parse out variation routes, POSNIX the dates
  ScheduleData <- TodaysSchedule
  ScheduleData$RouteID <- gsub(pattern="([a-z][0-9])|([a-z])",replacement="",x=ScheduleData$RouteID)
  ScheduleData$RouteID[which(!grepl("(^S)|( S)|(18S)|(9S)|(8S)",ScheduleData$RouteID))] <- gsub(pattern="(S[0-9])|(S)",replacement="",x=ScheduleData$RouteID[which(!grepl("(^S)|( S)|(18S)|(9S)|(8S)",ScheduleData$RouteID))])
  ScheduleForRoute <- ScheduleData[which(ScheduleData$RouteID == routeID),]
  ScheduleForRoute$EndTime <- as.POSIXlt(gsub(pattern="T",replacement=" ",x=ScheduleForRoute$EndTime))
  ScheduleForRoute$StartTime <- as.POSIXlt(gsub(pattern="T",replacement=" ",x=ScheduleForRoute$StartTime))
  #generate a sequence of POSNIX from 5am to 3am
  TimeSequence <- seq.POSIXt(from=as.POSIXlt(paste0(Sys.Date()," 00:00:00")),to=as.POSIXlt(paste0(Sys.Date()," 24:00:00")),by=60)
  #get the number of buses running on this route at time
  BusesOnRoute <- sapply(X=TimeSequence,FUN=function(x) {
    TripsAtTime <- length(ScheduleForRoute$RouteID[which(ScheduleForRoute$StartTime <= x & ScheduleForRoute$EndTime >= x)])
    return(TripsAtTime)
  })
  #turn out a data frame
  BusesByTime <- as.data.frame(cbind(TimeSequence,BusesOnRoute,rep(routeID,length(TimeSequence))),stringsAsFactors=F)
  names(BusesByTime) <- c("Time","Buses.On.Route","RouteID")
  return(BusesByTime)
}

#get # of buses by route for all buses in a vector of buses at 1 min intervals
getbigNdata <- function(vectorofrouteIDs) {
  bigtable <- as.data.frame(getnumberofbusesrunning(vectorofrouteIDs[1]),stringsAsFactors=F)
  names(bigtable) <- c("Time","Buses.On.Route","RouteID")
  for (n in vectorofrouteIDs[2:length(vectorofrouteIDs)]) {
    toadd <- as.data.frame(getnumberofbusesrunning(n),stringsAsFactors=F)
    names(toadd) <- c("Time","Buses.On.Route","RouteID")
    bigtable <- merge(bigtable,toadd,by=c("Time","Buses.On.Route","RouteID"),all=T)
  }
  bigtable <- bigtable[order(bigtable$RouteID,bigtable$Time),]
  return(bigtable)
}

######get todays n and N by time#####
if (!file.exists(paste0("~/wmata-bus-sampling-in-r/NumberOfBusesByRouteIDAndTime.",Sys.Date(),".csv"))) {
  NumberOfActiveTripsByRouteIdAndTime <- getbigNdata(busroutes$RouteID[which(busroutes$Has.Downtown.Stops.On.Route == T)])
  write.csv(NumberOfActiveTripsByRouteIdAndTime,file=paste0("~/wmata-bus-sampling-in-r/NumberOfBusesByRouteIDAndTime.",Sys.Date(),".csv"),row.names=F)
}
NumberOfActiveTripsByRouteIdAndTime <- read.csv(file=paste0("~/wmata-bus-sampling-in-r/NumberOfBusesByRouteIDAndTime.",Sys.Date(),".csv"),stringsAsFactors=F)