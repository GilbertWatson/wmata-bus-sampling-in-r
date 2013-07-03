#####install packages to read data#####
if("rjson" %in% rownames(installed.packages()) == FALSE) {install.packages("rjson")}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("snowfall" %in% rownames(installed.packages()) == FALSE) {install.packages("snowfall")}
require(rjson)
require(RCurl)
require(plyr)
require(snowfall)

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
TodaysSchedule <- read.csv(file=paste0("~/wmata-bus-sampling-in-r/Schedule.",Sys.Date(),".csv"))

#make sample frame: 1st define number of buses running
getnumberofbusesrunning <- function(routeID) {
}

#csv file
filefordata <- "/home/gilbert/Documents/busdata.csv"

#function to write to csv
writetocsv <- function(regulardataframe, filelocation) {
  write.csv2(regulardataframe,file=filelocation,append=T,row.names=F,col.names=T)
}

#get sample of buses
while (as.POSIXlt(Sys.time())$hour < 9) {
  a <- getbuspositiondata(sample(x=busroutes$RouteID,size=1,replace=T))
  if (length(a) > 0) {
    writetocsv(regulardataframe=a,filelocation=filefordata)
  }
  Sys.sleep(1)
}