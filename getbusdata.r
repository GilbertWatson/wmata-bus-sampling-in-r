#install packages to read data
if("rjson" %in% rownames(installed.packages()) == FALSE) {install.packages("rjson")}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("snowfall" %in% rownames(installed.packages()) == FALSE) {install.packages("snowfall")}
require(rjson)
require(RCurl)
require(plyr)
require(snowfall)

#my api key
source(file="~/apikey.txt")

#get all the bus routes to get bus route codes
if (!file.exists("~/wmata-bus-sampling-in-r/busroutes.csv")) {
  busrouteurl <- "http://api.wmata.com/Bus.svc/json/JRoutes?api_key="
  raw_data <- getURL(paste0(busrouteurl,key))
  busroutes <- ldply(fromJSON(raw_data)[[1]],data.frame,stringsAsFactors=F)
  busroutes <- busroutes[-which(grepl(pattern="[a-z]",x=busroutes$RouteID,ignore.case=F)),] #get rid of variation routes
  write.csv(busroutes,file="~/wmata-bus-sampling-in-r/busroutes.csv",row.names=F)
}
busroutes <- read.csv(file="~/wmata-bus-sampling-in-r/busroutes.csv",stringsAsFactors=F)

#get the bus route schedule by route ID
getschedulebybusid <- function(routeID) {
  scheduleurl <- paste0("http://api.wmata.com/Bus.svc/json/JRouteSchedule?routeId=",
                        routeID,
                        "&date=",
                        Sys.Date(),
                        "&includingVariations=true&api_key=",
                        key)
  raw_data <- getURL(scheduleurl)
  busschedule <- ldply(fromJSON(raw_data)[[1]],data.frame,stringsAsFactors=F)
  if (dim(busschedule)[1] != 0) {
    busschedule <- busschedule[,1:4]
    return(busschedule)
  }
  else{
    return()
  }
  Sys.sleep(10)
}

#get all the bus schedules for today
schedules <- NULL
for (n in busroutes$RouteID) {
  schedules <- rbind(schedules, getschedulebybusid(n))
  Sys.sleep(10)
}

#schedules <- do.call(rbind, sapply(X=busroutes$RouteID,FUN=getschedulebybusid))

#get all the stops
getstops <- function() {
  stopsurl <- paste0("http://api.wmata.com/Bus.svc/json/JStops?lat=0&lon=0&radius=0&api_key=",key)
  raw_data <- getURL(stopsurl)
  stops <- ldply(fromJSON(raw_data)[[1]],data.frame,stringsAsFactors=F)
  return(stops)
}

#get stops
stops <- getstops()

#get routes that travel through downtown
lower <- c(38.883216, -77.057690)
upper <- c(38.903508, -77.009657)
downtownroutes <- unique(stops$Routes[which(stops$Lat < upper[1] & stops$Lat > lower[1] & stops$Lon < upper[2] & stops$Lon >  lower[2])])

#make sample frame: 1st define number of buses running
getnumberofbusesrunning <- function(routeID) {
}

#get bus position data function
getbuspositiondata <- function(routeID) {
  buspositionurl <- paste0("http://api.wmata.com/Bus.svc/json/JBusPositions?routeId=",
                           routeID,
                           "&includingVariations=false&lat=0&lon=0&radius=0&api_key=",
                           key)
  raw_data <- getURL(buspositionurl)
  busposition <- ldply(fromJSON(raw_data)[[1]],data.frame,stringsAsFactors=F)
  if (length(busposition) > 0) {
    busposition$System.Time <- Sys.time()
  }
  return(busposition)
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
