#####install packages to read data#####
if("rjson" %in% rownames(installed.packages()) == FALSE) {install.packages("rjson")}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
require(rjson)
require(RCurl)
require(plyr)

######function to get bus routes#####
getbusroutes <- function() {
  busrouteurl <- "http://api.wmata.com/Bus.svc/json/JRoutes?api_key="
  raw_data <- getURL(paste0(busrouteurl,key))
  busroutes <- ldply(fromJSON(raw_data)[[1]],data.frame,stringsAsFactors=F)
  write.csv(busroutes,file="~/wmata-bus-sampling-in-r/busroutes.csv",row.names=F)
}

######get all the stops#####
getstops <- function() {
  stopsurl <- paste0("http://api.wmata.com/Bus.svc/json/JStops?lat=0&lon=0&radius=0&api_key=",key)
  raw_data <- getURL(stopsurl)
  stops <- ldply(fromJSON(raw_data)[[1]],data.frame,stringsAsFactors=F)
  return(stops)
}

######get the bus route schedule by route ID#####
getschedulebybusid <- function(routeID) {
  routeID <- gsub(pattern=" ",replacement="+",x=routeID,fixed=T)
  scheduleurl <- paste0("http://api.wmata.com/Bus.svc/json/JRouteSchedule?routeId=",
                        routeID,
                        "&date=",
                        Sys.Date(),
                        "&includingVariations=true&api_key=",
                        key)
  raw_data <- getURL(scheduleurl)
  json <- fromJSON(raw_data)
  busschedule0 <- ldply(json[[1]],data.frame,stringsAsFactors=F)
  busschedule1 <- ldply(json[[2]],data.frame,stringsAsFactors=F)
  if ((dim(busschedule1)[1] != 0) | (dim(busschedule0)[1] != 0)) {
    busschedule <- rbind(busschedule1[,1:4],busschedule0[,1:4])
    return(busschedule)
  }
  else{
    return()
  }
  Sys.sleep(10)
}

#get all the bus schedules for today
getallschedulesfortoday <- function(option) {
  if (option == "all") {
    br <- busroutes$RouteID
  }
  if (option =="downtown") {
    br <- busroutes$RouteID[which(busroutes$Has.Downtown.Stops.On.Route == T)]
  }
  schedules <- NULL
  for (n in br) {
    schedules <- rbind(schedules, getschedulebybusid(n))
    Sys.sleep(10)
  }
  return(schedules)
}

######get bus position data function#####
getbuspositiondata <- function(routeID) {
  routeID <- gsub(pattern=" ",replacement="+",x=routeID,fixed=T)
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