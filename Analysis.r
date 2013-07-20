#####read data from file#####

#get bus data
setwd("~/wmata-bus-sampling-in-r/sampledata/")
Monday <- read.csv("BusSample.2013-07-15.csv",stringsAsFactors=F)
Tuesday <- read.csv("BusSample.2013-07-16.csv",stringsAsFactors=F) #Tuesday Morning Failed
Wednesday <- read.csv("BusSample.2013-07-17.csv",stringsAsFactors=F)
Thursday <- read.csv("BusSample.2013-07-18.csv",stringsAsFactors=F)
# Friday <- read.csv("BusSample.2013-07-19.csv",stringsAsFactors=F) #Friday Failed

Data <- rbind(Monday,Tuesday,Wednesday,Thursday)
rm(Monday,Tuesday,Wednesday,Thursday)

#get number of buses data
setwd("~/wmata-bus-sampling-in-r/")
N_Monday <- read.csv("NumberOfBusesByRouteIDAndTime.2013-07-15.csv",stringsAsFactors=F)
# N_Tuesday <- read.csv("NumberOfBusesByRouteIDAndTime.2013-07-16.csv",stringsAsFactors=F) #failed
N_Wednesday <- read.csv("NumberOfBusesByRouteIDAndTime.2013-07-17.csv",stringsAsFactors=F)
N_Thursday <- read.csv("NumberOfBusesByRouteIDAndTime.2013-07-18.csv",stringsAsFactors=F)
# N_Friday <- read.csv("NumberOfBusesByRouteIDAndTime.2013-07-19.csv",stringsAsFactors=F) #failed

N_Data <- rbind(N_Monday,N_Wednesday,N_Thursday)
rm(N_Monday,N_Wednesday,N_Thursday)

#get schedule data
S_Monday <- read.csv("Schedule.2013-07-15.csv",stringsAsFactors=F)
# S_Tuesday <- read.csv("Schedule.2013-07-16.csv",stringsAsFactors=F) #failed
S_Wednesday <- read.csv("Schedule.2013-07-17.csv",stringsAsFactors=F)
S_Thursday <- read.csv("Schedule.2013-07-18.csv",stringsAsFactors=F)
# S_Friday <- read.csv("Schedule.2013-07-19.csv",stringsAsFactors=F) #failed

S_Data <- rbind(S_Monday,S_Wednesday,S_Thursday)
S_Data <- S_Data[order(S_Data$RouteID,S_Data$DirectionNum,S_Data$StartTime),]
S_Data$Day <- as.POSIXlt(S_Data$StartTime)$mday
rm(S_Monday,S_Wednesday,S_Thursday)

#stop data
Stops <- read.csv("stops.csv",stringsAsFactors=F)

#####clean the data#####

#clean times
Data$DateTime <- as.POSIXlt(Data$DateTime)
Data$TripEndTime <- as.POSIXlt(Data$TripStartTime)
Data$TripEndTime <- as.POSIXlt(Data$TripEndTime)
Data$System.Time <- as.POSIXlt(Data$System.Time)
Data$SystemTime <- as.POSIXlt(Data$SystemTime)
N_Data$Time <- as.POSIXlt(N_Data$Time,origin="1970-01-01", tz="America/New_York")
N_Data$Hour <- N_Data$Time$hour
N_Data$Day <- N_Data$Time$mday

#clean trip ids
Data$RouteID <- gsub(pattern="([a-z][0-9])|([a-z])",replacement="",x=Data$RouteID)
Data$RouteID[which(!grepl("(^S)|( S)|(18S)|(9S)|(8S)",Data$RouteID))] <- gsub(pattern="(S[0-9])|(S)",replacement="",x=Data$RouteID[which(!grepl("(^S)|( S)|(18S)|(9S)|(8S)",Data$RouteID))])

#only keep routes we want
rout <- c("90","X2","S1","D4","L2","64","D8","H1","S9","53",
          "31","70","42","V7","P6","N4","D5","V9","16X","5A",
          "G2","K2","A9","74","M4")
Data <- Data[which(Data$RouteID %in% rout),]

#get number of stops
Data$Number.Of.Stops <- sapply(X=Data$RouteID,FUN=function(x) {
  return(length(Stops$StopID[which(Stops$Routes == x)]))
})

#####get frequencies#####

#Calculate Bus Frequency by Time Leaving Start of Route#
FrequencyByRoute <- function(RouteID,Sched) {
  ScheduleData <- Sched
  ScheduleData$RouteID <- gsub(pattern="([a-z][0-9])|([a-z])",replacement="",x=ScheduleData$RouteID)
  ScheduleData$RouteID[which(!grepl("(^S)|( S)|(18S)|(9S)|(8S)",ScheduleData$RouteID))] <- gsub(pattern="(S[0-9])|(S)",replacement="",x=ScheduleData$RouteID[which(!grepl("(^S)|( S)|(18S)|(9S)|(8S)",ScheduleData$RouteID))])
  ScheduleData$StartTime <- as.POSIXct(gsub(pattern="T",replacement=" ",x=ScheduleData$StartTime))
  ScheduleForRoute.One <- ScheduleData$StartTime[which(ScheduleData$RouteID == RouteID & ScheduleData$DirectionNum == 1)]
  ScheduleForRoute.One <- ScheduleForRoute.One[order(ScheduleForRoute.One,decreasing=F)]
  ScheduleForRoute.Zero <- ScheduleData$StartTime[which(ScheduleData$RouteID == RouteID & ScheduleData$DirectionNum == 0)]
  ScheduleForRoute.Zero <- ScheduleForRoute.Zero[order(ScheduleForRoute.Zero,decreasing=F)]
  ScheduleForRoute.One.Frequency <- c(NA,diff(ScheduleForRoute.One))
  ScheduleForRoute.Zero.Frequency <- c(NA,diff(ScheduleForRoute.Zero))
  Zero <- as.data.frame(cbind(ScheduleForRoute.Zero,ScheduleForRoute.Zero.Frequency,rep(0,length(ScheduleForRoute.Zero.Frequency)),rep(RouteID,length(ScheduleForRoute.Zero.Frequency))),stringsAsFactors=F)
  One <- as.data.frame(cbind(ScheduleForRoute.One,ScheduleForRoute.One.Frequency,rep(1,length(ScheduleForRoute.One.Frequency)),rep(RouteID,length(ScheduleForRoute.One.Frequency))),stringsAsFactors=F)
  if (dim(Zero)[2] < 4) {
    out <- One
  }
  if (dim(One)[2] < 4) {
    out <- Zero
  }
  if (dim(Zero)[2] == 4 & dim(One)[2] == 4) {
    names(One) <- c("StartTime","Frequency","DirectionNum","RouteID")
    names(Zero) <- c("StartTime","Frequency","DirectionNum","RouteID")
    out <- as.data.frame(rbind(Zero,One),stringsAsFactors=F)
  }
  names(out) <- c("StartTime","Frequency","DirectionNum","RouteID")
  return(out)
}
getAllFrequencies <- function(vectorofroutes) {
  Data <- NULL
  for (n in vectorofroutes) {
    Data <- rbind(Data,FrequencyByRoute(n,S_Data))
  }
  return(Data)
}
AllFrequencies <- getAllFrequencies(unique(Data$RouteID))
class(AllFrequencies$StartTime) <- c('POSIXt','POSIXct')
AllFrequencies$StartTime <- as.POSIXlt(AllFrequencies$StartTime, tz="America/New_York")
AllFrequencies$Hour <- AllFrequencies$StartTime$hour
AllFrequencies$Day <- AllFrequencies$StartTime$mday
AllFrequencies$Frequency[which(as.numeric(AllFrequencies$Frequency) > 180)] <- NA

#get average frequency per hour
Frequency <- aggregate(as.numeric(AllFrequencies$Frequency),by=list(AllFrequencies$RouteID,AllFrequencies$Hour,AllFrequencies$Day),FUN=function(x) {return(mean(x,na.rm=T))})
names(Frequency) <- c("RouteID","Hour","Day","Frequency")
Frequency$Frequency[which(is.nan(Frequency$Frequency))] <- NA
rm(AllFrequencies)

#get average number of buses on route
BusesOnRoute <- aggregate(N_Data$Buses.On.Route,by=list(N_Data$RouteID,N_Data$Hour,N_Data$Day),FUN=function(x) {return(mean(x,na.rm=T))})
names(BusesOnRoute) <- c("RouteID","Hour","Day","BusesOnRoute")

#####put everything together#####
Data$System.Hour <- Data$SystemTime$hour
Data$System.Day <- Data$SystemTime$mday
Data$System.Sec <- Data$SystemTime$sec
AllData <- merge(Data,Frequency,by.x=c("RouteID","System.Day","System.Hour"),by.y=c("RouteID","Day","Hour"),all.x=T,all.y=F)
AllData <- merge(AllData,BusesOnRoute,by.x=c("RouteID","System.Day","System.Hour"),by.y=c("RouteID","Day","Hour"),all.x=T,all.y=F)

#clean up
rm(rout,FrequencyByRoute,getAllFrequencies,Data,Frequency,N_Data,S_Data,Stops)

#sample the data
samplebusdata <- function(strata,n_strata,data) {
  strata_u <- unique(data[,c(strata)])
  if (length(strata_u) != length(n_strata)) {
    stop("strata has no defined n to sample")
  }
  getit <- NULL
  for (n in seq(1,length(strata_u),1)) {
    getit <- rbind(getit,data[sample(x=which(data[,c(strata)] == strata_u[1]),size=n_strata[n],replace=F),])
  }
  return(getit)
}

#make strata
AllData$StopStrata <- sapply(AllData$Number.Of.Stops,FUN=function(x) {
  if (x < 50) {
    r <- "less than 50 stops"
  }
  if (x >= 50 & x < 100) {
    r <- "50 or greater, less than 100 stops"
  }
  if (x >= 100) {
    r <- "more than 100 stops"
  }
  return(r)
})


#max variance for strata from pilot
maxs2 <- c(109.44,443.18,588.47)

#get neyman allocation
getneyman <- function(data,strata,s2) {
  strata_u <- unique(data[,c(strata)])
  N_h <- sapply(strata_u,FUN=function(x) {
    return(length(data[which(data[,c(strata)] == x),1]))
  })
  S_h <- sqrt(s2)
  W_h <- (N_h*S_h)/sum(N_h*S_h)
  top <- sum(N_h^2*(S_h^2/W_h))
  bottom <- sum(N_h*S_h^2) + (sum(N_h)^2)*(4/(1.96^2))
  n <- top/bottom
  n_h <- n*W_h
  n_h <- ceiling(n_h)
}

