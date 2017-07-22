###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script sets for compiling count data ################
################## Time Cut for data ##########################
# authors: J. Wehrmann, #######################################
# todos: get suntable data till 2020
###############################################################

#### libraries ####
library(lubridate)
library(tcltk2)

#### directories ####
# modify the path
workdir<-"/Users/jasper 1/Documents/Rplayground/data/in"
outputdir<-"/Users/jasper 1/Documents/Rplayground/data/out"

#### modify time cut, all in seconds ###########################
risecut<-as.numeric("3600")
setcut<-as.numeric("7200")

### read data variable from script "BRC-compile_all_data" #####
records<-NAMEOFVARIABLE

### Timecutter START ###
## prepare data
records$record<-seq(1,length(records[,1]), 1)
records$number<-as.numeric(records$number)
records$number[which(is.na(records$number) == T)]<-1
records$time2<-paste(records$date, records$time, sep = " ")
records$time2<-as.POSIXlt(records$time2, format="%Y-%m-%d %H:%M:%S")
records$year<-year(records$time2)
records$yday<-yday(records$time2)
records$lab<-paste(records$year, records$yday)

## prepare sunset and sunrise data
setwd(workdir)
suntab<-read.csv("sunrisesetTable.csv", header=T, stringsAsFactors=T, fileEncoding="UTF-7")
suntab$date<-as.POSIXct(suntab$date, format="%Y-%m-%d")
suntab$year<-year(suntab$date)
suntab$yday<-yday(suntab$date)
suntab$lab<-paste(suntab$year, suntab$yday)
suntab$sunrise_R<-as.POSIXct(suntab$sunrise_R, format="%Y-%m-%d %H:%M:%S")
suntab$sunset_R<-as.POSIXct(suntab$sunset_R, format="%Y-%m-%d %H:%M:%S")
suntabrest<-suntab
suntabrest$sunrise2<-suntabrest$sunrise_R+risecut
suntabrest$sunset2<-suntabrest$sunset_R-setcut

## data gets cut off 
for (a in 1:length(records[,1])){
	b<-which(suntabrest$lab == records$lab[a])
	records$risecut[a]<-suntabrest$sunrise2[b]
	records$setcut[a]<-suntabrest$sunset2[b] 	
	}

cuts<-which(records$time2>records$risecut & records$time2<records$setcut)
records<-records[cuts,]
finaldata<-records
finaldata$time2<-NULL
finaldata$lab<-NULL
finaldata$setcut<-NULL
finaldata$risecut<-NULL
finaldata$dcdel<-NULL
finaldata$record<-NULL
finaldata$counttype<-NULL
finaldata$health<-NULL
finaldata$remark<-NULL
finaldata$record<-NULL
names(finaldata)

## write results
setwd(outputdir)
write.csv(finaldata, "1-timecutter_results.csv", fileEncoding="UTF-8", row.names=FALSE)