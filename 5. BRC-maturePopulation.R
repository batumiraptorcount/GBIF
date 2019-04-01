###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script for calculation of mature population #########
# author: J. Wehrmann #########################################
###############################################################

rm(list=ls())

#### functions #####
# when proportion is NA fill in this blank using the nearest available value
f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos

  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

#### libraries ####
library(lubridate)
library(zoo)

Sys.setenv(TZ="Europe/Berlin")

#### directories ####
# modify the path
workdir<-"/Users/Jasper/Documents/Rplayground/data analysis - Bitbucket/data/"
setwd(workdir)

#### read data ####

#mature population
data<-read.csv("final/PROCESSED-allyears_withFilter.csv", header=TRUE, encoding="UTF-8", sep=",")
data<-subset(data,data$filter == 1)
# run correction for UID with other script
if (sum(unique(data$filter))>1){
dailysums_corr_uid<-read.csv("final/daytotals_corrected_for_uid_harriers.csv", header=TRUE, encoding="UTF-8", sep=",")
} else if (unique(data$filter)==1){
dailysums_corr_uid<-read.csv("final/daytotals_corrected_for_uid_filter1.csv", header=TRUE, encoding="UTF-8", sep=",")
}


### edit species !!!!! #####
species<-"Mon"
# BlackKite is BK !!!

# run proprtional estimation of aged birds
records<-data
records$year<-year(records$date)
dailysums<-tapply(as.numeric(as.character(records$number)), list(as.Date(records$date), records$species), FUN=sum, na.rm=TRUE)
dailysums[is.na(dailysums)]<-0
dailysums <-data.frame(cbind(date=rownames(dailysums),dailysums), row.names=1:length(rownames(dailysums)),stringsAsFactors = FALSE)



if (species == "BootedE" | species == "Mon" | species == "Pal" | species == "Mar" | species == "GreaterSE" | species == "LesserSE" | species == "SteppeE" | species == "ShortTE"){
### Large Eagles, Harriers, ShortTE, Osprey


spec<-records[records$species==species,]

if (species == "Mon" | species == "Pal" | species == "Mar"){
zones<-which(spec$location=="E1" | spec$location=="O" | spec$location=="W1")
} else if (species == "GreaterSE" | species == "LesserSE" | species == "SteppeE"){
zones<-which(spec$location!="E4")
} else {zones<-which(spec$location!="E3" & spec$location!="W3")

}	
dailysums<-tapply(as.numeric(as.character(spec$number)), list(as.Date(spec$date), spec$age), FUN=sum, na.rm=FALSE)
dailysums[is.na(dailysums)]<-0
dailysums<-data.frame(cbind(date=rownames(dailysums),dailysums), row.names=1:length(rownames(dailysums)),stringsAsFactors = FALSE)
#names(dailysums)[names(dailysums)=="V2"]<-"noage"
#dailysums$all1<-as.numeric(dailysums$ad)+as.numeric(dailysums$imm)+as.numeric(dailysums$juv)+as.numeric(dailysums$nonjuv)+as.numeric(dailysums$noage)


dailysums_age<-tapply(as.numeric(as.character(spec$number[zones])), list(as.Date(spec$date[zones]), spec$age[zones]), FUN=sum, na.rm=FALSE)
dailysums_age[is.na(dailysums_age)]<-0
dailysums_age <-data.frame(cbind(date=rownames(dailysums_age),dailysums_age), row.names=1:length(rownames(dailysums_age)),stringsAsFactors = FALSE)

names(dailysums_age)[names(dailysums_age)=="V2"]<-"noage"
dailysums_age$all1<-as.numeric(dailysums_age$ad)+as.numeric(dailysums_age$imm)+as.numeric(dailysums_age$juv)+as.numeric(dailysums_age$nonjuv)+as.numeric(dailysums_age$noage)
dailysums_age<-dailysums_age[dailysums_age$all1>0,]

results<-data.frame(date=dailysums_corr_uid$date, test=as.Date("1800-01-01"), total=0)
results$test[which(results$date %in% dailysums_age$date)]<-dailysums_corr_uid$date[which(eval(parse(text=paste0("dailysums_corr_uid$",species)))>0 & dailysums_corr_uid$date %in% dailysums_age$date)]
results$total[which(results$date %in% dailysums_corr_uid$date)]<-eval(parse(text=paste0("dailysums_corr_uid$",species)))[which(results$date %in% dailysums_corr_uid$date)]


results$test[results$test=="1800-01-01"]<-NA


### proprtionen altersklassen berechnen
all_ad<-as.numeric(dailysums_age$ad)
all_nonjuv<-as.numeric(dailysums_age$ad)+as.numeric(dailysums_age$imm)+as.numeric(dailysums_age$nonjuv)
all_juv<-as.numeric(dailysums_age$juv)
all_imm<-as.numeric(dailysums_age$imm)
all_aged<-all_nonjuv+all_juv
aged<-which(all_aged>=5)
dailysums_age$nonjuv_RAT1<-100/(all_aged)*all_nonjuv
dailysums_age$nonjuv_RAT1[-aged]<-NA
dailysums_age$nonjuv_RAT2<-dailysums_age$nonjuv_RAT1

results$nonjuv_RAT2<-NA
results$nonjuv_RAT2[which(results$date %in% dailysums_age$date)]<-dailysums_age$nonjuv_RAT2

# wende use nearest value function Tag und jedes Jahr an

years<-unique(year(results$date))
for (i in 1:length(years)){
	year_block<-which(year(results$date) == years[i])
	results$nonjuv_RAT2[year_block]<-f1(results$nonjuv_RAT2[year_block])
	}
results$nonjuvPop<-round(results$total * results$nonjuv_RAT2/100)

if (species == "BootedE"){
results$maturePop<-results$nonjuvPop
} else{
# prop imm zu ad
dailysums_age$ad_RAT1<-100/(all_imm+all_ad)*all_ad
dailysums_age$ad_RAT1[-aged]<-NA
dailysums_age$ad_RAT2<-dailysums_age$ad_RAT1

results$ad_RAT2<-NA
results$ad_RAT2[which(results$date %in% dailysums_age$date)]<-dailysums_age$ad_RAT2

# wende use nearest value function pro Jahr an
for (i in 1:length(years)){
	year_block<-which(year(results$date) == years[i])
	results$ad_RAT2[year_block]<-f1(results$ad_RAT2[year_block])
	}
results$maturePop<-round(results$nonjuvPop * results$ad_RAT2/100)
#round(sum(results$maturePop)/length(years))
#sum(results$total[is.na(results$test)])
}

} else if (species == "HB" | species == "BK"){
### HB, BK
if (species == "BK") {spec_dataname<-"BlackKite"}else{spec_dataname<-species}
nonjuv<-paste0(species,"_NONJUV")
juv<-paste0(species,"_JUV")
spec<-records[records$species==species | records$species==juv | records$species==nonjuv,]
zones<-which(spec$location=="E1" | spec$location=="O" | spec$location=="W1")
	
dailysums<-tapply(as.numeric(as.character(spec$number)), list(as.Date(spec$date), spec$species), FUN=sum, na.rm=FALSE)
dailysums[is.na(dailysums)]<-0
dailysums<-data.frame(cbind(date=rownames(dailysums),dailysums), row.names=1:length(rownames(dailysums)),stringsAsFactors = FALSE)

dailysums_age<-tapply(as.numeric(as.character(spec$number[zones])), list(as.Date(spec$date[zones]), spec$species[zones]), FUN=sum, na.rm=FALSE)
dailysums_age[is.na(dailysums_age)]<-0
dailysums_age <-data.frame(cbind(date=rownames(dailysums_age),dailysums_age), row.names=1:length(rownames(dailysums_age)),stringsAsFactors = FALSE)

dailysums_age$all1<-as.numeric(eval(parse(text=paste0("dailysums_age$",juv))))+as.numeric(eval(parse(text=paste0("dailysums_age$",nonjuv))))
dailysums_age<-dailysums_age[dailysums_age$all1>0,]

results<-data.frame(date=dailysums_corr_uid$date, test=as.Date("1800-01-01"), total=0)
results$test[which(results$date %in% dailysums_age$date)]<-dailysums_corr_uid$date[which(dailysums_corr_uid$date %in% dailysums_age$date)]
results$total[which(results$date %in% dailysums_corr_uid$date)]<-eval(parse(text=paste0("dailysums_corr_uid$",spec_dataname)))[which(results$date %in% dailysums_corr_uid$date)]

#results$total[which(results$date %in% dailysums_corr_uid$date)]<-
#eval(parse(text=paste0("dailysums_corr_uid$species==",spec_dataname)))
#[which(results$date %in% dailysums_corr_uid$date)]


results$test[results$test=="1800-01-01"]<-NA

### proprtionen altersklassen berechnen
all_nonjuv<-as.numeric(eval(parse(text=paste0("dailysums_age$",nonjuv))))
all_juv<-as.numeric(eval(parse(text=paste0("dailysums_age$",juv))))
all_aged<-all_nonjuv+all_juv
aged<-which(all_aged>=5)
dailysums_age$nonjuv_RAT1<-100/(all_aged)*all_nonjuv
dailysums_age$nonjuv_RAT1[-aged]<-NA
dailysums_age$nonjuv_RAT2<-dailysums_age$nonjuv_RAT1

results$nonjuv_RAT2<-NA
results$nonjuv_RAT2[which(results$date %in% dailysums_age$date)]<-dailysums_age$nonjuv_RAT2

# wende use nearest value function pro Jahr an

years<-unique(year(results$date))
for (i in 1:length(years)){
	year_block<-which(year(results$date) == years[i])
	results$nonjuv_RAT2[year_block]<-f1(results$nonjuv_RAT2[year_block])
	}
results$maturePop<-round(results$total * results$nonjuv_RAT2/100)

#round(sum(results$maturePop)/length(years))
#sum(results$total[is.na(results$test)])

 

} else {print ("no valid species for this script")} 
if (sum(unique(data$filter))>1){results<-results[year(results$date)>=2015,]}
print(paste0(species, ": average mature population per year: ", round(sum(results$maturePop)/length(unique(year(results$date))))))
print(paste0(species, ": average whole population per year: ", round(sum(results$total)/length(unique(year(results$date))))))
#print(sum(results$total[is.na(results$test)]))


