###############################################################
################### BATUMI RAPTOR COUNT #######################
####### Double Count Script ###################################
####### authors: J. Wehrmann, B. Verhelst #####################
####### monitoring@batumiraptorcount.org ######################
####### version: 2.1 ##########################################
###############################################################
################## Instructions ###############################
# install all packages under library before running ###########
# get count data from BRC database NLBIF ######################
# get dc_specs.csv, dc_specs_nonsoaring.csv from BRC Bitbucket#
### modify pathes in line 29, 34, 35, 36, 394, 395 ############
###############################################################

rm(list=ls())

Sys.setenv(TZ="Europe/Berlin")

#### libraries  ####
library(lubridate)
library(rpanel)
library(tcltk2)
library(data.table)
library(beepr)

### directories #####
# modify the path
workdir<-"/Users/Jasper/Documents/Rplayground/data analysis - Bitbucket/data/"
setwd(workdir)

### load data #####
## modify this
count_data<-read.csv("BRC-Bitbucket/CountData.csv", header=TRUE, encoding="UTF-8") # read.csv("final/CLEANED-converted_allyears.csv", header=TRUE, encoding="UTF-8")
dc_specs<-read.csv("BRC-Bitbucket/dc_specs.csv", header=TRUE, encoding="UTF-8") #read.csv("final/dc_specs.csv", header=TRUE, encoding="UTF-8")
dc_specs_nonsoaring<-read.csv("BRC-Bitbucket/dc_specs_nonsoaring.csv", header=TRUE, encoding="UTF-8") #read.csv("final/dc_specs_nonsoaring.csv", header=TRUE, encoding="UTF-8")

if(names(count_data[1])=="X"){count_data[1]<-NULL}

#count_data$year<-year(count_data$date)
# Detect HB phase1 in order to not run the double count check as HB phase 1 counts only from station 1.
HBPhase1Start<-c("2010-08-21 00:00:01 CEST", "2011-08-21 00:00:01 CEST", "2012-08-21 00:00:01 CEST", "2013-08-21 00:00:01 CEST", "2014-08-21 00:00:01 CEST", "2015-08-21 00:00:01 CEST", "2016-08-21 00:00:01 CEST", "2017-08-21 00:00:01 CEST", "2018-08-21 00:00:01 CEST", "2019-08-21 00:00:01 CEST", "2020-08-21 00:00:01 CEST")
HBPhase1End <-c("2010-09-10 00:00:01 CEST", "2011-09-10 00:00:01 CEST", "2012-09-10 00:00:01 CEST", "2013-09-10 00:00:01 CEST", "2014-09-10 00:00:01 CEST", "2015-09-10 00:00:01 CEST", "2016-09-10 00:00:01 CEST", "2017-09-10 00:00:01 CEST", "2018-09-10 00:00:01 CEST", "2019-09-10 00:00:01 CEST", "2020-09-10 00:00:01 CEST")

# pre-format data
count_data$dcdel<-0
count_data$dckept <-0
count_data$dcdelremark<-""
count_data$speciesname<-as.character(count_data$speciesname)
count_data$speciesname<- gsub("-", "_", count_data$speciesname)
count_data$datetime<-paste(count_data$date, count_data$timestamp, sep = " ")
count_data$datetime<-as.POSIXct(count_data$datetime, format="%Y-%m-%d %H:%M:%S")
count_data<-count_data[order(count_data$datetime),]
count_data$id<-seq.int(nrow(count_data))
count_data$year<-as.numeric(count_data$year)

### function to compare details of entries
compare.it <- function(species1,species2,loc1,loc2,s1,s2,a1,a2,p1,p2,n1,n2) {
#for testing
		# species1<-speciesname1[counttypeframe1][dc_specsframe1][i]
		# species2<-speciesname2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p]
		# loc1<-location1[counttypeframe1][dc_specsframe1][i]
		# loc2<-location2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p]
		# s1<-sex1[counttypeframe1][dc_specsframe1][i]
		# s2<-sex2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p]
		# a1<-age1[counttypeframe1][dc_specsframe1][i]
		# a2<-age2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p]
		# p1<-plumage1[counttypeframe1][dc_specsframe1][i]
		# p2<-plumage2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p]
		# n1<-count1[counttypeframe1][dc_specsframe1][i]
		# n2<-count2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] 
		
    if ((n1 != 0 | n2 != 0) & (
    		# compares the potential spational overlap
		(loc1 == "E2" & (loc2 == "W3" | loc2 == "W2" | loc2 == "")) |
		(loc2 == "W2" & (loc1 == "E3" | loc1 == "E2" | loc1 == "")) |
		
		(loc1 == "E3" & (loc2 == "W3" | loc2 == "W2" | loc2 == "W1" | loc2 == "" | 
		(speed == soaring_speed & (loc2 == "O" | loc2 == "E1" | loc2 == "E2")))) |
		
    		(loc2 == "W3" & (loc1 == "E3" | loc1 == "E2" | loc1 == "E1" | loc1 == "" | 
    		(speed == soaring_speed & (loc1 == "O" | loc1 == "W1" | loc1 == "W2"))))
    		)) {	

	# create variables for DC check
	# _mprecise means more precise, _equal means both are same
	spec1_mprecise<-0
	spec2_mprecise<-0
	spec_equal<-0
	age1_mprecise<-0
	age2_mprecise<-0
	age_equal<-0
	sex1_mprecise<-0
	sex2_mprecise<-0
	sex_equal<-0
	sex_different<-0
	plumage1_mprecise<-0
	plumage2_mprecise<-0
	plumge_equal<-0
	plumge_different<-0
	loc1_closer<-0
	loc2_closer<-0
	
specs2<-dc_specs[colnames(dc_specs) == species2]

######## DC CHECK ########
# compare ID level for species
	if(species1 == species2){ #nlevels(specs1[,]) == nlevels(specs2[,])
		spec1_mprecise<-FALSE; spec2_mprecise<-FALSE; spec_equal<-TRUE}  else if(nlevels(specs1[,]) < nlevels(specs2[,])){ #species1 > species2
		spec1_mprecise<-TRUE;  spec2_mprecise<-FALSE; spec_equal<-FALSE} else if(nlevels(specs1[,]) >  nlevels(specs2[,])){
		spec1_mprecise<-FALSE; spec2_mprecise<-TRUE;  spec_equal<-FALSE}

# compare ID level for age
 	if(((a1 == "ad" | a1 == "imm") & a2 =="nonjuv") | (a1 !=  ""   & a2 == "")){ #age1 > age2
    		age1_mprecise<-TRUE;  age2_mprecise<-FALSE; age_equal<-FALSE } else if (((a2 == "ad" | a2 == "imm") & a1 =="nonjuv") | 	(a2 != ""   & a1 == "")){
	   	age1_mprecise<-FALSE; age2_mprecise<-TRUE;  age_equal<-FALSE } else if (a1 == a2){
    		age1_mprecise<-FALSE; age2_mprecise<-FALSE; age_equal<-TRUE	 } else {
    		age1_mprecise<-FALSE; age2_mprecise<-FALSE; age_equal<-FALSE }

# compare ID level for sex
	if(	(s1 != "" & s2 =="") |  	(s1 == "f"  & s2 == "fc")){ #sex1 > sex2
    		sex1_mprecise<-TRUE;  sex2_mprecise<-FALSE; sex_equal<-FALSE; sex_different<-FALSE } else if ((s2 != "" & s1 =="") | (s2 == "f"  & s1 == "fc")){ 
    		sex1_mprecise<-FALSE; sex2_mprecise<-TRUE;  sex_equal<-FALSE; sex_different<-FALSE } else if (s1 == s2){
    		sex1_mprecise<-FALSE; sex2_mprecise<-FALSE; sex_equal<-TRUE; sex_different<-FALSE  } else if (s1 != s2 & s1!="" & s2 != ""){
    		sex1_mprecise<-FALSE; sex2_mprecise<-FALSE; sex_equal<-FALSE; sex_different<-TRUE  } else {
    		sex1_mprecise<-FALSE; sex2_mprecise<-FALSE; sex_equal<-FALSE; sex_different<-FALSE }

# compare ID level for plumage/morph
    if( (p1 != "" & p2 == "")){ #plumage1 > plumage2
    	 	plumage1_mprecise<-TRUE;  plumage2_mprecise<-FALSE; plumage_equal<-FALSE; plumage_different<-FALSE } else if ((p2 != "" & p1 =="")){
		plumage1_mprecise<-FALSE; plumage2_mprecise<-TRUE;  plumage_equal<-FALSE; plumage_different<-FALSE } else if (p1 == p2){
    		plumage1_mprecise<-FALSE; plumage2_mprecise<-FALSE; plumage_equal<-TRUE;  plumage_different<-FALSE } else if (p1 != p2 & p1!="" & p2 != ""){
    	plumage1_mprecise<-FALSE; plumage2_mprecise<-FALSE; plumage_equal<-FALSE; plumage_different<-TRUE  } else {
 		plumage1_mprecise<-FALSE; plumage2_mprecise<-FALSE; plumage_equal<-FALSE; plumage_different<-FALSE }

# compare distance location
	if (loc1 == "E3"){ loc2_closer<-TRUE } else {loc1_closer<-TRUE}
	
# compare results of DC CHECK and keep the records with more precise ID level
	if (#keep 1
		((spec_equal == TRUE | spec1_mprecise == TRUE) & (age1_mprecise == TRUE & (sex1_mprecise == TRUE | sex_equal == TRUE)) & (plumage1_mprecise == TRUE | plumage_equal == TRUE))  | 
		((spec_equal == TRUE | spec1_mprecise == TRUE) & age_equal == TRUE & sex1_mprecise == TRUE & (plumage1_mprecise == TRUE | plumage_equal == TRUE))  | 
		((spec_equal == TRUE | spec1_mprecise == TRUE) & age_equal == TRUE & sex_equal == TRUE & plumage1_mprecise == TRUE)  |
		(spec1_mprecise == TRUE & age_equal == TRUE & sex_equal == TRUE & plumage_equal == TRUE) |
		#new
		(spec_equal == TRUE & age_equal == TRUE & sex_equal == TRUE & plumage_equal == TRUE & loc1_closer == TRUE)
		){keep<-1
		} else if ( #keep 2
		((spec_equal == TRUE | spec2_mprecise == TRUE) & (age2_mprecise == TRUE & (sex2_mprecise == TRUE | sex_equal == TRUE)) & (plumage2_mprecise == TRUE | plumage_equal == TRUE))  | 
		((spec_equal == TRUE | spec2_mprecise == TRUE) & age_equal == TRUE & sex2_mprecise == TRUE & (plumage2_mprecise == TRUE | plumage_equal == TRUE))  | 
		(spec2_mprecise == TRUE & age_equal == TRUE & sex_equal == TRUE & plumage2_mprecise == TRUE)  |
		#new
		(spec_equal == TRUE & age_equal == TRUE & sex_equal == TRUE & plumage_equal == TRUE & loc2_closer == TRUE) 
		){keep<-2 } else { 		keep<-0} #keep both
} else {	keep<-0} #keep both
return (keep)
} ######## end of function

# creating of subsets for double count comparison
for (y in min(count_data$year):max(count_data$year)){
	#subsets
	data<-subset(count_data, count_data$year == y)
	data$count<-as.numeric(data$count)
	station1<-as.data.table(subset(data, telpost==1))
	station2<-as.data.table(subset(data, telpost==2))
	
	# create vectors for each column and station
	datetime1<-station1$datetime
	datetime2<-station2$datetime
	count1<-as.numeric(station1$count)
	count2<-as.numeric(station2$count)
	count1_o<-as.numeric(station1$count)
	count2_o<-as.numeric(station2$count)
	countback1<-as.numeric(station1$countback)
	countback2<-as.numeric(station2$countback)
	countback1[is.na(countback1)]<-0
	countback2[is.na(countback2)]<-0
	speciesname1<-station1$speciesname
	speciesname2<-station2$speciesname
	location1<-as.vector(station1$location)
	location2<-as.vector(station2$location)
	age1<-as.vector(station1$age)
	age2<-as.vector(station2$age)
	sex1<-as.vector(station1$sex)
	sex2<-as.vector(station2$sex)
	plumage1<-as.vector(station1$plumage)
	plumage2<-as.vector(station2$plumage)
	counttype1<-as.vector(station1$counttype)
	counttype2<-as.vector(station2$counttype)
	migtype1<-as.vector(station1$migtype)
	migtype2<-as.vector(station2$migtype)
	remark1<-station1$remark
	remark2<-station2$remark
	dcdel1<-as.numeric(station1$dcdel)
	dcdel2<-as.numeric(station2$dcdel)
	dckept1<-as.numeric(station1$dckept)
	dckept2<-as.numeric(station2$dckept)
	dcdelremark1<-as.vector(station1$dcdelremark)
	dcdelremark2<-as.vector(station2$dcdelremark)
	protocol1<-as.vector(station1$protocol)
	protocol2<-as.vector(station2$protocol)
	id1<-as.vector(station1$id)
	id2<-as.vector(station2$id)
	count1_length<-length(id1)
	dc_number_total<-0
	
	# for progress bar to show calculation progress while running the script
	totalnumber<-sum(count1) + sum(count2)
	pb <- tkProgressBar(title = "Progress Bar", min = 0, max = length(station1$count), width = 600)
	systemtime<-Sys.time() 
	currentduration<-0
	
	##### frame subset for station 1 #####
	# exclude HB records during HB phase 1
	if(unique(station1$year)>2009){
		yearframe<-which(unique(station1$year) == year(HBPhase1Start))
		HBphase1frame1<-which(speciesname1 == "HB" & datetime1 > HBPhase1Start[yearframe] & datetime1 < HBPhase1End[yearframe])
		counttype1[HBphase1frame1]<-"sc"
		
		# exclude HB_age and BK_age records
		HB_BK_age_frame<-! which(speciesname1 == "HB_AD" | speciesname1 == "HB_JUV" | speciesname1 == "BK_AD" | speciesname1 == "BK_NONJUV")
		counttype1[HB_BK_age_frame]<-"sc"
		
		# exclude Station 1 "single count" data
		counttypeframe1<-which(counttype1 != "sc")
	} else{
		counttypeframe1<-c(1:length(counttype1))
		}
	
	#frame for dc_specs
	dc_specsframe1<- which(speciesname1[counttypeframe1] %in% colnames(dc_specs))
	
	# set & attach individual species speed
	soaring_speed<-900 # 900 sec are 15 min
	non_soaring_speed<-600 # 600 sec are 10 min
	allspecies<-data.frame(species=unique(data$speciesname), seconds=soaring_speed)
	allspecies[which(allspecies$species %in% dc_specs_nonsoaring[,]),]$seconds<-non_soaring_speed
	
	for (i in 1:length(count1[counttypeframe1][dc_specsframe1])) {
		#progress bar
		duration<-as.numeric((Sys.time()-systemtime),units="secs")
		currentduration<-currentduration + duration
		avgrestduration<-if(round(currentduration/i*(length(station1$count)-i)/60,0) > 2){paste(round(currentduration/60,1) , "/", round(currentduration/i*(length(station1$count)-i)/60,1),"min")} else {paste(round(currentduration/i*(length(station1$count)-i),0),"sec left")}
		systemtime<-Sys.time()
	
	  	setTkProgressBar(pb, i, title=paste("DoubleCountScript-BRC"), label=paste(y, " Progress: ",round(i/length(station1$count)*100, 0), "% " , avgrestduration, ", detected double counts: ", dc_number_total))
	   	
	  	specs1<-NA;	timeframe2<-NA; specframe2<-NA;counttypeframe2<-NA; zoneframe2<-NA;
	
		### frame subset for station 2 ###
		speed<-allspecies[allspecies$species == speciesname1[counttypeframe1][dc_specsframe1][i],]$seconds
		
		# frames opposite observation by time window
		timeframe2<-which(datetime2 >= datetime1[counttypeframe1][dc_specsframe1][i] - speed & datetime2 <= datetime1[counttypeframe1][dc_specsframe1][i] + speed)
		# frames opposite observation by respective species for potential DC
		specs1<-dc_specs[colnames(dc_specs) == speciesname1[counttypeframe1][dc_specsframe1][i]]
		if (	sum(speciesname2[timeframe2] %in% specs1[1:nrow(specs1),],na.rm=TRUE) > 0){specframe2<-which(speciesname2[timeframe2] %in% specs1[1:nrow(specs1),])}else{specframe2<-NA} 
			
		# exclude Station 2 E3 data
		if (sum(location2[timeframe2][specframe2] != "E3", na.rm=TRUE) > 0){zoneframe2<-which(location2[timeframe2][specframe2] != "E3")}else{zoneframe2<-NA}
		
		# exclude Station 2 "single count" data
		if (unique(station1$year)>2009 & sum(counttype2[timeframe2][specframe2][zoneframe2] != "sc", na.rm=TRUE) > 0){counttypeframe2<-which(counttype2[timeframe2][specframe2][zoneframe2] != "sc")}else if (unique(station1$year)>2009){counttypeframe2<-NA} else {counttypeframe2<-c(1:length(counttype2[timeframe2][specframe2][zoneframe2]))}
		
			
		#start loop to compare details with remaining framed data
		if (sum(!is.na(specframe2), na.rm=TRUE) > 0 & sum(!is.na(zoneframe2), na.rm=TRUE) > 0 & sum(!is.na(counttypeframe2), na.rm=TRUE) > 0 ){
		for (p in 1:length(datetime2[timeframe2][specframe2][zoneframe2][counttypeframe2])){
			keep<-0
			order_frame<-order(as.numeric(datetime1[counttypeframe1][dc_specsframe1][i] - datetime2[timeframe2][specframe2][zoneframe2][counttypeframe2])^2)
			#compare.it (species1,species2,loc1,loc2,s1,s2,a1,a2,p1,p2,n1,n2)
			keep<-compare.it (
			speciesname1[counttypeframe1][dc_specsframe1][i],
			speciesname2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p],
			location1[counttypeframe1][dc_specsframe1][i],
			location2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p],
			sex1[counttypeframe1][dc_specsframe1][i],
			sex2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p],
			age1[counttypeframe1][dc_specsframe1][i],
			age2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p],
			plumage1[counttypeframe1][dc_specsframe1][i],
			plumage2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p],
			count1[counttypeframe1][dc_specsframe1][i],
			count2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] )
			
			dc_max_reached1 <- count1_o[counttypeframe1][dc_specsframe1][i] < (dckept1[counttypeframe1][dc_specsframe1][i] - dcdel1[counttypeframe1][dc_specsframe1][i])
			dc_max_reached2 <- count2_o[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] < (dckept2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] - dcdel2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p])			
			
			if (keep != 0 & !is.na(keep) & dc_max_reached1 == FALSE & dc_max_reached2 == FALSE){
				
				dc_max1 <- count1_o[counttypeframe1][dc_specsframe1][i] - (dckept1[counttypeframe1][dc_specsframe1][i] - dcdel1[counttypeframe1][dc_specsframe1][i])
				dc_max2 <- count2_o[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] - (dckept2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] - dcdel2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p])
				
				dc_number <- min(count2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p], count1[counttypeframe1][dc_specsframe1][i], dc_max1, dc_max2) 
				dc_number_total<-dc_number_total + dc_number
				if(keep == 1 & dc_number > 0){
				#keep records of station1, store DC number in dcdel
				dckept1[counttypeframe1][dc_specsframe1][i] <- dckept1[counttypeframe1][dc_specsframe1][i] + dc_number
				dcdel2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] <- dcdel2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] - dc_number
				#store ID of opposite observation
				
				dcdelremark1[counttypeframe1][dc_specsframe1][i] <- paste0(dcdelremark1[counttypeframe1][dc_specsframe1][i], "ID", id2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p],":", -dc_number," ")
				
				dcdelremark2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] <- paste0(dcdelremark2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p],"ID", id1[counttypeframe1][dc_specsframe1][i], ":", dc_number," ")	
				
				#change count number of opposite station
				count2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p]<- count2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] - dc_number
				
			} else if (keep == 2 & dc_number > 0){
				#keep records of station2, store DC number in dcdel
				dcdel1[counttypeframe1][dc_specsframe1][i] <- dcdel1[counttypeframe1][dc_specsframe1][i] - dc_number
				dckept2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] <- dckept2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] + dc_number
				#store ID of opposite observation
				dcdelremark1[counttypeframe1][dc_specsframe1][i] <- paste0(dcdelremark1[counttypeframe1][dc_specsframe1][i], "ID", id2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p], ":", dc_number," ")
				
				dcdelremark2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p] <- paste0(dcdelremark2[timeframe2][specframe2][zoneframe2][counttypeframe2][order_frame][p],"ID", id1[counttypeframe1][dc_specsframe1][i],":", -dc_number," ")	
				
				#change count number of opposite station	
				count1[counttypeframe1][dc_specsframe1][i]<- count1[counttypeframe1][dc_specsframe1][i] - dc_number
				
			}else {keep<-3}
			}}}}
	
	sum(dcdel1)
	sum(dcdel2)
	sum(count1)
	sum(count2)
	
	#create table for all
	date1<-as.Date(datetime1)
	date2<-as.Date(datetime2)
	timestamp1<-format(datetime1,"%H:%M:%S")
	timestamp2<-format(datetime2,"%H:%M:%S")
	new_station1<-data.frame(id1, date1, timestamp1, speciesname1, count1, count1_o, countback1, station=1, location1, age1, sex1, plumage1, counttype1, migtype1, remark1, dckept1, dcdel1, dcdelremark1, protocol1)
	new_station2<-data.frame(id2, date2, timestamp2, speciesname2, count2, count2_o, countback2, station=2, location2, age2, sex2, plumage2, counttype2, migtype2, remark2, dckept2, dcdel2, dcdelremark2, protocol2)
	header<-c("id", "date", "time", "species", "number_new", "number_ori", "numberN", "station", "location", "age", "sex", "morph", "ctype", "mtype", "remark", "dckept", "dcdel", "dcremark", "protocol")
	colnames(new_station1)<-header
	colnames(new_station2)<-header
	new_count<-rbind(new_station1, new_station2)
	rownames(new_count)<-new_count$id
	new_count<-new_count[order(new_count$id),]
	
	dcdel_spec_add1<-tapply(dcdel1>0, speciesname1, FUN=sum, na.rm=TRUE)
	dcdel_spec_add2<-tapply(dcdel2>0, speciesname2, FUN=sum, na.rm=TRUE)
	dcdel_spec_sub1<--tapply(dcdel1<0, speciesname1, FUN=sum, na.rm=TRUE)
	dcdel_spec_sub2<--tapply(dcdel2<0, speciesname2, FUN=sum, na.rm=TRUE)
	dc_spec_table<-transpose(data.frame(rbind(add=dcdel_spec_add1, sub=dcdel_spec_sub1)))
	colnames(dc_spec_table)<-c("add","sub")
	
	dcdel_tab_add<-tapply(new_count$dcdel>0, list(new_count$species, new_count$station), FUN=sum, na.rm=TRUE)
	dcdel_tab_sub<--tapply(new_count$dcdel<0, list(new_count$species, new_count$station), FUN=sum, na.rm=TRUE)
	dcdel_tab_sum<-data.frame(tapply(new_count$number_new, list(new_count$species, new_count$station), FUN=sum, na.rm=TRUE))
	dcdel_tab_sum$species<-rownames(dcdel_tab_sum)
	dcdel_tab_sum<-dcdel_tab_sum[order(dcdel_tab_sum$species),]
	
	original_count_tab_sum<-data.frame(tapply(data$count, list(data$speciesname, data$telpost), FUN=sum, na.rm=TRUE))
	original_count_tab_sum$species<-rownames(original_count_tab_sum)
	original_count_tab_sum<-original_count_tab_sum[order(original_count_tab_sum$species),]
	dcdel_tab<-cbind(dcdel_tab_add, dcdel_tab_sub,dcdel_tab_sum)
	
	dcdel_tab<-data.frame(species=original_count_tab_sum$species, dcdel1=dcdel_tab_sum$X1-original_count_tab_sum$X1, dcdel2=dcdel_tab_sum$X2-original_count_tab_sum$X2)
	dcdel_tab[is.na(dcdel_tab)]<-0
	
	this_year<-unique(year(new_count$date))
	dc_records<-length(which(new_count$ctype == "dc"))
	dc_detected<-length(which(new_count$ctype == "dc" & new_count$dcremark != ""))
	dc_quality<-paste0(this_year,": ", round(100/dc_records*dc_detected,1)," %")
	quality<-paste0(this_year," - marked / detected records: ",dc_records," / ", dc_detected, " ### DC Quality: ",dc_quality)
	
	############ data check ####################
	new_count<-as.data.table(new_count)
	if(nrow(new_count[new_count$number_o < new_count$dckept - new_count$dcdel,]) > 0){print(paste(this_year," - ERROR ALERT: NUMBER_O > DCKEPT - DCDEL"))} else if(sum(new_count$dckept + new_count$dcdel) > 0){print(paste(this_year," - ERROR ALERT: dckept + dcdel > 0"))} else {print(quality)}
	############################################
	
	if (exists('quality_all')){quality_all<-c(quality_all, quality)} else {quality_all<-quality}
	if (exists('new_count_all_data')){new_count_all_data<-rbind(new_count_all_data,new_count)} else {new_count_all_data<-new_count}
	
#	write.csv(new_count, paste0("final/1-new_count_DC_",unique(year(new_count$date)),".csv"), fileEncoding ="UTF-8", row.names=FALSE)
	write.csv(dcdel_tab, paste0("final/1-dcdel_tab_", unique(year(new_count$date)),".csv"), fileEncoding ="UTF-8", row.names=FALSE)
#	write.csv(quality, paste0("final/1-quality_", unique(year(new_count$date)),".txt"), fileEncoding ="UTF-8")
	
	close(pb)
} 
# end of subset and comparison loop

names(new_count_all_data)
names(new_count_all_data)[5]<-"number"
names(new_count_all_data)[6]<-"raw"
names(new_count_all_data)[7]<-"north" 
new_count_all_data$remark[which(is.na(new_count_all_data$remark) == T)]<-""

### write results
#modify path
write.csv(new_count_all_data, paste0("BRC-Bitbucket/PROCESSED-allyears.csv"), fileEncoding ="UTF-8", row.names=FALSE) #paste0("final/PROCESSED-allyears.csv"), fileEncoding ="UTF-8", row.names=FALSE)
write.csv(quality_all, paste0("BRC-Bitbucket/quality_allyears.txt"), fileEncoding ="UTF-8") #paste0("final/quality_allyears.txt"), fileEncoding ="UTF-8")

beep("complete")


