###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script sets for compiling count data ################
############### Detect double counts ##########################
####### authors: B. Verhelst, J. Wehrmann #####################

############################ Instructions ################################
# runs only under BRC formatted data
# see BRC-oldDataConverter.R for details

# todo's: 
# order tablespecs by taxonomy ID, 
# adjust abbreviations
# adjust headertitles
# remove German - change to English

rm(list=ls())

#### libraries  ####
library(lubridate)
library(rpanel)
library(tcltk2)



### directories #####
# modify the path
workdir<-"data/in"
outputdir<-"data/out"

### load data #####
setwd(workdir)
# modify the file name, make sure the csv is encoded in UTF-8!!
full_data<-read.csv("2016_prefinal.csv", header=TRUE, encoding="UTF-8")
dc_table<-read.csv("dc_table.csv", header=TRUE, encoding="UTF-8")

############################################
data<-full_data
for (k in 1:length(names(dc_table))){
	for (m in 1:length(dc_table[k])){
	dc_table[k][m,]
	dc_table[5][2:3,]
	names(dc_table[5])[1:2,]
	vergleiche 1 names(dc_table[k]) mit 2 dc_table[k][m,]
	# alle von 1
	data[data$speciesname==names(dc_table[k]) & data$telpost==1, ]
	# alle von 2
	data[data$speciesname==dc_table[5][1:2,] & data$telpost==2, ]
	
	}
}






############################################

### data preparation #####
full_data$speciesname<-as.character(full_data$speciesname)
#delete NA
full_data[is.na(full_data)]<-""

records<-full_data
records$record<-seq(1,length(records$date), 1)
records$count<-as.numeric(records$count)
records$count[which(is.na(records$count) == T)]<-1
records$time2<-paste(records$date, records$timestamp, sep = " ")
records$time2<-as.POSIXlt(records$time2, format="%Y-%m-%d %H:%M:%S")
records$year<-year(records$time2)
records$yday<-yday(records$time2)
records$lab<-paste(records$year, records$yday)
speclist<-unique(records$speciesname)

#subsets
station1<-subset(records, telpost==1)
station2<-subset(records, telpost==2)
time1<-station1$time2
time2<-station2$time2
a1<-length(station1$record)
a2<-length(station2$record)
b1<-sum(station1$count)
b2<-sum(station2$count)

#for plot counter
#x_axis<-c(1,2,3,4)
#y_axis<-c(1,2,3,4)

# for DC
v<-0
w<-0
removed<-0

location1<-station1$location
location2<-station2$location
location2list<-location2
number1<-station1$count
number2<-station2$count
species1<-as.vector(station1$speciesname)
species2<-as.vector(station2$speciesname)
species2list<-species2
time2list<-time2
age1<-as.vector(station1$age)
age2<-as.vector(station2$age)
age2list<-age2
sex1<-as.vector(station1$sex)
sex2<-as.vector(station2$sex)
sex2list<-sex2
migtype1<-as.character(station1$migtype)
migtype2<-as.character(station2$migtype)
migtype2list<-migtype2
plumage1<-as.vector(station1$plumage)
plumage2<-as.vector(station2$plumage)
plumage2list<-plumage2

#counttype keep for output file
counttype1<-station1$counttype
counttype2<-station2$counttype
counttype2list<-counttype2

RDC1<-as.vector(station1$counttype == "dc")
RDC2<-as.vector(station2$counttype == "dc")
RDC2list<-RDC2

SC1<-as.vector(station1$counttype == "sc")
SC2<-as.vector(station2$counttype == "sc")
SC2list<-SC2

remark1<-as.vector(station1$remark)
remark2<-as.vector(station2$remark)
remark2list<-remark2
rest1<-as.vector(station1$count)
rest2<-as.vector(station2$count)
index2<-seq(1,length(time2list),1)
index2list<-index2
DCDEL1<-rep(0, length(number1))
DCDEL2<-rep(0, length(number2))
DCDEL1<-as.numeric(DCDEL1)
DCDEL2<-as.numeric(DCDEL2)

# for progress bar to show calculation progress while running the script
Totalnumber<-sum(number1)+sum(number2)
pb <- tkProgressBar(title = "Progress Bar", min = 0, max = a1, width = 600)
systemtime<-Sys.time() 
currentduration<-0

# raptors are seprated into three groups
# nonflockingspecies are considered to usually not promote flocking and thermaling behaviour resulting in faster transpassing of the bottleneck. 
# 	-> data comparison during a shorter timeframe
# smallspecies are considered to be undetectable on large distance in the bottleneck. 
#	-> data comparison in a smaller spatial zone
# all others do not belong to either of these two groups

nonflockingspecies<-(c("EleonoraF","Harrier-SPEC", "Hen", "Hobby", "Kestrel-SPEC", "Lanner", "LargeFALCON", "Mar", "Mon", "MonPalHen", "Osprey", "Pal", "Peregrine", "Roller", "Saker", "StockD", "TurtleD", "WoodP", "Merlin", "Goshawk", "CommonKes", "LesserKes", "Falcon-SPEC"))

smallspecies<-(c("EleonoraF","Harrier-SPEC", "Hen", "Lanner", "LargeFALCON", "Mar", "Mon", "MonPalHen", "Pal", "Peregrine", "Roller", "Saker", "StockD", "TurtleD", "WoodP", "RedFF", "Hobby", "Hobby/RedFF", "Kestrel-SPEC", "Merlin", "EurasianSH", "LevantSH", "SparrowH-SPEC", "Goshawk", "SparrowH/Goshawk", "CommonKes", "LesserKes", "Falcon-SPEC"))

# Detect HB phase1 in order to not run the double count check as HB phase 1 counts only from station 1. 
# Detect HB phase2 in order to run the double count check as then HB get counted from both stations.
HBPhase1Year <-c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
HBPhase1Start<-c("2008-08-21 00:00:01 CEST", "2009-08-21 00:00:01 CEST", "2010-08-21 00:00:01 CEST", "2011-08-21 00:00:01 CEST", "2012-08-21 00:00:01 CEST", "2013-08-21 00:00:01 CEST", "2014-08-21 00:00:01 CEST", "2015-08-21 00:00:01 CEST", "2016-08-21 00:00:01 CEST", "2017-08-21 00:00:01 CEST", "2018-08-21 00:00:01 CEST", "2019-08-21 00:00:01 CEST", "2020-08-21 00:00:01 CEST")
HBPhase1End <-c("2008-09-10 00:00:01 CEST", "2009-09-10 00:00:01 CEST", "2010-09-10 00:00:01 CEST", "2011-09-10 00:00:01 CEST", "2012-09-10 00:00:01 CEST", "2013-09-10 00:00:01 CEST", "2014-09-10 00:00:01 CEST", "2015-09-10 00:00:01 CEST", "2016-09-10 00:00:01 CEST", "2017-09-10 00:00:01 CEST", "2018-09-10 00:00:01 CEST", "2019-09-10 00:00:01 CEST", "2020-09-10 00:00:01 CEST")

if (year(time1)[1] %in% HBPhase1Year){
	for (t in 1:length(HBPhase1Year)){
		if (year(time1)[t] == HBPhase1Year[t]){
			STARTtimeHBPhase1<-as.POSIXct(HBPhase1Start[t], format="%Y-%m-%d %H:%M:%S")
			ENDtimeHBPhase1<-as.POSIXct(HBPhase1End[t], format="%Y-%m-%d %H:%M:%S")
		}
	}
} else {warning ("add your date to HBPhase1Year, HBPhase1Start, HBPhase1End")	}
	
### DOUBLE COUNT CHECK ########
for (i in 1:a1){
	# for progress bar
	duration<-as.numeric((Sys.time()-systemtime),units="secs")
	currentduration<-currentduration+duration
	avgrestduration<-round(currentduration/i*(a1-i)/60,0)
	systemtime<-Sys.time()
   	setTkProgressBar(pb, i, label=paste( year(time1)[1]," | done: ",round(i/a1*100, 0), "% | time remaining: ", avgrestduration, "min | detected DC: ",removed))

	# TIMEFRAME for transpassing duration gets set
	# select relevant records by ID based on time, behaviour
	if (species1[i] %in% nonflockingspecies){
		kappa<-which(time2list > (time1[i]-600)& 
		time2list < (time1[i]+600))
	} else {
		kappa<-which(time2list > (time1[i]-900)& 
		time2list < (time1[i]+900))
	}
#########################################
	### select more options for kappa 
	## playground
	#kappa<-which(
	#	 (station2$time2 > (station1$time2[i]-3000)
	#& 	 station2$time2 < (station1$time2[i]+3000))
	#	)
		
	# add station2$species %in% species_comparison$
	#species_comparison<-data.frame("id"=0)
	#species_comparison$Mon<-c("MonPalHen", "Harrier-SPEC", "MediumRaptor", "Raptor-SPEC")
	
	

#########################################

	
	# SPATIALZONE gets activated for smallspecies, DO NOT CHANGE, later only the large species (size==1) are considered for double count checking in farther distance zones.
	if (species1[i] %in% smallspecies){ size<-0} else{	size<-1}
	# consider species also for station 2


	if(
		length(kappa)>0 &
		SC1[i]=="FALSE" &
		number1[i] > 0 &
	 	(species1[i]!="HB" | (species1[i]=="HB" & (time1[i]< STARTtimeHBPhase1 | time1[i]> ENDtimeHBPhase1))) &
	 	species1[i]!="SHOT" &
	 	species1[i]!="BK_NONJUV" &
	 	species1[i]!="BK_JUV" & 
	 	species1[i]!="HB_AD" &
	 	species1[i]!="HB_JUV")
	 {
		time2<-time2list[kappa]
		time2<-time2+ceiling(rnorm(length(time2),30,15))
		# should be ordered by increasing time diff
		diff<-abs(time1[i]-time2) 		
		species2<-species2list[kappa][order(diff)]
		sex2<-sex2list[kappa][order(diff)]
		age2<-age2list[kappa][order(diff)]
		plumage2<-plumage2list[kappa][order(diff)]
		location2<-location2list[kappa][order(diff)]
		index2<-index2list[kappa][order(diff)]	
		SC2<-SC2list[kappa][order(diff)]

	for (j in 1:length(time2)){
		if (
			number2[j] > 0 & 
			SC2[j] =="FALSE" &
			species2[j]!="SHOT" & 
			species2[j]!="BK_NONJUV" &
			species2[j]!="BK_JUV" & 
			species2[j]!="HB_AD" &  
			species2[j]!="HB_JUV")		
		{
			### CHECKS RECORD of station 1 with station 2. Any changes must be done mirrorwise in section station 2 checks station 1
			# checks species records (station1) with same species and lower ID-levels (station2)
			if	((
				(species1[i]==species2[j]) |
				(species1[i]=="Mon" & species2[j]=="MonPalHen") |
				(species1[i]=="Pal" & species2[j]=="MonPalHen")|
				(species1[i]=="Hen" & species2[j]=="MonPalHen")|
				(species1[i]=="Mon" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="Mar" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="Hen" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="Pal" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="HB" & species2[j]=="Buzzard-SPEC")|
				(species1[i]=="SB" & species2[j]=="Buzzard-SPEC")|
				(species1[i]=="SB" & species2[j]=="MediumRaptor") |
				(species1[i]=="HB" & species2[j]=="MediumRaptor")|
				(species1[i]=="BlackKite" & species2[j]=="MediumRaptor")|
				(species1[i]=="Mar" & species2[j]=="MediumRaptor")|
				(species1[i]=="Mon" & species2[j]=="MediumRaptor")|
				(species1[i]=="Pal" & species2[j]=="MediumRaptor")|
				(species1[i]=="Hen" & species2[j]=="MediumRaptor")|
				(species1[i]=="BootedE" & species2[j]=="MediumRaptor")|
				(species1[i]=="SteppeE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="GreaterSE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="SteppeE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="LesserSE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="ImperialE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="WhiteTE" & species2[j]=="LargeEAGLE")|
				
				(species1[i]=="MonPalHen" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="MonPalHen" & species2[j]=="MediumRaptor")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="MediumRaptor")|
				(species1[i]=="Buzzard-SPEC" & species2[j]=="MediumRaptor")|
				
				(species1[i]=="CommonKes" & species2[j]=="Kestrel-SPEC")|
				(species1[i]=="LesserKes" & species2[j]=="Kestrel-SPEC")|
				(species1[i]=="Hobby" & species2[j]=="Hobby/RedFF")|
				(species1[i]=="RedFF" & species2[j]=="Hobby/RedFF")|
				(species1[i]=="Peregrine" & species2[j]=="LargeFALCON")|
				(species1[i]=="Saker" & species2[j]=="LargeFALCON")|
				(species1[i]=="Lanner" & species2[j]=="LargeFALCON")|
				#for 2008 and 09
				(species1[i]=="CommonKes" & species2[j]=="Falcon-SPEC")|
				(species1[i]=="LesserKes" & species2[j]=="Falcon-SPEC")|
				(species1[i]=="Hobby" & species2[j]=="Falcon-SPEC")|
				(species1[i]=="RedFF" & species2[j]=="Falcon-SPEC")|
				(species1[i]=="Peregrine" & species2[j]=="Falcon-SPEC")|
				(species1[i]=="Saker" & species2[j]=="Falcon-SPEC")|
				(species1[i]=="Lanner" & species2[j]=="Falcon-SPEC")|
				(species1[i]=="EurasianSH" & species2[j]=="SparrowH-SPEC")|
				(species1[i]=="LevantSH" & species2[j]=="SparrowH-SPEC")|
				(species1[i]=="EurasianSH" & species2[j]=="SparrowH/Goshawk")|
				(species1[i]=="LevantSH" & species2[j]=="SparrowH/Goshawk")|
				(species1[i]=="Goshawk" & species2[j]=="SparrowH/Goshawk")|
				(species1[i]=="SparrowH-SPEC" & species2[j]=="SparrowH/Goshawk")|
	
				(species1[i]=="BlaStork" & species2[j]=="Stork-SPEC")|
				(species1[i]=="WhiStork" & species2[j]=="Stork-SPEC"))&
	 
				# IMPORTANT to keep this hirachie of DISTANCE ZONES during the comparison process, the main overlap zone needs to be checked at first and so on.
				( 
				# for all species until 1st ZONE 1 of the other station
				(location1[i]=="E2" & (location2[j]=="W2" | location2[j]=="W3"))| # MAIN OVERLAP ZONE
				(location1[i]=="E3" & (location2[j]=="W2" | location2[j]=="W1")) | # 

				# for larger species (size==1) until opposite ZONE 2 of the other station
				(size==1 & location1[i]=="E3" & (location2[j]=="E2" | location2[j]=="E1" | location2[j]=="O")) | # 
	 			(size==1 & (location1[i]=="O" | location1[i]=="W1" | location1[i]=="W2") & location2[j]=="W3") #  
				)&
				
				# Check AGE & SEXES.
				# keeps the higher ID details. 
				# hierarchie in script IMPORTANT
				(
				(age1[i]==age2[j]) |
				(age1[i]=="ad" & age2[j]=="nonjuv")|
				(age1[i]=="imm" & age2[j]=="nonjuv")|
				(age1[i]=="ad" & age2[j]=="")|
				(age1[i]=="imm" & age2[j]=="")|
				(age1[i]=="nonjuv" & age2[j]=="")|
				(age1[i]=="juv" & age2[j]=="")
				) & (
				(sex1[i]==sex2[j]) |
				(sex1[i]=="f" & sex2[j]=="fc")|
	 			(sex1[i]=="f" & sex2[j]=="")|
	 			(sex1[i]=="fc" & sex2[j]=="")|
	 			(sex1[i]=="m" & sex2[j]=="")
				) & (
				(plumage1[i]==plumage2[j]) |
				(plumage1[i]=="light" & plumage2[j]=="")|
	 			(plumage1[i]=="dark" & plumage2[j]=="")				
				) &	
				number2[index2[j]]>0 & rest1[i]>0 & rest2[index2[j]]>0)				
				{
				# as result of the previous comparison the data gets changed here.
				# detected double counts number gets substracted from the record with lower ID-level. The substraction is recorded in the columns DCDEL1/2. The minor ID level receives a substraction and the major ID level receives an addition.
				v<-min(number2[index2[j]],rest1[i])
				number2[index2[j]]<-(number2[index2[j]]-v)
				DCDEL2[index2[j]]<-DCDEL2[index2[j]]-v
				DCDEL1[i]<-DCDEL1[i]+v
				rest1[i]<-(rest1[i]-v)
				rest2[index2[j]]<-(rest2[index2[j]]-v)
				removed<-removed+v
				
				### CHECKS RECORD of station 2 with station 1
				# checks species records (station2) with same species and lower ID-levels (station1)

				} else if ((
				(species1[i]==species2[j]) |
				(species1[i]=="MonPalHen" & species2[j]=="Mon") |
				(species1[i]=="MonPalHen" & species2[j]=="Pal") |
				(species1[i]=="MonPalHen" & species2[j]=="Hen")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="Mon")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="Pal")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="Hen")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="Mar")|
				(species1[i]=="Buzzard-SPEC" & species2[j]=="HB")|
				(species1[i]=="Buzzard-SPEC" & species2[j]=="SB")|
				(species1[i]=="MediumRaptor" & species2[j]=="SB") |
				(species1[i]=="MediumRaptor" & species2[j]=="HB") |
				(species1[i]=="MediumRaptor" & species2[j]=="BlackKite")|
				(species1[i]=="MediumRaptor" & species2[j]=="Mar")|
				(species1[i]=="MediumRaptor" & species2[j]=="Mon")|
				(species1[i]=="MediumRaptor" & species2[j]=="Pal")|
				(species1[i]=="MediumRaptor" & species2[j]=="Hen")|
				(species1[i]=="MediumRaptor" & species2[j]=="BootedE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="LesserSE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="GreaterSE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="SteppeE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="ImperialE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="WhiteTE")|
				
				(species1[i]=="Harrier-SPEC" & species2[j]=="MonPalHen")|
												
				(species1[i]=="MediumRaptor" & species2[j]=="MonPalHen")|
				(species1[i]=="MediumRaptor" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="MediumRaptor" & species2[j]=="Buzzard-SPEC")|
				
				(species1[i]=="Kestrel-SPEC" & species2[j]=="CommonKes")|
				(species1[i]=="Kestrel-SPEC" & species2[j]=="LesserKes")|
				(species1[i]=="Hobby/RedFF" & species2[j]=="Hobby")|
				(species1[i]=="Hobby/RedFF" & species2[j]=="RedFF")|
				(species1[i]=="LargeFALCON" & species2[j]=="Peregrine")|
				(species1[i]=="LargeFALCON" & species2[j]=="Saker")|
				(species1[i]=="LargeFALCON" & species2[j]=="Lanner")|
				## 2008/09
				(species1[i]=="Falcon-SPEC" & species2[j]=="CommonKes")|
				(species1[i]=="Falcon-SPEC" & species2[j]=="LesserKes")|
				(species1[i]=="Falcon-SPEC" & species2[j]=="Hobby")|
				(species1[i]=="Falcon-SPEC" & species2[j]=="RedFF")|
				(species1[i]=="Falcon-SPEC" & species2[j]=="Peregrine")|
				(species1[i]=="Falcon-SPEC" & species2[j]=="Saker")|
				(species1[i]=="Falcon-SPEC" & species2[j]=="Lanner")|
				(species1[i]=="SparrowH-SPEC" & species2[j]=="EurasianSH")|
				(species1[i]=="SparrowH-SPEC" & species2[j]=="LevantSH")|
				(species1[i]=="SparrowH/Goshawk" & species2[j]=="EurasianSH")|
				(species1[i]=="SparrowH/Goshawk" & species2[j]=="LevantSH")|
				(species1[i]=="SparrowH/Goshawk" & species2[j]=="Goshawk")|
				(species1[i]=="SparrowH/Goshawk" & species2[j]=="SparrowH-SPEC")|
				
				(species1[i]=="Stork-SPEC" & species2[j]=="BlaStork")|
				(species1[i]=="Stork-SPEC" & species2[j]=="WhiStork"))& 
	
				(
				(location2[j]=="W2" & location1[i]=="E2") | # 
				(location2[j]=="W3" & (location1[i]=="E2" | location1[i]=="E1")) |#
								
				(size==1 & location2[j]=="W3" & (location1[i]=="O" | location1[i]=="W1" | location1[i]=="W2")) | 
				(size==1 & (location2[j]=="O" | location2[j]=="W1" | location2[j]=="W2") & location1[i]=="E3") 
				
				)&
				(
				(age1[i]==age2[j]) |
				(age1[i]=="nonjuv" & age2[j]=="ad")|
				(age1[i]=="nonjuv" & age2[j]=="imm")|
				(age1[i]=="" & age2[j]=="ad") |
				(age1[i]=="" & age2[j]=="imm") |
				(age1[i]=="" & age2[j]=="nonjuv") |
				(age1[i]=="" & age2[j]=="juv")
				) & (
				(sex1[i]==sex2[j]) |
				(sex1[i]=="fc" & sex2[j]=="f")|
				(sex1[i]=="" & sex2[j]=="m") |
				(sex1[i]=="" & sex2[j]=="f") |
				(sex1[i]=="" & sex2[j]=="fc")		
	 			) & (
				(plumage1[i]==plumage2[j]) |
				(plumage1[i]=="" & plumage2[j]=="light")|
				(plumage1[i]=="" & plumage2[j]=="dark") 			
	 			) &
				number1[i]>0 & rest2[index2[j]]>0 & rest1[i]>0)		
				{
				## HB phase1, number of HB keeps untouched on station 1. Data from station2 gets checked and substracted if double counts are detected.
			if (species2[j]=="HB" & time2[j]> STARTtimeHBPhase1 & time2[j]< ENDtimeHBPhase1) 
				{
					v<-min(number2[index2[j]],rest1[i])
					number2[index2[j]]<-(number2[index2[j]]-v)
					DCDEL2[index2[j]]<-DCDEL2[index2[j]]-v
					DCDEL1[i]<-DCDEL1[i]+v
					rest1[i]<-(rest1[i]-v)
					rest2[index2[j]]<-(rest2[index2[j]]-v)
					removed<-removed+v
				} else {
					w<-min(number1[i],rest2[index2[j]])
					number1[i]<-number1[i]-w
					DCDEL1[i]<-DCDEL1[i]-w
					DCDEL2[index2[j]]<-DCDEL2[index2[j]]+w
					rest2[index2[j]]<-rest2[index2[j]]-w
					rest1[i]<-rest1[i]-w
					removed<-removed+w
				}	
				}}
			}}
}

species1<-as.factor(species1)
species2list<-as.factor(species2list)
total=sum(number1)+sum(number2)

Site1<-rep("1", length(time1))
a<-data.frame(time1, Site1, species1, number1, age1, sex1, plumage1, location1, migtype1, counttype1, DCDEL1, remark1, stringsAsFactors = FALSE) #speciesid1,

Site2<-rep("2", length(time2list))
b<-data.frame( time2list, Site2, species2list, number2, age2list, sex2list, plumage2list, location2list, migtype2list, counttype2list, DCDEL2, remark2list, stringsAsFactors = FALSE) #speciesid2list

names(b)[1]<-"time1"
names(b)[2]<-"Site1"
names(b)[3]<-"species1"
names(b)[4]<-"number1"
names(b)[5]<-"age1"
names(b)[6]<-"sex1"
names(b)[7]<-"plumage1"
names(b)[8]<-"location1"
names(b)[9]<-"migtype1"
names(b)[10]<-"counttype1"
names(b)[11]<-"DCDEL1"
names(b)[12]<-"remark1"
newcount<-rbind(a,b)

colnames(newcount)[1] <-"datetime"
colnames(newcount)[2] <-"station"
colnames(newcount)[3] <-"species"
colnames(newcount)[4] <-"number"
colnames(newcount)[5] <-"age"
colnames(newcount)[6] <-"sex"
colnames(newcount)[7] <-"plumage"
colnames(newcount)[8] <-"location"
colnames(newcount)[9] <-"migtype"
colnames(newcount)[10] <-"counttype"
colnames(newcount)[11] <-"DCDEL"
colnames(newcount)[12] <-"remark"

recordsB<-newcount

timeB<-paste(recordsB$date, recordsB$time, sep = " ")
recordsB$timeB<-as.POSIXlt(timeB, format="%Y-%m-%d %H:%M:%S")
recordsB<-recordsB[order(recordsB$timeB),]
recordsB$year<-year(recordsB$timeB)
recordsB$yday<-yday(recordsB$timeB)

# # create table
# tablespecs<-data.frame(species = speclist, aDC = rep(0, length(speclist)), bDC = rep(0, length(speclist)), DEL = rep(0, length(speclist)))
# tablespecs$species<-as.character(tablespecs$species)

# #bDC
# counta<-tapply(records$count, records$speciesname, sum)
# for (z in 1: length(counta)){
# tablespecs[which(tablespecs[,1] == names(counta)[z]),u+2]<-counta[z]}

# #aDC
# subsetb<-recordsB
# countb<-tapply(subsetb$number, subsetb$species, sum)
# for (z in 1: length(countb)){
# tablespecs[which(tablespecs[,1] == names(countb)[z]),u+1]<-countb[z]}

#DEL & DELREL
#tablespecs$DEL<-tablespecs$aDC - tablespecs$bDC
#tablespecs$bDC<-NULL
#tablespecs$DELREL<-round(100/tablespecs$aDC*tablespecs$DEL, digits=0)

# order by taxonomy # taxoID missing, speciesID not taxonomical
#speciesids<-data.frame(specid=unique(records$speciesid), specname=unique(records$speciesname))
#speciesids[order(speciesids$specid),]
#tablespecs<-tablespecs[with(tablespecs, order(speciesid)), ]

recordsB$timeB <- NULL
setwd(outputdir)
write.csv(recordsB, "1-newcount.csv")
#write.csv(tablespecs, "1-tablespecsDC.csv")
close(pb)