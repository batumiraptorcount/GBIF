###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script sets for compiling count data ################
############### Detect double counts ##########################
####### authors: B. Verhelst, J. Wehrmann #####################

############################ Instructions ################################
# runs only under BRC formatted data
# see BRC-oldDataConverter.R for details
# todo's: order tablespecs by taxonomy ID

rm(list=ls())

#### libraries  ####
library(lubridate)
library(tcltk)
library(rpanel)


### directories #####
# modify the path
workdir<-"/Users/jasper 1/Documents/Rplayground/data/in"
outputdir<-"/Users/jasper 1/Documents/Rplayground/data/out"

### load data #####
setwd(workdir)
# modify the file name, make sure the csv is encoded in UTF-8!!
full_data<-read.csv("2015-RAW-BRC_tab.csv", header=TRUE, encoding="UTF-8")

### data preparation #####
full_data$species<-as.character(full_data$species)
records<-full_data
records$record<-seq(1,length(records$date), 1)
records$number<-as.numeric(records$number)
records$number[which(is.na(records$number) == T)]<-1
records$time2<-paste(records$date, records$time, sep = " ")
records$time2<-as.POSIXlt(records$time2, format="%Y-%m-%d %H:%M:%S")
records$year<-year(records$time2)
records$yday<-yday(records$time2)
records$lab<-paste(records$year, records$yday)
speclist<-unique(records$species)

#subsets
station1<-subset(records, station==1)
station2<-subset(records, station==2)
time1<-station1$time2
time2<-station2$time2
a1<-length(station1$record)
a2<-length(station2$record)
b1<-sum(station1$number)
b2<-sum(station2$number)

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
number1<-station1$number
number2<-station2$number
species1<-as.vector(station1$species)
species2<-as.vector(station2$species)
species2list<-species2
time2list<-time2
age1<-as.vector(station1$age)
age2<-as.vector(station2$age)
age2list<-age2
sex1<-as.vector(station1$sex)
sex2<-as.vector(station2$sex)
sex2list<-sex2
health1<-as.character(station1$health)
health2<-as.character(station2$health)
health2list<-health2
morph1<-as.vector(station1$morph)
morph2<-as.vector(station2$morph)
morph2list<-morph2

#counttype keep for output file
counttype1<-station1$counttype
counttype2<-station2$counttype
counttype2list<-counttype2

RDC1<-as.vector(station1$counttype == "DC")
RDC2<-as.vector(station2$counttype == "DC")
RDC2list<-RDC2

SC1<-as.vector(station1$counttype == "SC")
SC2<-as.vector(station2$counttype == "SC")
SC2list<-SC2

remark1<-as.vector(station1$remark)
remark2<-as.vector(station2$remark)
remark2list<-remark2
rest1<-as.vector(station1$number)
rest2<-as.vector(station2$number)
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
bisherigedauer<-0

# raptors are seprated into three groups
# nonflockingspecies are considered to usually not promote flocking and thermaling behaviour resulting in faster transpassing of the bottleneck. 
# 	-> data comparison during a shorter timeframe
# smallspecies are considered to be undetectable on large distance in the bottleneck. 
#	-> data comparison in a smaller spatial zone
# all others do not belong to either of these two groups

nonflockingspecies<-(c("EleonoraF","Harrier-SPEC", "Hen", "Hobby", "Kestrel-SPEC", "Lanner", "LargeFALCON", "Mar", "Mon", "MonPalHen", "Osprey", "Pal", "Peregrine", "Roller", "Saker", "StockD", "TurtleD", "WoodP", "Merlin", "Goshawk", "CommonKestrel", "LesserKestrel", "Falcon-SPEC"))
smallspecies<-(c("EleonoraF","Harrier-SPEC", "Hen", "Lanner", "LargeFALCON", "Mar", "Mon", "MonPalHen", "Pal", "Peregrine", "Roller", "Saker", "StockD", "TurtleD", "WoodP", "RedFF", "Hobby", "Hobby/RedFF", "Kestrel-SPEC", "Merlin", "EurasianSH", "LevantSH", "SparrowH-SPEC", "Goshawk", "SparrowH/Goshawk", "CommonKestrel", "LesserKestrel", "Falcon-SPEC"))

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
	dauer<-as.numeric((Sys.time()-systemtime),units="secs")
	bisherigedauer<-bisherigedauer+dauer
	avgrestdauer<-round(bisherigedauer/i*(a1-i)/60,0)
	systemtime<-Sys.time()
   	setTkProgressBar(pb, i, label=paste( year(time1)[1]," | done: ",round(i/a1*100, 0), "% | time remaining: ", avgrestdauer, "min | detected DC: ",removed))

	# TIMEFRAME for transpassing duration gets set
	if (species1[i] %in% nonflockingspecies){
		kappa<-which(time2list > (time1[i]-600)& 
		time2list < (time1[i]+600))
	} else {
		kappa<-which(time2list > (time1[i]-900)& 
		time2list < (time1[i]+900))
	}
	# SPATIALZONE gets activated for smallspecies, DO NOT CHANGE, later only the large species (size==1) are considered for double count checking in farther distance zones.
	if (species1[i] %in% smallspecies){ size<-0} else{	size<-1}


	if(length(kappa)>0 & (SC1[i]=="FALSE")){
		time2<-time2list[kappa]
		time2<-time2+ceiling(rnorm(length(time2),30,15))
		diff<-abs(time1[i]-time2)
		species2<-species2list[kappa][order(diff)]
		sex2<-sex2list[kappa][order(diff)]
		age2<-age2list[kappa][order(diff)]
		morph2<-morph2list[kappa][order(diff)]
		location2<-location2list[kappa][order(diff)]
		index2<-index2list[kappa][order(diff)]	
		SC2<-SC2list[kappa][order(diff)]

	for (j in 1:length(time2)){
		if (
			((species1[i]!="HB" | 
			((species1[i]=="HB" & time1[i]< STARTtimeHBPhase1) |
			(species1[i]=="HB" & time1[i]> ENDtimeHBPhase1))) & 
			(species2[j]!="HB" | 
			((species2[j]=="HB" & time2[j]< STARTtimeHBPhase1) |
			(species2[j]=="HB" & time2[j]> ENDtimeHBPhase1)))) &
			(SC2[j] =="FALSE") &
			(species1[i]!="SHOT") & 
			(species1[i]!="NIS") & 
			(species2[j]!="SHOT") & 
			(species2[j]!="NIS") & 
			(species1[i]!="BK_NONJUV") & 
			(species1[i]!="BK_JUV") & 
			(species1[i]!="HB_AD") & 
			(species1[i]!="HB_JUV") &
			(species2[j]!="BK_NONJUV") & 
			(species2[j]!="BK_JUV") & 
			(species2[j]!="HB_AD") & 
			(species2[j]!="HB_JUV")
		){
			### CHECKS RECORD of station 1 with station 2. Any changes must be done mirrorwise in section station 2 checks station 1
			# checks species records (station1) with same species and lower ID-levels (station2)
			if	((
				(species1[i]==species2[j]) |
				(species1[i]=="BootedED" & species2[j]=="BootedE") |
				(species1[i]=="BootedEL" & species2[j]=="BootedE") |
				(species1[i]=="Mon" & species2[j]=="MonPalHen") |
				(species1[i]=="Pal" & species2[j]=="MonPalHen")|
				(species1[i]=="Hen" & species2[j]=="MonPalHen")|
				(species1[i]=="Mon" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="Mar" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="Hen" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="Pal" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="HB" & species2[j]=="Buzzard")|
				(species1[i]=="SB" & species2[j]=="Buzzard")|
				(species1[i]=="SB" & species2[j]=="MediumRaptor") |
				(species1[i]=="HB" & species2[j]=="MediumRaptor")|
				(species1[i]=="BlackKite" & species2[j]=="MediumRaptor")|
				(species1[i]=="Mar" & species2[j]=="MediumRaptor")|
				(species1[i]=="Mon" & species2[j]=="MediumRaptor")|
				(species1[i]=="Pal" & species2[j]=="MediumRaptor")|
				(species1[i]=="Hen" & species2[j]=="MediumRaptor")|
				(species1[i]=="BootedE" & species2[j]=="MediumRaptor")|
				(species1[i]=="BootedEL" & species2[j]=="MediumRaptor")|
				(species1[i]=="BootedED" & species2[j]=="MediumRaptor")|
				(species1[i]=="SteppeE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="LesserSE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="GreaterSE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="SteppeE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="ImperialE" & species2[j]=="LargeEAGLE")|
				(species1[i]=="WhiteTE" & species2[j]=="LargeEAGLE")|
				
				(species1[i]=="MonPalHen" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="MonPalHen" & species2[j]=="MediumRaptor")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="MediumRaptor")|
				(species1[i]=="Buzzard" & species2[j]=="MediumRaptor")|
				
				(species1[i]=="CommonKestrel" & species2[j]=="Kestrel-SPEC")|
				(species1[i]=="LesserKestrel" & species2[j]=="Kestrel-SPEC")|
				(species1[i]=="Hobby" & species2[j]=="Hobby/RedFF")|
				(species1[i]=="RedFF" & species2[j]=="Hobby/RedFF")|
				(species1[i]=="Peregrine" & species2[j]=="LargeFALCON")|
				(species1[i]=="Saker" & species2[j]=="LargeFALCON")|
				(species1[i]=="Lanner" & species2[j]=="LargeFALCON")|
				#for 2008 and 09
				(species1[i]=="CommonKestrel" & species2[j]=="Falcon-SPEC")|
				(species1[i]=="LesserKestrel" & species2[j]=="Falcon-SPEC")|
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
	
				(species1[i]=="BlackStork" & species2[j]=="Stork")|
				(species1[i]=="WhiteStork" & species2[j]=="Stork"))&
	 
				# IMPORTANT to keep this hirachie of DISTANCE ZONES during the comparison process, the main overlap zone needs to be checked at first and so on.
				( 
				# for all species until 1st ZONE 1 of the other station
				(location1[i]=="E2" & location2[j]=="W2")| # MAIN OVERLAP ZONE
				(location1[i]=="E3" & location2[j]=="W2") | # 
				(location1[i]=="W2" & location2[j]=="W3") | #
				(location1[i]=="E3" & location2[j]=="W1") | #
				(location1[i]=="W1" & location2[j]=="W3") | #
				# (location1[i]=="E2" & location2[j]=="W1") | # detects too many unreal double counts. In field the zones get distinguished well enough. Important to show in count protocol.
				# for larger species (size==1) check also DISTANCE ZONE "overhead" and the further ZONE 1 & 2
				(size==1 & location1[i]=="E3" & location2[j]=="E2") | # 
	 			(size==1 & location1[i]=="W2" & location2[j]=="W3") | # 
				(size==1 & location1[i]=="E3" & location2[j]=="E1")|  #
				(size==1 & location1[i]=="W1" & location2[j]=="W3") | # 
				(location1[i]=="E3" & location2[j]=="O") |
				(location1[i]=="O" & location2[j]=="W3") 
				)&
				
				# Check AGE & SEXES.
				# keeps the higher ID details. 
				# hierarchie in script IMPORTANT
				(
				(age1[i]==age2[j]) |
				(age1[i]=="ad" & age2[j]=="non-juv")|
				(age1[i]=="imm" & age2[j]=="non-juv")|
				(age1[i]=="ad" & age2[j]=="")|
				(age1[i]=="imm" & age2[j]=="")|
				(age1[i]=="non-juv" & age2[j]=="")|
				(age1[i]=="juv" & age2[j]=="")
				) & (
				(sex1[i]==sex2[j]) |
				(sex1[i]=="f" & sex2[j]=="fc")|
	 			(sex1[i]=="f" & sex2[j]=="")|
	 			(sex1[i]=="fc" & sex2[j]=="")|
	 			(sex1[i]=="m" & sex2[j]=="")
				) & (
				(morph1[i]==morph2[j]) |
				(morph1[i]=="light" & morph2[j]=="")|
	 			(morph1[i]=="dark" & morph2[j]=="")				
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
				(species1[i]=="BootedE" & species2[j]=="BootedED") |
				(species1[i]=="BootedE" & species2[j]=="BootedEL") |
				(species1[i]=="MonPalHen" & species2[j]=="Mon") |
				(species1[i]=="MonPalHen" & species2[j]=="Pal") |
				(species1[i]=="MonPalHen" & species2[j]=="Hen")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="Mon")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="Pal")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="Hen")|
				(species1[i]=="Harrier-SPEC" & species2[j]=="Mar")|
				(species1[i]=="Buzzard" & species2[j]=="HB")|
				(species1[i]=="Buzzard" & species2[j]=="SB")|
				(species1[i]=="MediumRaptor" & species2[j]=="SB") |
				(species1[i]=="MediumRaptor" & species2[j]=="HB") |
				(species1[i]=="MediumRaptor" & species2[j]=="BlackKite")|
				(species1[i]=="MediumRaptor" & species2[j]=="Mar")|
				(species1[i]=="MediumRaptor" & species2[j]=="Mon")|
				(species1[i]=="MediumRaptor" & species2[j]=="Pal")|
				(species1[i]=="MediumRaptor" & species2[j]=="Hen")|
				(species1[i]=="MediumRaptor" & species2[j]=="BootedE")|
				(species1[i]=="MediumRaptor" & species2[j]=="BootedEL")|
				(species1[i]=="MediumRaptor" & species2[j]=="BootedED")|
				(species1[i]=="LargeEAGLE" & species2[j]=="LesserSE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="GreaterSE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="SteppeE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="ImperialE")|
				(species1[i]=="LargeEAGLE" & species2[j]=="WhiteTE")|
				
				(species1[i]=="Harrier-SPEC" & species2[j]=="MonPalHen")|								
				(species1[i]=="MediumRaptor" & species2[j]=="MonPalHen")|
				(species1[i]=="MediumRaptor" & species2[j]=="Harrier-SPEC")|
				(species1[i]=="MediumRaptor" & species2[j]=="Buzzard")|
				
				(species1[i]=="Kestrel-SPEC" & species2[j]=="CommonKestrel")|
				(species1[i]=="Kestrel-SPEC" & species2[j]=="LesserKestrel")|
				(species1[i]=="Hobby/RedFF" & species2[j]=="Hobby")|
				(species1[i]=="Hobby/RedFF" & species2[j]=="RedFF")|
				(species1[i]=="LargeFALCON" & species2[j]=="Peregrine")|
				(species1[i]=="LargeFALCON" & species2[j]=="Saker")|
				(species1[i]=="LargeFALCON" & species2[j]=="Lanner")|
				## 2008/09
				(species1[i]=="Falcon-SPEC" & species2[j]=="CommonKestrel")|
				(species1[i]=="Falcon-SPEC" & species2[j]=="LesserKestrel")|
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
				
				(species1[i]=="Stork" & species2[j]=="BlackStork")|
				(species1[i]=="Stork" & species2[j]=="WhiteStork"))& 
	
				((size==1 & location2[j]=="W3" & location1[i]=="O") |  
				(size==1 & location2[j]=="W3" & location1[i]=="W1") | 
				(size==1 & location2[j]=="W3" & location1[i]=="W2")| 
				(size==1 & location2[j]=="O" & location1[i]=="E3") | 
				(size==1 & location2[j]=="W1" & location1[i]=="E3") |
	 			(size==1 & location2[j]=="W2" & location1[i]=="E3") |

				(location2[j]=="W3" & location1[i]=="E2") |#
				(location2[j]=="W3" & location1[i]=="E1") |#
				(location2[j]=="E1" & location1[i]=="E3") |#
				(location2[j]=="E2" & location1[i]=="E3") |#
				(location2[j]=="W2" & location1[i]=="E2") # 
				)&
				(
				(age1[i]==age2[j]) |
				(age1[i]=="non-juv" & age2[j]=="ad")|
				(age1[i]=="non-juv" & age2[j]=="imm")|
				(age1[i]=="" & age2[j]=="ad") |
				(age1[i]=="" & age2[j]=="imm") |
				(age1[i]=="" & age2[j]=="non-juv") |
				(age1[i]=="" & age2[j]=="juv")
				) & (
				(sex1[i]==sex2[j]) |
				(sex1[i]=="fc" & sex2[j]=="f")|
				(sex1[i]=="" & sex2[j]=="m") |
				(sex1[i]=="" & sex2[j]=="f") |
				(sex1[i]=="" & sex2[j]=="fc")		
	 			) & (
				(morph1[i]==morph2[j]) |
				(morph1[i]=="" & morph2[j]=="light")|
				(morph1[i]=="" & morph2[j]=="dark") 			
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
a<-data.frame(time1, Site1, species1, number1, age1, sex1, morph1, location1, health1, counttype1, DCDEL1, remark1, stringsAsFactors = FALSE) #speciesid1,

Site2<-rep("2", length(time2list))
b<-data.frame( time2list, Site2, species2list, number2, age2list, sex2list, morph2list, location2list, health2list, counttype2list, DCDEL2, remark2list, stringsAsFactors = FALSE) #speciesid2list

names(b)[1]<-"time1"
names(b)[2]<-"Site1"
names(b)[3]<-"species1"
names(b)[4]<-"number1"
names(b)[5]<-"age1"
names(b)[6]<-"sex1"
names(b)[7]<-"morph1"
names(b)[8]<-"location1"
names(b)[9]<-"health1"
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
colnames(newcount)[7] <-"morph"
colnames(newcount)[8] <-"location"
colnames(newcount)[9] <-"health"
colnames(newcount)[10] <-"counttype"
colnames(newcount)[11] <-"DCDEL"
colnames(newcount)[12] <-"remark"

recordsB<-newcount

timeB<-paste(recordsB$date, recordsB$time, sep = " ")
recordsB$timeB<-as.POSIXlt(timeB, format="%Y-%m-%d %H:%M:%S")
recordsB<-recordsB[order(recordsB$timeB),]
recordsB$year<-year(recordsB$timeB)
recordsB$yday<-yday(recordsB$timeB)

tablespecs<-data.frame(species = speclist, DC = rep(0, length(speclist)), NONDC = rep(0, length(speclist)), DEL = rep(0, length(speclist)))
tablespecs$species<-as.character(tablespecs$species)

for(u in 1: length(unique(records$year))){
subseta<-subset(records, records$year == unique(records$year)[u])
counta<-tapply(subseta$number, subseta$species, sum)
for (z in 1: length(counta)){
tablespecs[which(tablespecs[,1] == names(counta)[z]),u+2]<-counta[z]}}

for(u in 1: length(unique(recordsB$year))){
subsetb<-subset(recordsB, recordsB$year == unique(recordsB$year)[u])
countb<-tapply(subsetb$number, subsetb$species, sum)
for (z in 1: length(countb)){
tablespecs[which(tablespecs[,1] == names(countb)[z]),u+1]<-countb[z]}}
tablespecs$DEL<-tablespecs$DC - tablespecs$NONDC
#tablespecs$NONDC<-NULL
tablespecs$DELREL<-round(100/tablespecs$DC*tablespecs$DEL, digits=2)

#tablespecs<-tablespecs[with(tablespecs, order(speciesid)), ]

recordsB$timeB <- NULL
setwd(outputdir)
write.csv(recordsB, "1-newcount.csv")
write.csv(tablespecs, "1-tablespecsDC.csv")
close(pb)