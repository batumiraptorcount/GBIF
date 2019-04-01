###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script for CORRECTION FOR UID  ######################
# author: J. Wehrmann #########################################
###############################################################
rm(list=ls())

Sys.setenv(TZ="Europe/Berlin")

#### libraries ####
library(lubridate)
library(data.table)
#### directories ####	
# modify the path
#workdir<-""
workdir<-"/Users/Jasper/Documents/Rplayground/data analysis - Bitbucket/data/"
setwd(workdir)

### read data ###
#records<-read.table("count_data_csv", header=TRUE, encoding="UTF-8", sep=",")
data<-read.csv("final/PROCESSED-allyears_withFilter.csv", header=TRUE, encoding="UTF-8", sep=",")

records<-as.data.table(data)
#records$year<-year(records$date)

# modify filter
# 1: standardised data since 2011
# 2: only early morning counts since 2014
# 0: pilot, irregular and experimental counts since 2008
# recalculation for harriers is based on data close the counting post only
records<-subset(data,data$filter == 1 & !(data$species %in% c("Harrier_SPEC","MonPalHen","Mar", "Mon", "Pal", "Hen")))

# add yday independent of leapyear
records$year<-year(records$date)
records$yday<-yday(records$date)
leapyear<-which(records$year == 2008 | records$year == 2012 | records$year == 2016 | records$year == 2020 | records$year == 2024)
records$yday[leapyear] <- records$yday[leapyear]-1
records$yday[-leapyear] <- records$yday[-leapyear]

### Daytotals ############################################################
daytotals<-tapply(as.numeric(as.character(records$number)), list(as.Date(records$date), records$species), FUN=sum, na.rm=TRUE)
daytotals[is.na(daytotals)]<-0
daytotals <-data.frame(cbind(date=rownames(daytotals),daytotals), row.names=1:length(rownames(daytotals)),stringsAsFactors = FALSE)
### START of CORRECTION FOR UID ############################################################
b<-NULL
# creates table identical normal sums in dsum and dsum2
for (y in 2:length(names(daytotals))){
	for (i in 1:length(daytotals$date)){
		a<-data.frame(date=daytotals$date[i], dsum=daytotals[i,y], species=names(daytotals)[y], dsum2=daytotals[i,y])
		if (exists('b')){b<-rbind(b,a)} else {b<-a}
	}
}
### this list determines the groups and the respective species for the CORRECTION FOR UID
# Raptor_SPEC is not used for correction as it can produces very unlikely data for rare species
# rare species or morphological groups are not included in groups to avoid boosting their total number in unrealistic way
spectab<-list(
	Buzzard_SPEC = c("HB", "StepBuz"),
	LargeEAGLE = c("LesserSE", "GreaterSE", "SteppeE"),
	MediumRaptor = c("HB", "BlackKite", "StepBuz"),
	Hobby_RedFF = c("Hobby", "RedFF"), 
	Kestrel_SPEC = c("LesserKestrel", "CommonKestrel"), 
	SparrowH.SPEC = c("EurasianSH", "LevantSH")
	)

ratio <-function (x,y,z){ x/100*(100/y*z) } # x is dsum_group; y is gsum; z is dsum_spec

# creates daily subset and writes the ratio function results into the table new_records with the estimated proportion in dsums2 
for (d in 1:length(unique(b$date))){
	c<-b[b$date == as.character(unique(b$date)[d]), ]
	c[sapply(c, is.factor)] <- lapply(c[sapply(c, is.factor)], as.character)
	# creates matrix which species have to be checked
	go<-match(names(spectab),as.character(c$species))

	for (i in 1:length(c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]])){
		gs<-data.frame(spectab[names(spectab) == c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]][i]])
		gssi<-match(gs[,1], c$species)
		gssi<-gssi[!gssi %in% gssi[is.na(gssi)]]
		c[gssi,]$dsum
		gsum<-sum(as.numeric(as.character(c[gssi,]$dsum)))
		dsum<-as.numeric(as.character(c[gssi,]$dsum))
		dsum_UIL<-as.numeric(as.character(c$dsum[c$species == c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]][i]]))
		if (dsum_UIL>0 & gsum>0){
		c[gssi,]$dsum2<-round(as.numeric(as.character(c[gssi,]$dsum2)) + ratio(dsum_UIL,gsum,dsum))
		# assigned amount of UIL substracted from UIL dsum2
		c$dsum2[c$species == c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]][i]]<- as.numeric(as.character(c$dsum[c$species == c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]][i]])) - round(sum(ratio(dsum_UIL,gsum,dsum)))
		}
	}
	if (exists('new_records')){new_records<-rbind(new_records,c)} else {new_records<-c}
}

non_harriers <- new_records

rm(new_records)

##############################################################################################
#############                   REPEAT FOR HARRIERS ONLY                               #######
##############################################################################################

### read data for non harriers###
# modify the path or choose a variable with the data from previous scripts
records<-subset(data,data$filter == 1 & (data$species %in% c("Harrier_SPEC","MonPalHen") | (data$species %in% c("Mar","Mon","Pal","Hen") & data$location %in% c("E1","O","W1"))))

### NORMAL DAILYSUMS and YEARSUMS ############################################################
dailysums<-tapply(as.numeric(as.character(records$number)), list(as.Date(records$date), records$species), FUN=sum, na.rm=TRUE)
dailysums[is.na(dailysums)]<-0
dailysums <-data.frame(cbind(date=rownames(dailysums),dailysums), row.names=1:length(rownames(dailysums)),stringsAsFactors = FALSE)
records$year<-year(records$date)
yearsums<-tapply(as.numeric(as.character(records$number)), list(records$year, records$species), FUN=sum, na.rm=TRUE)
yearsums[is.na(yearsums)]<-0
yearsums <-data.frame(cbind(year=rownames(yearsums), yearsums), row.names=1:length(rownames(yearsums)),stringsAsFactors = FALSE)
### START of PROPORTIONAL ESTIMATION ############################################################
b<-NULL
# creates table identical normal sums in dsum and dsum2
for (y in 2:length(names(dailysums))){
	for (i in 1:length(dailysums$date)){
		a<-data.frame(date=dailysums$date[i], dsum=dailysums[i,y], species=names(dailysums)[y], dsum2=dailysums[i,y])
		if (exists('b')){b<-rbind(b,a)} else {b<-a}
	}
}
### this list determines the groups and the respective species for the proportional estimation
spectab<-list(
	Harrier_SPEC = c("Mar", "Mon", "Pal", "Hen"), 
	MonPalHen = c("Mon", "Pal", "Hen"))

ratio <-function (x,y,z){ x/100*(100/y*z) }
# x is dsum_group
# y is gsum
# z is dsum_spec

# creates daily subset and writes the ratio function results into the table new_records 
# with the estimated proportion in dsums2 
for (d in 1:length(unique(b$date))){
	c<-b[b$date == as.character(unique(b$date)[d]), ]
	c[sapply(c, is.factor)] <- lapply(c[sapply(c, is.factor)], as.character)
	# creates matrix which species have to be checked
	go<-match(names(spectab),as.character(c$species))

	for (i in 1:length(c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]])){
		gs<-data.frame(spectab[names(spectab) == c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]][i]])
		gssi<-match(gs[,1], c$species)
		gssi<-gssi[!gssi %in% gssi[is.na(gssi)]]
		c[gssi,]$dsum
		gsum<-sum(as.numeric(as.character(c[gssi,]$dsum)))
		dsum<-as.numeric(as.character(c[gssi,]$dsum))
		dsum_UIL<-as.numeric(as.character(c$dsum[c$species == c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]][i]]))
		if (dsum_UIL>0 & gsum>0){
		c[gssi,]$dsum2<-round(as.numeric(as.character(c[gssi,]$dsum2)) + ratio(dsum_UIL,gsum,dsum))
		# assigned amount of UIL substracted from UIL dsum2
		c$dsum2[c$species == c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]][i]]<- as.numeric(as.character(c$dsum[c$species == c[go,]$species[!c[go,]$species %in% c[go,]$species[is.na(c[go,]$species)]][i]])) - round(sum(ratio(dsum_UIL,gsum,dsum)))
		}
	}
	if (exists('new_records')){new_records<-rbind(new_records,c)} else {new_records<-c}
}

harriers <- new_records

##### extract only additional harriers for each species and date, compute dailysums based on all data (including in E and W 2 and 3) and recompute dsum and dsum2
harriers$extra <- as.numeric(harriers$dsum2) - as.numeric(harriers$dsum)

records<-subset(data,data$filter == 1 & data$species %in% c("Harrier_SPEC","MonPalHen","Mar","Mon","Pal","Hen"))
dailysums<-aggregate(records$number, list(as.character(records$date), records$species), FUN=sum)
colnames(dailysums) <- c("date","species","dsum.or")

harriers <- merge(harriers,dailysums,all.x=T)
harriers[which(is.na(harriers$dsum.or)==T),]$dsum.or<-0
harriers$dsum2 <- harriers$dsum.or + harriers$extra

harriers <- harriers[,c("date","dsum.or","species","dsum2")]
colnames(harriers)[2] <- "dsum"
harriers$dsum <- as.character(harriers$dsum)
harriers$dsum2 <- as.character(harriers$dsum2)

findats <- rbind(non_harriers,harriers)

findats2<-tapply(as.numeric(as.character(findats$dsum2)), list(as.Date(findats$date), as.character(findats$species)), FUN=sum, na.rm=TRUE)
findats3<-data.frame(findats2)
findats3$date<-row.names(findats3)

#####################
#daytotals_corr<-data.frame(date=findats$date, species=findats$species, number=findats$dsum2)
daytotals_corr<-findats3
### Daytotals, Yeartotals, corrected for UID ########################################################

#	yeartotals_corr<-data.frame(tapply(as.numeric(as.character(daytotals_corr$number)), list(year(daytotals_corr$date), daytotals_corr$species), FUN=sum, na.rm=TRUE))
write.csv(daytotals_corr, "final/daytotals_corrected_for_uid_filter1.csv", fileEncoding="UTF-8", row.names=FALSE)
#write.csv(yeartotals_corr, "final/yeartotals_corrected_for_uid.csv", fileEncoding="UTF-8", row.names=TRUE)