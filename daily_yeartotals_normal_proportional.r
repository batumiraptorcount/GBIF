###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script for PROPORTIONAL ESTIMATION  #################
################## Time Cut for data ##########################
# authors: J. Wehrmann, #######################################
#### todo's: order species in results by taxonomy ID
#### get all results into one file if possible
#### get suntable data till 2020
###############################################################

rm(list=ls())

#### libraries ####
library(lubridate)

#### directories ####	
# modify the path
workdir<-"/Users/jasper 1/Documents/Rplayground/data/in"
outputdir<-"/Users/jasper 1/Documents/Rplayground/data/out"
setwd(workdir)

### read data ###
# modify the path or choose a variable with the data from previous scripts
records<-read.table("1-timecutter_results.csv", header=TRUE, encoding="UTF-8", sep=",")
#records<-finaldata

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
	MediumRaptor = c("HB", "BlackKite", "StepBuz", "BootedE", "Mar", "Mon", "Pal", "Hen"), 
	Buzzard = c("HB", "StepBuz", "LongLB", "CommonBuz"), 
	LargeEAGLE = c("LesserSE", "GreaterSE", "SteppeE"), #, "ImperialE", "GoldenE"), both can be neglected because of their low numbers
	Harrier.SPEC = c("Mar", "Mon", "Pal", "Hen"), 
	MonPalHen = c("Mon", "Pal", "Hen"), 
	#Falcon.SPEC= c("Hobby", "RedFF", "LesserKestrel", "CommonKestrel", "Peregrine", "Saker", "Lanner"), 
	#LargeFALCON = c("Peregrine", "Saker", "Lanner"), # makes no sense to include in estimation
	Hobby.RedFF = c("Hobby", "RedFF"), 
	Kestrel.SPEC = c("LesserKestrel", "CommonKestrel"), 
	SparrowH.Goshawk = c("EurasianSH", "LevantSH", "Goshawk"), 
	SparrowH.SPEC = c("EurasianSH", "LevantSH"))

ratio <-function (x,y,z){ x/100*(100/y*z) }
# x is dsum_group
# y is gsum
# z is dsum_spec

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
	# creates dailysums and yearlysums table based on normal data (dsum)
	sums1<-data.frame(tapply(as.numeric(as.character(new_records$dsum)), list(as.Date(new_records$date), new_records$species), FUN=sum, na.rm=TRUE))
	#sums_year1<-data.frame(tapply(as.numeric(as.character(new_records$dsum)), list(year(new_records$date), new_records$species), FUN=sum, na.rm=TRUE))
	
	# creates dailysums and yearlysums table based on proportional estimation data (dsum2)

	sums2<-data.frame(tapply(as.numeric(as.character(new_records$dsum2)), list(as.Date(new_records$date), new_records$species), FUN=sum, na.rm=TRUE))
	#sums_year2<-data.frame(tapply(as.numeric(as.character(new_records$dsum2)), list(year(new_records$date), new_records$species), FUN=sum, na.rm=TRUE))

for (i in 1:2){
	data<-eval(parse(text=paste0("sums", i)))
	data$date<-row.names(data)
	data$yday<-yday(data$date)
	data$year<-year(data$date)
	
	# creates a uniform table
	# Fehler drin. erkennt die Variablen des data.frames nicht
	# Lösung: 'ARTART' %in% names(data)
	# Als Schleife mit Zugriff auf Artenliste wesentlich kürzer und schneller
	if (!'RedKite' %in% names(data)){data$RedKite<-0}
	if (!'EleonoraF' %in% names(data)){data$EleonoraF<-0}
	if (!'Lanner' %in% names(data)){data$Lanner<-0}
	if (!'Saker' %in% names(data)){data$Saker<-0}
	if (!'Merlin' %in% names(data)){data$Merlin<-0}
	if (!'RoughLB' %in% names(data)){data$RoughLB<-0}
	if (!'EurasianSH' %in% names(data)){data$EurasianSH<-0}
	if (!'LevantSH' %in% names(data)){data$LevantSH<-0}
	if (!'BlackV' %in% names(data)){data$BlackV<-0}
	if (!'GriffonV' %in% names(data)){data$GriffvonV<-0}
	if (!'Buzzard' %in% names(data)){data$Buzzard<-0}
	if (!'GoldenE' %in% names(data)){data$GoldenE<-0}
	if (!'Goshawk' %in% names(data)){data$Goshawk<-0}
	if (!'SparrowH.SPEC' %in% names(data)){data$SparrowH.SPEC<-0}
	if (!'SparrowH.Goshawk' %in% names(data)){data$SparrowH.Goshawk<-0}
	if (!'WhitePelican' %in% names(data)){data$WhitePelican <-0}
	if (!'DalmatianPelican' %in% names(data)){data$DalmatianPelican <-0}
	if (!'DemoiselleCrane' %in% names(data)){data$DemoiselleCrane <-0}
	if (!'Falcon.SPEC' %in% names(data)){data$Falcon.SPEC <-0}
	if (!'Crane.SPEC' %in% names(data)){data$Crane.SPEC <-0}
	if (!'WoodP' %in% names(data)){data$WoodP <-0}
	if (!'StockD' %in% names(data)){data$StockD <-0}
	if (!'TurtleD' %in% names(data)){data$TurtleD <-0}
	if (!'LesserKestrel' %in% names(data)){data$LesserKestrel <-0}
	if (!'CommonKestrel' %in% names(data)){data$CommonKestrel <-0}
	if (!'Kestrel.SPEC' %in% names(data)){data$Kestrel.SPEC<-0}
	if (!'Hobby.RedFF' %in% names(data)){data$Hobby.RedFF <-0}
	if (!'Stork.SPEC' %in% names(data)){data$Stork.SPEC <-0}
	if (!'CrestedHB' %in% names(data)){data$CrestedHB<-0}
	if (!'HB_AD' %in% names(data)){data$HB_AD <-0}
	if (!'HB_JUV' %in% names(data)){data$HB_JUV<-0}
	if (!'BlackKite_JUV' %in% names(data)){data$BlackKite_JUV <-0}
	if (!'BlackKite_NONJUV' %in% names(data)){data$BlackKite_NONJUV <-0}
	if (!'CommonCrane' %in% names(data)){data$CommonCrane <-0}
	if (!'ShortEaredOwl' %in% names(data)){data$ShortEaredOwl <-0}
	if (!'CommonBuz' %in% names(data)){data$CommonBuz <-0}
	if (!'LongLB' %in% names(data)){data$LongLB <-0}
	if (!'RedFF' %in% names(data)){data$RedFF <-0}
	#if (!exists('data$xxxxx')){data$xxxx <-0}

	data_final<-data.frame(
			year = data$year,			
			date = data$date,			
			HB = data$HB,
			StepBuz = data$StepBuz,
			CommonBuz = data$CommonBuz,
			LongLB = data$LongLB,
			CrestedHB = data$CrestedHB,
			RoughLB = data$RoughLB,
			Buzzard = data$Buzzard,
			MediumRaptor = data$MediumRaptor,
			
			BlackKite = data$BlackKite,
			RedKite = data$RedKite,
			
			LesserSE = data$LesserSE,
			GreaterSE = data$GreaterSE,
			SteppeE = data$SteppeE,
			LargeEAGLE = data$LargeEAGLE,
			BootedE = data$BootedE,
			ShortTE = data$ShortTE,
			ImperialE = data$ImperialE,
			GoldenE = data$GoldenE,
			Osprey = data$Osprey,
			WhiteTE = data$WhiteTE,
			
			Mar = data$Mar,
			Mon = data$Mon,
			Pal = data$Pal,
			Hen = data$Hen,
			MonPalHen = data$MonPalHen,
			Harrier.SPEC = data$Harrier.SPEC,
			
			EurasianSH = data$EurasianSH,
			LevantSH = data$LevantSH,
			Goshawk = data$Goshawk,
			SparrowH.SPEC = data$SparrowH.SPEC,
			SparrowH.Goshawk = data$SparrowH.Goshawk,
			
			EgyptianV = data$EgyptianV,
			BlackV = data$BlackV,
			GriffonV = data$GriffonV,
			
			Peregrine = data$Peregrine,
			Saker = data$Saker,
			Lanner = data$Lanner,
			EleonoraF = data$EleonoraF,
			LesserKestrel = data$LesserKestrel,
			CommonKestrel = data$CommonKestrel,
			Merlin = data$Merlin,
			Hobby = data$Hobby,
			RedFF = data$RedFF,
			LargeFALCON = data$LargeFALCON,
			Hobby.RedFF = data$Hobby.RedFF,
			Kestrel.SPEC = data$Kestrel.SPEC,
			Falcon.SPEC = data$Falcon.SPEC,
			
			Roller = data$Roller,
			BlackStork = data$BlackStork,
			WhiteStork = data$WhiteStork,
			Stork.SPEC = data$Stork.SPEC,
			WhitePelican = data$WhitePelican,
			DalmatianPelican = data$DalmatianPelican,
			CommonCrane = data$CommonCrane,
			DemoiselleCrane = data$DemoiselleCrane,
			Crane.SPEC = data$Crane.SPEC,			
			TurtleD = data$TurtleD,
			WoodP = data$WoodP,
			StockD = data$StockD,
			ShortEO = data$ShortEaredOwl,
			HB_AD = data$HB_AD,
			HB_JUV = data$HB_JUV,
			BK_NONJUV = data$BlackKite_NONJUV,
			BK_JUV = data$BlackKite_JUV
			)
		assign(paste0("results_sums", i),data_final)
}

setwd(outputdir)

		#daytotals
write.csv(results_sums1, "1-dailytotals_results_normal.csv", fileEncoding="UTF-8", row.names=FALSE)
		#yeartotals
#		write.csv(yeartotals, "1-yeartotals_results_normal.csv", fileEncoding="UTF-8", row.names=TRUE)
		#daytotals
write.csv(results_sums2, "1-dailytotals_results_prop_addition.csv", fileEncoding="UTF-8", row.names=FALSE)
		#yeartotals
#		write.csv(sums_year, "1-yeartotals_results_prop_addition.csv", fileEncoding="UTF-8", row.names=TRUE)





