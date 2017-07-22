###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script sets for compiling count data ################
############### Convert BRC data < 2015 to standard format ####
# authors: J. Wehrmann,########################################
###############################################################

rm(list=ls())

#### libraries ####
library(lubridate)

#### directories ####
# modify the path
workdir<-"/Users/jasper 1/Documents/Rplayground/data/in"
outputdir<-"/Users/jasper 1/Documents/Rplayground/data/out"

### load data #####
setwd(workdir)
# modify the file name
data<-read.csv("2013-RAW-BRC.csv", header=TRUE, encoding="UTF-8", sep=",")

#### adjust the dataset #####
# find and replace data values 
data<- data.frame(lapply(data, function(x) {gsub("NoValue", "", x)}))
data<- data.frame(lapply(data, function(x) {gsub("No value", "", x)}))
data<- data.frame(lapply(data, function(x) {gsub("Mel", "melanistic", x)}))
data<- data.frame(lapply(data, function(x) {gsub("Leu", "leucistic", x)}))
data<- data.frame(lapply(data, function(x) {gsub("Dark", "dark", x)}))
data<- data.frame(lapply(data, function(x) {gsub("Light", "light", x)}))
data<- data.frame(lapply(data, function(x) {gsub("Fulvescens", "fulvescens", x)}))
data<- data.frame(lapply(data, function(x) {gsub("female", "f", x)}))
data<- data.frame(lapply(data, function(x) {gsub("FC", "fc", x)}))
data<- data.frame(lapply(data, function(x) {gsub("male", "m", x)}))
data<- data.frame(lapply(data, function(x) {gsub("non-juv", "nonjuv", x)}))
data<- data.frame(lapply(data, function(x) {gsub("adult", "ad", x)}))
data<- data.frame(lapply(data, function(x) {gsub("juvenile", "juv", x)}))
data<- data.frame(lapply(data, function(x) {gsub("immature", "imm", x)}))
data<- data.frame(lapply(data, function(x) {gsub("injured", "inj", x)}))
data<- data.frame(lapply(data, function(x) {gsub("killed", "kil", x)}))
data<- data.frame(lapply(data, function(x) {gsub(";", " ", x)}))
data<- data.frame(lapply(data, function(x) {gsub(",", " ", x)}))
data<- data.frame(lapply(data, function(x) {gsub("'", "", x)}))
data<- data.frame(lapply(data, function(x) {gsub("EuropeanSH", "EurasianSH", x)}))
data<- data.frame(lapply(data, function(x) {gsub("SakerF", "Saker", x)}))

#sets header in small letters
names(data)<-tolower(c(names(data))) 

# adjust more values
if (year(data$date)[1]<2015){
	data$morph<-as.character(data$morph)
	data$health<-""
	data$counttype<-""
	for (i in 1:length(data$date)){
		if (any(colnames(data) == "inj") | any(colnames(data) == "injured")){
			if (data$injured[i] == "YES" | data$injured[i] == 1) {data$health[i]<-"inj" }
			if (data$shotdown[i] == "YES"| data$shotdown[i] == 1) {data$health[i]<-"kil" }
		}
		if (any(colnames(data) == "rdc")){
			if (data$rdc[i] == 1) {data$counttype[i]<-"DC" }
			if (data$sc[i] == 1) {data$counttype[i]<-"SC" }
		}
		if (data$species[i] == "BootedEL") {data$morph[i]<-"light"; data$species[i]<-"BootedE"}
		if (data$species[i] == "BootedED") {data$morph[i]<-"dark"; data$species[i]<-"BootedE" }
		}
		if (any(colnames(data) == "shotdown")){	
			data$shotdown<-NULL
			data$inj<-NULL
		}
		if (any(colnames(data) == "rdc")){
			data$rdc<-NULL
			data$sc<-NULL
		}
}
# changes NA data to ""
data[is.na(data)] <-""

#creates the header in BRC-standard format, DO NOT CHANGE
processed_data<-data.frame(
	date=data$date, 
	time=data$time,
	station=data$station,
	species=data$species,
	age=data$age,
	sex=data$sex,
	morph=data$morph,
	location=data$location,
	number=data$number,
	health=data$health,
	remark=data$remark,
	counttype=data$counttype,
	dcdel=data$dcdel
	)
names(processed_data)

#### write data####
setwd(outputdir)
write.csv(processed_data, "1-converter_results.csv", fileEncoding="UTF-8", row.names=FALSE)