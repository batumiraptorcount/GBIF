###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script sets for compiling count data ################
###### Basic compilation of data for further processing #######
#################################

rm(list=ls())

#### libraries ####
library(lubridate)

#### directories ####
# modify the path
workdir<-"/Users/jasper 1/Documents/Rplayground/data analysis - Bitbucket/data/in"
outputdir<-"data/out"
setwd(workdir)

#### read data ####
# modify the path
data<-read.csv("full_data_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")
unique(data$species)
data$species[data$species == "TurtleDove"]<-"TurtleD"
data$number[data$species == "LesserKes" & data$year > 2009]<-0
data$number[data$species == "Hobby" & data$year > 2009]<-0
data$number[data$species == "Hobby/RedFF" & data$year > 2009]<-0
data$plumage[data$plumage == "dark"]<-"d"
data$plumage[data$plumage == "light"]<-"l"
data$plumage[data$plumage == "fox red"]<-""
data$plumage[data$plumage == "fulvescens"]<-"ful"

b<-subset(data, data$species != "Common Snipe" & data$species != "Grey Heron" & data$species != "Little Egret" & data$species != "Whimbrel" & data$species != "Ruff" & data$species != "Greenshank/Marsh Sandpiper" & data$species != "Black-winged Stilt" & data$species != "Ortolan" & data$species != "Night Heron" & data$species != "Heron-SPEC" & data$species != "Pratincole-SPEC" & data$species != "Northern Lapwing" & data$species != "Great Snipe" & data$species != "Black-winged Pratincole" & data$species != "Eurasian Spoonbill" & data$species != "Great White Egret" & data$species != "Common Nightjar" & data$species != "White's Thrush" & data$species != "Pigeon-SPEC" & data$species != "Lesser Grey Shrike" & data$species != "Curlew" & data$species != "Cattle Egret" & data$species != "Spoonbill" & data$species != "Glossy Ibis" & data$species != "Nightjar" & data$species != "Slender-billed Gull" & data$species != "Cormorant" & data$species != "Purple Heron" & data$species != "Calidris sp" & data$species != "Green Sandpiper" & data$species != "Ruddy Shelduck")
unique(b$plumage)
#write.csv(b, "full_data_PROCESSED_prefinal.csv", fileEncoding="UTF-8", row.names=TRUE)

data2009<-read.csv("2009_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")
data2010<-read.csv("2010_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")
data2011<-read.csv("2011_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")
data2012<-read.csv("2012_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")
data2013<-read.csv("2013_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")
data2014<-read.csv("2014_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")
data2015<-read.csv("2015_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")
data2016<-read.csv("2016_PROCESSED_prefinal.csv", header=TRUE, encoding="UTF-8", sep=",")



data2009$numberdoublecount<-NULL
data2009$Low<-NULL
data2009$Age_det<-NULL
data2010$Low<-NULL
data2012$BE_flock<-NULL

data<-data.frame(rbind(data2008, data2009, data2010, data2011, data2012, data2013, data2014, data2015, data2016))
#names(data)[1]<-"id"
data$species<-as.character(data$species)
#delete NA
data[is.na(data)]<-""

unique(data$species)

### attach date formats ###
data$yday<-yday(data$date)
data$year<-year(data$date)


### continue working with the data variable by using further scripts

#Black-winged Partincole
#Commen Snipe, Commen Snipe
#GWEgret , Great Egret, Great White Egret
#LittEgret, Little Egrets, Little Egret
#RuddyShel, Ruddy Shelduck
#Pernis_SPEC
#Pratincole sp, Pratincole-SPEC
#GreyH, Grey Herons, Grey Heron
#Heron sp, Heron SPEC

#BlaStork, BlackStork
#CommonKestrel, CommonKes
#Buzzard, Buzzard-SPEC
#CommonCrane, EurasianCrane
#DalmatianPelican, DalmatianP
#EuropeanSH, EurasianSH
#LesserKestrel, LesserKes
#Raptor_SPEC, Raptor-SPEC
#Saker, SakerF
#TurtleDove, TurtleD
#WhiteStork, WhiStork


