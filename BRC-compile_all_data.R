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
workdir<-"/Users/jasper 1/Documents/Rplayground/data/in"
outputdir<-"/Users/jasper 1/Documents/Rplayground/data/out"
setwd(workdir)

#### read data ####
# modify the path
data2008<-read.csv("2008-BRC-PROCESSED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2009<-read.csv("2009-BRC-PROCESSED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2010<-read.csv("2010-BRC-PROCESSED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2011<-read.csv("2011-BRC-PROCESSED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2012<-read.csv("2012-BRC-PROCESSED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2013<-read.csv("2013-BRC-PROCESSED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2014<-read.csv("2014-BRC-PROCESSED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2015<-read.csv("2015-BRC-PROCESSED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2016<-read.csv("2016-DC-uncleaned.csv", header=TRUE, encoding="UTF-8", sep=",")

data<-rbind(data2008, data2009, data2010, data2011, data2012, data2013, data2014, data2015, data2016)

#### find & replace entries ####
data<- data.frame(lapply(data, function(x) {gsub(";", " ", x)}))
data<- data.frame(lapply(data, function(x) {gsub(",", " ", x)}))
data<- data.frame(lapply(data, function(x) {gsub("'", "", x)}))
data<- data.frame(lapply(data, function(x) {gsub("w1", "W1", x)}))
data<- data.frame(lapply(data, function(x) {gsub(">E3", "E4", x)}))
data$number<-as.numeric(as.character(data$number))

### attach date formats ###
data<-data.frame(data, yday=yday(data$date), year=year(data$date))

### continue working with the data variable by using further scripts