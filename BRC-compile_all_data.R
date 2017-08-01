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
workdir<-"data/in"
outputdir<-"data/out"
setwd(workdir)

#### read data ####
# modify the path
data2008<-read.csv("2008-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2009<-read.csv("2009-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2010<-read.csv("2010-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2011<-read.csv("2011-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2012<-read.csv("2012-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2013<-read.csv("2013-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2014<-read.csv("2014-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2015<-read.csv("2015-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")
data2016<-read.csv("2016-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",")



data2009$numberdoublecount<-NULL
data2009$Low<-NULL
data2009$Age_det<-NULL
data2010$Low<-NULL
data2012$BE_flock<-NULL

data<-data.frame(rbind(data2008, data2009, data2010, data2011, data2012, data2013, data2014, data2015, data2016))

#delete NA
data[is.na(data)]<-""

### attach date formats ###
data$yday<-yday(data$date)
data$year<-year(data$date)


### continue working with the data variable by using further scripts