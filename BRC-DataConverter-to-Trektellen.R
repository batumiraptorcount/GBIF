###############################################################
################### BATUMI RAPTOR COUNT #######################
######### Script sets for compiling count data ################
############### Convert BRC data < 2015 to trektellen upload format ####
# authors: J. Wehrmann,########################################
###############################################################
# switch converter BRC-converter-Trektellen-to-BRC-standard
# adjust species to trektellen standard


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
#data2015<-read.csv("2015-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",") # have been recorded with Trektellen APP
#data2016<-read.csv("2016-BRC-CLEANED.csv", header=TRUE, encoding="UTF-8", sep=",") # have been recorded with Trektellen APP



data2009$numberdoublecount<-NULL
data2009$Low<-NULL
data2009$Age_det<-NULL
data2010$Low<-NULL
data2012$BE_flock<-NULL

data<-data.frame(rbind(data2008, data2009, data2010, data2011, data2012, data2013, data2014))#, data2015, data2016

#delete NA
data[is.na(data)]<-""

### attach date formats ###
data$yday<-yday(data$date)
data$year<-year(data$date)


#### find & replace general entries ####
data$remark<-gsub(";", " ", data$remark, fixed = TRUE)
data$remark<-gsub(",", " ", data$remark, fixed = TRUE)
data$remark<-gsub("'", " ", data$remark, fixed = TRUE)
data$location[data$location == ">E3"]<-"E4"
data$location[data$location == "w1"]<-"W1"

#### adjust speciesnames to same abbreviations
### playground
#data[data$sex == "non-juv", ]
#####

data$count<-as.numeric(as.character(data$count))
data$speciesid<-as.numeric(as.character(data$speciesid))
data$speciesname<-as.character(data$speciesname)

# Kites ########################################
data$speciesname[
  data$speciesname == "BlackKite_JUV"
| data$speciesname == "BK_juv" 
| data$speciesname == "BKjuv" 
| data$speciesname == "Bkjuv" 
| data$speciesname == "BK_Juv"]<-"BK_JUV"
data$speciesid[data$speciesname == "BK_JUV"]<-as.numeric("-92")
data$speciesname[
  data$speciesname == "BlackKite_NONJUV" 
| data$speciesname == "BKad" 
| data$speciesname == "BKimm" 
| data$speciesname == "BK_Ad" 
| data$speciesname == "BKad"  
| data$speciesname == "BK_Non-juv"]<-"BK_NONJUV"
data$speciesid[data$speciesname == "BK_NONJUV"]<-as.numeric("-93")
data$speciesname[
  data$speciesname == "BK"
| data$speciesname == "BlaKite"]<-"BlackKite"
data$speciesid[data$speciesname == "BlackKite"]<-as.numeric("88")

data$speciesname[data$speciesname == "RK"]<-"RedKite"
data$speciesid[data$speciesname == "RedKite"]<-as.numeric("7000") # code missing ####################


# Eagles ########################################
data$plumage[data$speciesname == "BED"]<-"dark"
data$plumage[data$speciesname == "BEL"]<-"light"
data$speciesname[
  data$speciesname == "BED"
| data$speciesname == "BEL"
| data$speciesname == "BE"]<-"BootedE"
data$speciesid[data$speciesname == "BootedE"]<-as.numeric("592")

data$speciesname[data$speciesname == "GE"]<-"GoldenE"
data$speciesid[data$speciesname == "GoldenE"]<-as.numeric("107")

data$speciesname[data$speciesname == "GSE"]<-"GreaterSE"
data$speciesid[data$speciesname == "GreaterSE"]<-as.numeric("106")

data$speciesname[data$speciesname == "Imp" 
| data$speciesname == "Imp E" 
| data$speciesname == "ImpE"]<-"ImperialE"
data$speciesid[data$speciesname == "ImperialE"]<-as.numeric("589")

data$speciesname[
  data$speciesname == "LesGrSte" 
| data$speciesname == "LEspec" 
| data$speciesname == "LAquila-SPEC" 
| data$speciesname == "LUID"
| data$speciesname == "LesGreStep"]<-"LargeEAGLE"
data$speciesid[data$speciesname == "LargeEAGLE"]<-as.numeric("1151")

data$speciesname[data$speciesname == "LSE"]<-"LesserSE"
data$speciesid[data$speciesname == "LesserSE"]<-as.numeric("105")

data$speciesname[data$speciesname == "STE"]<-"ShortTE"
data$speciesid[data$speciesname == "ShortTE"]<-as.numeric("94")

data$speciesid[data$speciesname == "SteppeE"]<-as.numeric("588")

data$speciesname[data$speciesname == "Osp"]<-"Osprey"
data$speciesid[data$speciesname == "Osprey"]<-as.numeric("109")

data$speciesname[
  data$speciesname == "WTE" 
| data$speciesname == "White-TE" 
| data$speciesname == "Wh-Tail E"]<-"WhiteTE"
data$speciesid[data$speciesname == "WhiteTE"]<-as.numeric("91")



# Storks, Pelicans, Rollers, Doves and Pigeons ########################################
data$speciesname[
  data$speciesname == "BS"
| data$speciesname == "BlackStork"]<-"BlaStork"
data$speciesid[data$speciesname == "BlaStork"]<-as.numeric("31")

data$speciesname[
  data$speciesname == "WBST" 
| data$speciesname == "Stork-SPEC"  
| data$speciesname == "Storks-SPEC"]<-"Stork-SPEC"
data$speciesid[data$speciesname == "Stork-SPEC"]<-as.numeric("528")

data$speciesname[
  data$speciesname == "WS"
| data$speciesname == "WhitStork" 
| data$speciesname == "WhiStork" 
| data$speciesname == "WhiteStork" ]<-"WhiStork"
data$speciesid[data$speciesname == "WhiStork"]<-as.numeric("32")

data$speciesname[data$speciesname == "WP"]<-"WhiteP"
data$speciesid[data$speciesname == "WhiteP"]<-as.numeric("506")

data$speciesname[data$speciesname == "DalmatianPelican"]<-"DalmatianP"
data$speciesid[data$speciesname == "DalmatianP"]<-as.numeric("507")


data$speciesname[data$speciesname == "Rol"]<-"Roller"
data$speciesid[data$speciesname == "Roller"]<-as.numeric("255")

data$speciesid[data$speciesname == "WoodP"]<-as.numeric("233")

data$speciesname[data$speciesname == "TurtleD"]<-"TurtleDove"
data$speciesid[data$speciesname == "TurtleDove"]<-as.numeric("235")

data$speciesid[data$speciesname == "StockD"]<-as.numeric("232")

data$speciesname[
  data$speciesname == "SEowl"
| data$speciesname == "SEO"
| data$speciesname == "ShortEaredOwl"]<-"ShortEO"
data$speciesid[data$speciesname == "ShortEO"]<-as.numeric("246")




# Vultures ########################################
data$speciesname[data$speciesname == "BV"]<-"BlackV"
data$speciesid[data$speciesname == "BlackV"]<-as.numeric("93")

data$speciesname[data$speciesname == "Egy"]<-"EgyptianV"
data$speciesid[data$speciesname == "EgyptianV"]<-as.numeric("578")

data$speciesname[
  data$speciesname == "GV" 
| data$speciesname == "Griffon V"]<-"GriffonV"
data$speciesid[data$speciesname == "GriffonV"]<-as.numeric("92")



# Buzzards ########################################
data$speciesname[
  data$speciesname == "BUZ" 
| data$speciesname == "Buz" 
| data$speciesname == "Buzspec"
| data$speciesname == "B"
| data$speciesname == "Buzzard" 
| data$speciesname == "SBHB" 
| data$speciesname == "Pernis_SPEC"]<-"Buzzard-SPEC"
data$speciesid[data$speciesname == "Buzzard-SPEC"]<-as.numeric("586")

data$speciesname[
  data$speciesname == "CB"
| data$speciesname == "CommonB"]<-"CommonBuz"
data$speciesid[data$speciesname == "CommonBuz"]<-as.numeric("101")

data$speciesname[
  data$speciesname == "CHB"
| data$speciesname == "Crested HB"]<-"CrestedHB"
data$speciesid[data$speciesname == "CrestedHB"]<-as.numeric("1014")

data$speciesid[data$speciesname == "HB"]<-as.numeric("86")

data$speciesname[data$speciesname == "HB_ad" 
| data$speciesname == "HBad" 
| data$speciesname == "HB_Ad"
| data$speciesname == "HB_AD"]<-"HB_AD"
data$speciesid[data$speciesname == "HB_AD"]<-as.numeric("-90")

data$speciesname[data$speciesname == "HB_juv" 
| data$speciesname == "HBjuv"
| data$speciesname == "Hbjuv"  
| data$speciesname == "HB_Juv"]<-"HB_JUV"
data$speciesid[data$speciesname == "HB_JUV"]<-as.numeric("-91")

data$speciesname[data$speciesname == "LLB"]<-"LongLB"
data$speciesid[data$speciesname == "LongLB"]<-as.numeric("103")
# LongLB not monitored in 2010, 11, 12, 13, 14
data$remark[data$speciesname == "LongLB" & (data$year >= "2010" & data$year <= "2014")]<-paste("not monitored: irregular entry of", data$count[data$speciesname == "LongLB" & (data$year >= "2010" & data$year <= "2014")])
data$count[data$speciesname == "LongLB" & (data$year >= "2010" & data$year <= "2014")]<-0



data$speciesname[data$speciesname == "SB"]<-"StepBuz"
data$speciesid[data$speciesname == "StepBuz"]<-as.numeric("102")
# StepBuz not monitored in 2010, 11
data$remark[data$speciesname == "StepBuz" & (data$year == "2010" | data$year == "2011")]<-paste("not monitored: irregular entry of", data$count[data$speciesname == "StepBuz" & (data$year == "2010" | data$year == "2011")])
data$count[data$speciesname == "StepBuz" & (data$year == "2010" | data$year == "2011")]<-0

data$speciesid[data$speciesname == "SB_Juv"]<-as.numeric("5000")
data$speciesid[data$speciesname == "SB_Ad"]<-as.numeric("1156")

data$speciesname[data$speciesname == "RLB"]<-"RoughLB"
data$speciesid[data$speciesname == "RoughLB"]<-as.numeric("104")




# Cranes ########################################
data$speciesname[
  data$speciesname == "CC"
| data$speciesname == "ComCrane"
| data$speciesname == "CommonCrane"
| data$speciesname == "CommonC"
| data$speciesname == "Crane"]<-"EurasianCrane"
data$speciesid[data$speciesname == "EurasianCrane"]<-as.numeric("129")

data$speciesname[data$speciesname == "Demoiselle crane"]<-"DemCrane"
data$speciesid[data$speciesname == "DemCrane"]<-as.numeric("624")



# Falcons ########################################
data$speciesname[
  data$speciesname == "CK"
| data$speciesname == "CommonKestrel"]<-"CommonKes"
data$speciesid[data$speciesname == "CommonKes"]<-as.numeric("110")

data$speciesname[
  data$speciesname == "LK" 
| data$speciesname == "LesserKestrel" 
| data$speciesname == "LKes"]<-"LesserKes"
data$speciesid[data$speciesname == "LesserKes"]<-as.numeric("593")

data$speciesname[
  data$speciesname == "Kesspec"
| data$speciesname == "Kestrel-SPEC"
| data$speciesname == "Kes"
| data$speciesname == "CLK"]<-"Kestrel-SPEC" 
data$speciesid[data$speciesname == "Kestrel-SPEC"]<-as.numeric("1100")

data$speciesname[data$speciesname == "RFF"]<-"RedFF"
data$speciesid[data$speciesname == "RedFF"]<-as.numeric("111")

data$speciesname[data$speciesname == "Hob"]<-"Hobby"
data$speciesid[data$speciesname == "Hobby"]<-as.numeric("113")

data$speciesname[
  data$speciesname == "HobRFF" 
| data$speciesname == "Hob/RFF"]<-"Hobby/RedFF"
data$speciesid[data$speciesname == "Hobby/RedFF"]<-as.numeric("1099")

data$speciesid[data$speciesname == "EleonoraF"]<-as.numeric("597")

data$speciesname[data$speciesname == "Mer"]<-"Merlin"
data$speciesid[data$speciesname == "Merlin"]<-as.numeric("112")

data$speciesname[data$speciesname == "Per"]<-"Peregrine"
data$speciesid[data$speciesname == "Peregrine"]<-as.numeric("115")

data$speciesname[data$speciesname == "Saker"]<-"SakerF"
data$speciesid[data$speciesname == "SakerF"]<-as.numeric("599")

data$speciesname[data$speciesname == "Lanner"]<-"LannerF"
data$speciesid[data$speciesname == "LannerF"]<-as.numeric("598")

data$speciesname[
  data$speciesname == "PerSak" 
| data$speciesname == "LargeFALCON" 
| data$speciesname == "LargeFalcon" 
| data$speciesname == "LFspec" 
| data$speciesname == "PerSakLan"]<-"LargeFALCON"
data$speciesid[data$speciesname == "LargeFALCON"]<-as.numeric("1150")

data$speciesname[data$speciesname == "Falco-SPEC"]<-"Falcon-SPEC"
data$speciesid[data$speciesname == "Falcon-SPEC"]<-as.numeric("602")



# Harriers ########################################
data$speciesname[
  data$speciesname == "Harspec" 
| data$speciesname == "Circus-SPEC"]<-"Harrier-SPEC"
data$speciesid[data$speciesname == "Harrier-SPEC"]<-as.numeric("584")

data$speciesname[data$speciesname == "MAR"]<-"Mar"
data$speciesid[data$speciesname == "Mar"]<-as.numeric("95")

data$speciesname[
  data$speciesname == "Hen" 
| data$speciesname == "Hen H"]<-"Hen"
data$speciesid[data$speciesname == "Hen"]<-as.numeric("96")

data$speciesname[data$speciesname == "MON"]<-"Mon" 
data$speciesid[data$speciesname == "Mon"]<-as.numeric("98")

data$speciesname[data$speciesname == "MonPal"]<-"MonPalHen"
data$speciesid[data$speciesname == "MonPalHen"]<-as.numeric("1102")

data$speciesid[data$speciesname == "Pal"]<-as.numeric("97")
# adapt hard Protocol rules >=2014 only
#data$remark[data$speciesname == "Pal" & data$sex == "" & data$age == ""]<-paste("formerly Pal without age and sex")
#data$speciesname[data$speciesname == "Pal" & data$sex == "" & data$age == ""]<-"MonPalHen"


#Sparrowhawks
data$speciesname[
  data$speciesname == "LSPH"
| data$speciesname == "LSH"
| data$speciesname == "LevantS"]<-"LevantSH"
data$speciesid[data$speciesname == "LevantSH"]<-as.numeric("1019")
# LevantSH not monitored in 2010, 11, 12, 13
data$remark[data$speciesname == "LevantSH" & (data$year == "2010" | data$year == "2011" | data$year == "2012" | data$year == "2013")]<-paste("not monitored: irregular entry of", data$count[data$speciesname == "LevantSH" & (data$year == "2010" | data$year == "2011" | data$year == "2012" | data$year == "2013")])
data$count[data$speciesname == "LevantSH" & (data$year == "2010" | data$year == "2011" | data$year == "2012" | data$year == "2013")]<-0

data$speciesname[
  data$speciesname == "SH"
| data$speciesname == "ESPH"
| data$speciesname == "EuropeanSH"]<-"EurasianSH"
data$speciesid[data$speciesname == "EurasianSH"]<-as.numeric("100")
# EurasianSH not monitored in since 2010
data$remark[data$speciesname == "EurasianSH" & (data$year >= 2010)]<-paste("not monitored: irregular entry of", data$count[data$speciesname == "EurasianSH" & (data$year >= "2010")])
data$count[data$speciesname == "EurasianSH" & (data$year >= "2010")]<-0


data$speciesname[data$speciesname == "SPH" ]<-"SparrowH-SPEC"
data$speciesid[data$speciesname == "SparrowH-SPEC"]<-as.numeric("1101")
# SparrowH-SPEC not monitored in since 2010
data$remark[data$speciesname == "SparrowH-SPEC" & (data$year >= 2010)]<-paste("not monitored: irregular entry of", data$count[data$speciesname == "SparrowH-SPEC" & (data$year >= "2010")])
data$count[data$speciesname == "SparrowH-SPEC" & (data$year >= "2010")]<-0

data$speciesname[data$speciesname == "GH"]<-"Goshawk"
data$speciesid[data$speciesname == "Goshawk"]<-as.numeric("99")
# Goshawk not monitored in since 2010
data$remark[data$speciesname == "Goshawk" & (data$year >= 2010)]<-paste("not monitored: irregular entry of", data$count[data$speciesname == "Goshawk" & (data$year >= "2010")])
data$count[data$speciesname == "Goshawk" & (data$year >= "2010")]<-0

data$speciesname[data$speciesname == "SPGH"]<-"SPH/Goshawk"
data$speciesid[data$speciesname == "SPH/Goshawk"]<-as.numeric("585")
# SPH/Goshawk not monitored in since 2010
data$remark[data$speciesname == "SPH/Goshawk" & (data$year >= 2010)]<-paste("not monitored: irregular entry of", data$count[data$speciesname == "SPH/Goshawk" & (data$year >= "2010")])
data$count[data$speciesname == "SPH/Goshawk" & (data$year >= "2010")]<-0



# species groups #########################################


data$speciesname[data$speciesname == "MUID" ]<-"MediumRaptor"
data$speciesid[data$speciesname == "MediumRaptor"]<-as.numeric("1103")

data$speciesname[
  data$speciesname == "UID" 
| data$speciesname == "NIS"
| data$speciesname == "Raptor_SPEC"
| data$speciesname == "SUID"]<-"Raptor-SPEC"
data$speciesid[data$speciesname == "Raptor-SPEC"]<-as.numeric("997")



################################################################################

#### adjust age to same abbreviations
data$age<-tolower(data$age)
data$sex<-tolower(data$sex)
data$age<-gsub(" ", "", data$age, fixed = TRUE)
data$age<-gsub("-", "", data$age, fixed = TRUE)
data$sex<-gsub(" ", "", data$sex, fixed = TRUE)

data$sex[data$sex == "nonjuv"]<-"" #concerns 3 records of Mar in 2014
data$sex[data$age == "fc" & data$sex == ""]<-"fc" #concerns 2 records of in 2010
data$age[data$age == "fc" & data$sex == "fc"]<-"" #concerns 2 records of in 2010
data$age[data$age == "m"]<-"nonjuv" #concerns 3 record in 2013 were non-juv males
data$age[data$age == "adult"]<-"ad"
# 1st
# adjust speciesnames

##### adjust age data

#adjust Harrier's age >2cy to adult
data$age[(data$speciesname == "Mon" 
| data$speciesname == "Pal"
| data$speciesname == "Hen"
| data$speciesname == "MonPalHen"
| data$speciesname == "Mar"
| data$speciesname == "Harrier-SPEC")
& (data$age == "3cy" | data$age == "4cy")]<-"ad"

#adjust Harrier's age 2cy to imm
data$age[(data$speciesname == "Mon" 
| data$speciesname == "Pal"
| data$speciesname == "Hen"
| data$speciesname == "MonPalHen"
| data$speciesname == "Mar"
| data$speciesname == "Harrier-SPEC"
| data$speciesname == "RedFF")
& data$age == "2cy"]<-"imm"


#adjust Large Eagle's age 2cy-4cy to imm
data$age[(data$speciesname == "LesserSE" 
| data$speciesname == "GreaterSE"
| data$speciesname == "SteppeE"
| data$speciesname == "LargeEAGLE"
| data$speciesname == "ImpE"
| data$speciesname == "GoldenE")
& (data$age == "2cy" | data$age == "3cy" | data$age == "4cy")]<-"imm"

#adjust Large Eagle's age >4cy to ad
data$age[(data$speciesname == "LesserSE" 
| data$speciesname == "GreaterSE"
| data$speciesname == "SteppeE"
| data$speciesname == "LargeEAGLE"
| data$speciesname == "ImpE"
| data$speciesname == "GoldenE")
& (data$age == "5cy" | data$age == "6cy" | data$age == "7cy")]<-"ad"

data$age[(data$speciesname == "BootedE" & data$age =="2cy")]<-"nonjuv" # concerns 3 records in 2010
data$age[(data$speciesname == "RedFF" & data$age =="3cy")]<-"ad" # concerns 3 records in 2010
data$age[(data$speciesname == "BlackKite" & (data$age =="3cy" | data$age =="2cy"))]<-"nonjuv" # concerns 12 records in 2010
data$remark[(data$speciesname == "EgyptianV" & data$age =="4cy")]<-"4cy" # concerns 1 record in 2010
data$age[(data$speciesname == "EgyptianV" & data$age =="4cy")]<-"imm" # concerns 1 record in 2010
data$sex[(data$speciesname == "Mar" & data$sex =="non-juv")]<-"" # concerns 3 record in 2014, age is set to nonjuv

# handles misentries with sex = fc. 
# If age is determined, sex gets adjusted. 
# If age is missing, but sex is determined, age becomes non-juv
data$sex[(data$sex == "female")] <-"f"
data$sex[(data$sex == "male")] <-"m"
data$sex[(data$age == "ad") & (data$sex == "fc")] <-"f"
data$sex[(data$age == "nonjuv") & (data$sex == "fc")] <-"f"
data$sex[(data$age == "juv") & (data$sex == "fc")] <-""

data$age[((data$sex == "f" | data$sex == "m") & data$age == "")]<-"nonjuv"

data$age[(data$speciesname == "HB_AD" | data$speciesname == "HB_JUV" | data$speciesname == "BK_NONJUV" | data$speciesname == "BK_JUV")]<-""

######

data$plumage<-tolower(data$plumage)
data$sex[(data$plumage == "fc")] <-"fc"
data$plumage[(data$plumage == "fc")] <-""


#### adjust counttype to same abbreviations
data$counttype<-as.character(data$counttype)
data$counttype[data$counttype == "SC"]<-"S"
data$counttype[data$counttype == "DC"]<-"D"

#### adjust migtype to same abbreviations
data$migtype<-as.character(data$migtype)
data$migtype[data$migtype == "injured" | data$migtype == "inj"]<-"i"
data$migtype[data$migtype == "killed" | data$migtype == "kil"]<-"k"

#### adjust stations
data$telpost<-as.numeric(data$telpost)
data$telpost[data$telpost == 1]<-"1047"
data$telpost[data$telpost == 2]<-"1048"

### final test
unique(data$speciesname)
unique(data$telpost)
unique(data$sex)
unique(data$age)
unique(data$plumage)
unique(data$migtype)
unique(data$counttype)

#unique(data$speciesname[(data$sex == "fc")] )


names(data)
# was passiert mit den anderen Arten? am besten beim Processing rausfallen lassen
# write file for each year seperately, 2008-2014

sum(data$count[data$speciesname == "CrestedHB" & data$telpost == "1047" & data$year == "2015"])

#### write data####
#setwd(outputdir)
#write.csv(a, "1-converter_BRC-to-Trek_results.csv", fileEncoding="UTF-8", row.names=FALSE)
#a<-subset(data, data$year == "2008")
