################### BATUMI RAPTOR COUNT #######################
######### Script sets for compiling count data ################
############ Converter for Trektellen to BRC format ###########
###############################################################
# before you run that script #######
# replace telpost names with numbers
# change date format to YYYY-MM-DD
# change timestamp format to HH:MM:SS


library(lubridate)

#### directories ####
# modify the path
workdir<-"/Users/jasper 1/Documents/Rplayground/data analysis - Bitbucket/data/in"
outputdir<-"/Users/jasper 1/Documents/Rplayground/data analysis - Bitbucket/data/in"
setwd(workdir)

### read data ###
full_data<-read.csv("2017_Trektellen_RAW.csv", header=T, fileEncoding="UTF-8", sep="\t")

#! trektellen files are written in UTF-7
full_data<-data.frame(full_data)
######################## Converter START ########################

full_data$speciesname<-as.character(full_data$speciesname)
full_data$age<-as.character(tolower(full_data$age))
full_data$plumage<-as.character(tolower(full_data$plumage))
full_data$migtype<-as.character(tolower(full_data$migtype))
full_data$counttype<-as.character(tolower(full_data$counttype))
full_data$sex<-as.character(tolower(full_data$sex))
full_data$telpost<-as.character(full_data$telpost)

full_data$telpost[full_data$telpost == "1. Sakhalvasho"]<-1
full_data$telpost[full_data$telpost == "2. Shuamta"]<-2
full_data$telpost<-as.numeric(full_data$telpost)

#### set abbreviations straight ####
full_data$age[full_data$age == "a"]<-"ad"
full_data$age[full_data$age == "i"]<-"imm"
full_data$age[full_data$age == "j"]<-"juv"
full_data$age[full_data$age == "non-juv"]<-"nonjuv"
full_data$plumage[full_data$plumage == "d"]<-"dark"
full_data$plumage[full_data$plumage == "l"]<-"light"
full_data$migtype[full_data$migtype == "i"]<-"inj"
full_data$migtype[full_data$migtype == "k"]<-"kil"
full_data$counttype[full_data$counttype == "s"]<-"sc"
full_data$counttype[full_data$counttype == "d"]<-"dc"
full_data$speciesname[full_data$speciesname == "Accspec"]<-"SparrowH/Goshawk"
full_data$speciesname[full_data$speciesname == "BK"]<-"BlackKite" 
full_data$speciesname[full_data$speciesname == "BlaKite"]<-"BlackKite"
full_data$speciesname[full_data$speciesname == "BK_JUV"]<-"BlackKite_JUV"
full_data$speciesname[full_data$speciesname == "BK_NONJUV"]<-"BlackKite_NONJUV"
full_data$speciesname[full_data$speciesname == "Bk_juv"]<-"BlackKite_JUV"
full_data$speciesname[full_data$speciesname == "Bk_imm"]<-"BlackKite_NONJUV"
full_data$speciesname[full_data$speciesname == "Bk_ad"]<-"BlackKite_NONJUV"
full_data$speciesname[full_data$speciesname == "BKjuv"]<-"BlackKite_JUV"
full_data$speciesname[full_data$speciesname == "BKimm"]<-"BlackKite_NONJUV"
full_data$speciesname[full_data$speciesname == "BKad"]<-"BlackKite_NONJUV"
full_data$speciesname[full_data$speciesname == "BK_Ad"]<-"BlackKite_NONJUV"
full_data$speciesname[full_data$speciesname == "BK_Non-juv"]<-"BlackKite_NONJUV"
full_data$speciesname[full_data$speciesname == "BK_Juv"]<-"BlackKite_JUV"
full_data$speciesname[full_data$speciesname == "BE"]<-"BootedE"
full_data$speciesname[full_data$speciesname == "BEL"]<-"BootedEL" 
full_data$speciesname[full_data$speciesname == "BED"]<-"BootedED" 
full_data$speciesname[full_data$speciesname == "Buzspec"]<-"Buzzard"
full_data$speciesname[full_data$speciesname == "Buzzard sp"]<-"Buzzard"
full_data$speciesname[full_data$speciesname == "Buz/HonBuz"]<-"Buzzard"
full_data$speciesname[full_data$speciesname == "Buz"]<-"Buzzard"
full_data$speciesname[full_data$speciesname == "BUZ"]<-"Buzzard"
full_data$speciesname[full_data$speciesname == "B"]<-"Buzzard"
full_data$speciesname[full_data$speciesname == "BlaStork"]<-"BlackStork"
full_data$speciesname[full_data$speciesname == "BS"]<-"BlackStork"
full_data$speciesname[full_data$speciesname == "BS/WS"]<-"Stork"
full_data$speciesname[full_data$speciesname == "BlackVulture"]<-"BlackV" 
full_data$speciesname[full_data$speciesname == "BV"]<-"BlackV"
full_data$speciesname[full_data$speciesname == "CommonB"]<-"CommonBuz"
full_data$speciesname[full_data$speciesname == "CB"]<-"CommonBuz" 
full_data$speciesname[full_data$speciesname == "Crane"]<-"EurasianCrane" 
full_data$speciesname[full_data$speciesname == "CC"]<-"EurasianCrane"
full_data$speciesname[full_data$speciesname == "CommonC"]<-"EurasianCrane"
full_data$speciesname[full_data$speciesname == "ComCrane"]<-"EurasianCrane"
full_data$speciesname[full_data$speciesname == "EuCrane"]<-"EurasianCrane"
full_data$speciesname[full_data$speciesname == "CommonK"]<-"CommonKestrel"
full_data$speciesname[full_data$speciesname == "CommonKes"]<-"CommonKestrel"
full_data$speciesname[full_data$speciesname == "CK"]<-"CommonKestrel"
full_data$speciesname[full_data$speciesname == "CLK"]<-"Kestrel-SPEC"
full_data$speciesname[full_data$speciesname == "Crested HB"]<-"CrestedHB"
full_data$speciesname[full_data$speciesname == "CHB"]<-"CrestedHB"
full_data$speciesname[full_data$speciesname == "Demoisellecrane"]<-"DemoiselleCrane"
full_data$speciesname[full_data$speciesname == "Demoiselle crane"]<-"DemoiselleCrane"
full_data$speciesname[full_data$speciesname == "DalmatianP"]<-"DalmatianPelican"
full_data$speciesname[full_data$speciesname == "EgytianV"]<-"EgyptianV"
full_data$speciesname[full_data$speciesname == "Egy"]<-"EgyptianV"
full_data$speciesname[full_data$speciesname == "SparrowH"]<-"EurasianSH"
full_data$speciesname[full_data$speciesname == "ESH"]<-"EurasianSH"
full_data$speciesname[full_data$speciesname == "ESPH"]<-"EurasianSH"
full_data$speciesname[full_data$speciesname == "Fal"]<-"Falcon-SPEC"
full_data$speciesname[full_data$speciesname == "GSE"]<-"GreaterSE"
full_data$speciesname[full_data$speciesname == "GE"]<-"GoldenE"
full_data$speciesname[full_data$speciesname == "Gh"]<-"Goshawk"
full_data$speciesname[full_data$speciesname == "GH"]<-"Goshawk"
full_data$speciesname[full_data$speciesname == "Griffon"]<-"GriffonV" 
full_data$speciesname[full_data$speciesname == "Griffon V"]<-"GriffonV" 
full_data$speciesname[full_data$speciesname == "GV"]<-"GriffonV" 
full_data$speciesname[full_data$speciesname == "Har"]<-"Harrier-SPEC"
full_data$speciesname[full_data$speciesname == "harrier sp"]<-"Harrier-SPEC"
full_data$speciesname[full_data$speciesname == "Harspec"]<-"Harrier-SPEC"
full_data$speciesname[full_data$speciesname == "Circus-SPEC"]<-"Harrier-SPEC"
full_data$speciesname[full_data$speciesname == "Hen H"]<-"Hen"
full_data$speciesname[full_data$speciesname == "HOb"]<-"Hobby"
full_data$speciesname[full_data$speciesname == "HOB"]<-"Hobby"
full_data$speciesname[full_data$speciesname == "Hob"]<-"Hobby"
full_data$speciesname[full_data$speciesname == "Hob/RFF"]<-"Hobby/RedFF"
full_data$speciesname[full_data$speciesname == "HobRFF"]<-"Hobby/RedFF"
full_data$speciesname[full_data$speciesname == "HB "]<-"HB"
full_data$speciesname[full_data$speciesname == "HBjuv"]<-"HB_JUV"
full_data$speciesname[full_data$speciesname == "HBad"]<-"HB_AD"
full_data$speciesname[full_data$speciesname == "HB_juv"]<-"HB_JUV"
full_data$speciesname[full_data$speciesname == "HB_ad"]<-"HB_AD"
full_data$speciesname[full_data$speciesname == "HB_Juv"]<-"HB_JUV"
full_data$speciesname[full_data$speciesname == "HB_Ad"]<-"HB_AD"
full_data$speciesname[full_data$speciesname == "Imp"]<-"ImperialE"
full_data$speciesname[full_data$speciesname == "ImpE"]<-"ImperialE"
full_data$speciesname[full_data$speciesname == "Imp E"]<-"ImperialE"
full_data$speciesname[full_data$speciesname == "LUID"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "LAquila-SPEC"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "Large EAGLE"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "LEspec"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "LesGreste"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "LesGrSte"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "LesGrestep"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "LesGresteppe"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "Large Eagle"]<-"LargeEAGLE"
full_data$speciesname[full_data$speciesname == "large FALCON"]<-"LargeFALCON"
full_data$speciesname[full_data$speciesname == "LargeFalcon"]<-"LargeFALCON"
full_data$speciesname[full_data$speciesname == "LFspec"]<-"LargeFALCON"
full_data$speciesname[full_data$speciesname == "Fspec"]<-"LargeFALCON"
full_data$speciesname[full_data$speciesname == "Lkes"]<-"LesserKestrel"
full_data$speciesname[full_data$speciesname == "LKes"]<-"LesserKestrel"
full_data$speciesname[full_data$speciesname == "Kestrel"]<-"CommonKestrel"
full_data$speciesname[full_data$speciesname == "Kes"]<-"CommonKestrel"
full_data$speciesname[full_data$speciesname == "Kest/LesKest"]<-"Kestrel-SPEC"
full_data$speciesname[full_data$speciesname == "Kesspec"]<-"Kestrel-SPEC"
full_data$speciesname[full_data$speciesname == "LK"]<-"LesserKestrel"
full_data$speciesname[full_data$speciesname == "LesserKes"]<-"LesserKestrel"
full_data$speciesname[full_data$speciesname == "LesKes"]<-"LesserKestrel"
full_data$speciesname[full_data$speciesname == "LSE"]<-"LesserSE"
full_data$speciesname[full_data$speciesname == "LSH"]<-"LevantSH"
full_data$speciesname[full_data$speciesname == "LSPH"]<-"LevantSH"
full_data$speciesname[full_data$speciesname == "LevantS"]<-"LevantSH"
full_data$speciesname[full_data$speciesname == "LLB"]<-"LongLB"
full_data$speciesname[full_data$speciesname == "Long-legged buzzard"]<-"LongLB"
full_data$speciesname[full_data$speciesname == "MAR"]<-"Mar"
full_data$speciesname[full_data$speciesname == "Marsh"]<-"Mar"
full_data$speciesname[full_data$speciesname == "Mer"]<-"Merlin"
full_data$speciesname[full_data$speciesname == "MUID"]<-"MediumRaptor"
full_data$speciesname[full_data$speciesname == "Monpal"]<-"MonPalHen"
full_data$speciesname[full_data$speciesname == "MonPal"]<-"MonPalHen"
full_data$speciesname[full_data$speciesname == "MonpalHen"]<-"MonPalHen"
full_data$speciesname[full_data$speciesname == "OSP"]<-"Osprey"
full_data$speciesname[full_data$speciesname == "Osp"]<-"Osprey" 
full_data$speciesname[full_data$speciesname == "Per"]<-"Peregrine" 
full_data$speciesname[full_data$speciesname == "Peregrine/Saker"]<-"LargeFALCON"
full_data$speciesname[full_data$speciesname == "PerSak"]<-"LargeFALCON"
full_data$speciesname[full_data$speciesname == "PerSakLan"]<-"LargeFALCON"
full_data$speciesname[full_data$speciesname == "PernisSPEC"]<-"Pernis-SPEC" 
full_data$speciesname[full_data$speciesname == "RFF"]<-"RedFF"
full_data$speciesname[full_data$speciesname == "RK"]<-"RedKite"
full_data$speciesname[full_data$speciesname == "Rol"]<-"Roller"
full_data$speciesname[full_data$speciesname == "RLB"]<-"RoughLB"
full_data$speciesname[full_data$speciesname == "SakerF"]<-"Saker" 
full_data$speciesname[full_data$speciesname == "Sak"]<-"Saker" 
full_data$speciesname[full_data$speciesname == "SB"]<-"StepBuz"
full_data$speciesname[full_data$speciesname == "SteppeB"]<-"StepBuz"
full_data$speciesname[full_data$speciesname == "SBHB"]<-"Buzzard"
full_data$speciesname[full_data$speciesname == "SH"]<-"SparrowH-SPEC"
full_data$speciesname[full_data$speciesname == "LevantS/Sparrow"]<-"SparrowH-SPEC"
full_data$speciesname[full_data$speciesname == "Shspec"]<-"SparrowH-SPEC"
full_data$speciesname[full_data$speciesname == "SPH"]<-"SparrowH-SPEC"
full_data$speciesname[full_data$speciesname == "SPH/GH"]<-"SparrowH/Goshawk"
full_data$speciesname[full_data$speciesname == "SPGH"]<-"SparrowH/Goshawk"
full_data$speciesname[full_data$speciesname == "STE"]<-"ShortTE"
full_data$speciesname[full_data$speciesname == "SHTE"]<-"ShortTE"
full_data$speciesname[full_data$speciesname == "Seowl"]<-"ShortEaredOwl" 
full_data$speciesname[full_data$speciesname == "SEowl"]<-"ShortEaredOwl" 
full_data$speciesname[full_data$speciesname == "Short-earedOwl"]<-"ShortEaredOwl"
full_data$speciesname[full_data$speciesname == "ShortEO"]<-"ShortEaredOwl" 
full_data$speciesname[full_data$speciesname == "SEO"]<-"ShortEaredOwl"
full_data$speciesname[full_data$speciesname == "STEP"]<-"SteppeE"
full_data$speciesname[full_data$speciesname == "SUID"]<-"MediumRaptor" # only very little numbers
full_data$speciesname[full_data$speciesname == "TurtleDove"]<-"TurtleD"
full_data$speciesname[full_data$speciesname == "UID"]<-"MediumRaptor" # only very little numbers
full_data$speciesname[full_data$speciesname == "WTE"]<-"WhiteTE" 
full_data$speciesname[full_data$speciesname == "Wh-TailE"]<-"WhiteTE" 
full_data$speciesname[full_data$speciesname == "White-tailedE"]<-"WhiteTE" 
full_data$speciesname[full_data$speciesname == "White-TE"]<-"WhiteTE" 
full_data$speciesname[full_data$speciesname == "Wh-Tail E"]<-"WhiteTE"
full_data$speciesname[full_data$speciesname == "WhiStork"]<-"WhiteStork"
full_data$speciesname[full_data$speciesname == "WS"]<-"WhiteStork"
full_data$speciesname[full_data$speciesname == "WhitStork"]<-"WhiteStork"
full_data$speciesname[full_data$speciesname == "Storks-SPEC"]<-"Stork-SPEC"
full_data$speciesname[full_data$speciesname == "WBST"]<-"Stork-SPEC"
full_data$speciesname[full_data$speciesname == "WP"]<-"WhitePelican"
full_data$speciesname[full_data$speciesname == "WhiteP"]<-"WhitePelican"
full_data$speciesname[full_data$speciesname == "Start"]<-"start"
full_data$speciesname[full_data$speciesname == "Start "]<-"start"
full_data$speciesname[full_data$speciesname == "START"]<-"start"
full_data$speciesname[full_data$speciesname == "SHIFT"]<-"shift"
full_data$speciesname[full_data$speciesname == "Shift"]<-"shift"
full_data$speciesname[full_data$speciesname == "END"]<-"end"
full_data$speciesname[full_data$speciesname == "End"]<-"end"
full_data$speciesname[full_data$speciesname == "shot"]<-"SHOT"
full_data$speciesname[full_data$speciesname == "Shot"]<-"SHOT"
full_data$speciesname[full_data$speciesname == "STOP"]<-"end"
full_data$speciesname[full_data$speciesname == "Stop"]<-"end"
full_data$speciesname[full_data$speciesname == "Rain"]<-"rain"
full_data$speciesname[full_data$speciesname == "Comment"]<-"comment"
full_data$speciesname[full_data$speciesname == "Nis"]<-"NIS"
full_data$speciesname[full_data$speciesname == "nis"]<-"NIS"
full_data$speciesname[full_data$speciesname == "Meteo"]<-"meteo"
full_data$age<-tolower(full_data$age)
full_data$age[full_data$age == "adult"]<-"ad"
full_data$age[full_data$age == "Non-Juv"]<-"nonjuv"
full_data$age[full_data$age == "I"]<-"imm"
full_data$age[full_data$age == "A"]<-"ad"
full_data$age[full_data$age == "J"]<-"juv"
full_data$age[full_data$age == "no value"]<-""
full_data$age[full_data$age == "m"]<-""
full_data$age[full_data$age == "fc"]<-""
full_data$age[full_data$age == "f"]<-""
full_data$age[full_data$age == "2cy"]<-"imm"
full_data$age[full_data$age == "2 cy"]<-"imm"
full_data$age[full_data$age == "3cy"]<-"imm"
full_data$age[full_data$age == "3 cy"]<-"imm"
full_data$age[full_data$age == "4cy"]<-"imm"
full_data$age[full_data$age == "4 cy"]<-"imm"
full_data$sex<-tolower(full_data$sex)
full_data$sex[full_data$sex == "no value"]<-""
full_data$sex[full_data$sex == "novalue"]<-""
full_data$sex[full_data$sex == "novalue"]<-""
full_data$sex[full_data$sex == "F"]<-"f"
full_data$sex[full_data$sex == "FC"]<-"fc"
full_data$sex[full_data$sex == "M"]<-"m"
full_data[full_data == "No Value"]<-""
full_data$count[full_data$count == "No Value"]<-0
full_data$count[full_data$count == "NoValue"]<-0
full_data$count[full_data$count == "no value"]<-0
full_data$count[full_data$count == ""]<-0
full_data[full_data =="NoValue"]<-""
full_data<- data.frame(lapply(full_data, function(x) {gsub(";", " ", x)}))
full_data<- data.frame(lapply(full_data, function(x) {gsub(",", " ", x)}))
full_data<- data.frame(lapply(full_data, function(x) {gsub("'", "", x)}))
full_data$countid<-NULL
######################## Converter END ########################

data<-data.frame(
date 		= full_data$date, 
timestamp 	= full_data$timestamp,
telpost 		= full_data$telpost,
speciesname = full_data$speciesname,
#speciesid 	= full_data$speciesid,
count 		= full_data$count,
countback 	= full_data$countback,
local 		= full_data$local,
location 	= full_data$location,
age 			= full_data$age,
sex 			= full_data$sex,
plumage 		= full_data$plumage,
counttype 	= full_data$counttype,
migtype 		= full_data$migtype,
remark 		= full_data$remark)
## write data
setwd(outputdir)
write.csv(data, "1-converter_output.csv", fileEncoding ="UTF-8")
