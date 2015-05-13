rm(list=ls(all=TRUE))
gc()
library(fields)

#source("/home/mfader/_R/LPJmL_Calibration/src/init.r")
#setwd("/home/mfader/_R/LPJmL_Calibration/")

#setting working directory
wkdir<-paste(getwd(),"/",sep="")
#selectCountry.r path setup
cowfile.global<-paste(wkdir,"lpjinputs/cow_mg_2006.bin",sep="")
cowfile.local<-paste(wkdir,"lpjinputs/med/cow_mg_2006.bin",sep="")
countryfao.path<-paste(wkdir,"FAO/",sep="")
#crop_calibr.r path setup
lpjinput.path <- paste(wkdir,"lpjinputs/med/",sep="")
output.path <-paste(wkdir,"lpjoutputs/med/",sep="")
fao.path<-paste(wkdir,"FAO/",sep="")
src.path<-paste(wkdir,"src/",sep="")
plot.path<-paste(wkdir,"plots/",sep="")
result.path<-paste(wkdir,"results/",sep="")


#-----------------
# Setting parameters
#-----------------
ncell <- 5794#67420 # no of cells recorded in file
ncell.in <- 5794#67420
res <- .5 # resolution in degrees
#baseyear <- 1901 #first year recorded in lpj output file
baseyear <- 1961 #first year recorded in lpj output file
startyear1 <- 2000 #first year to evaluate 
endyear1 <- 2009 #last year to evaluate 2009
fao.years <- c(2000:2009)#chose the years for fao data(Notice, the total years of FAO data is from 1961-2012)
experiment <- "revision Nela new crops"
nbands <- 52
CALI_TYPE<-"ALL"#IRRIG, RAINF
NOIRRIG<-F

nwr <- 10 # MAgPIE world regions10
ncft <<- 19 # ncft/2 is 26 but skipping man. gras, others, cotton, vegetables, fodder grass, bioenergy grass, bioenergy tree
#n.cou
ntry.fao <- 351
n.country.lpjml <- 197
nreg <- 197
nband.date <- 22   # no of bands in sdate/hdate files
eval.years <- paste(startyear1,"-",endyear1,sep="")


pxlim<-c(-15,52)# for mediterrian grid
pylim<-c(-5,55)

cropnames <-c("Temp_Cereals","Rice","Maize","Trop_Cereals","Pulses","Temp_Roots",
	   "Potatoes","Trop_Roots","Sunflower","Soybeans","Groundnuts","Rapeseed","Su_Cane",
	   "Others","Man_Grasslands","Bioen_Grass","Bioen_Tree","Citrus","Orchards",
	   "Date_Palm","Olives","Nuts_Trees","Grapes","Vegetables","Cotton","Fodder_grass")

cropnames2 <-c("Temp. Cereals","Rice","Maize","Trop. Cereals","Pulses","Temp. Roots",
	   "Potatoes","Trop. Roots","Sunflower","Soybeans","Groundnuts","Rapeseed","Su. Cane",
	   "Others","Man. Grasslands","Bioen. Grass","Bioen. Tree","Citrus","Orchards",
	   "Date Palm","Olives","Nut Trees","Grapes","Vegetables","Cotton","Fodder grass")
	   
FAO_Item<-c("Wheat", "Rice, paddy", "Maize", "Millet","Peas, dry", "Sugar beet", "Potatoes", "Cassava",
	  "Sunflower seed","Soybeans", "Groundnuts, with shell","Rapeseed", "Sugar cane", NA,NA,NA,NA,
	  "Oranges","Apples","Dates","Olives","Almonds, with shell", "Grapes", NA, NA, NA)


color1 <- colorRampPalette(c("red4","red","orange","lightgoldenrod","yellow","green","green4"))
color2 <- rep(rainbow(50)[1:45],times=100)
col.yields <- color1(25)

# values in percent of drymatter, e.g. wheat has 88 % of drymatter and 22% of water
FRESHMATTER <-  100 / c(88, 87, 88, 88, 90, 24,20, 35, 
			93, 91, 94, 92, 27, 100, 25 ,100 ,100,13,16,
			70,30,90,10,10,91,25) 
FRESHMATTER<<-c(FRESHMATTER,FRESHMATTER)





#-----------------
# switch trees calibration or crops calibration here 
#-----------------
lpjoutput.path <- paste(wkdir,"lpjoutputs/med_crops/",sep="")
source(paste(wkdir,"src/crop_calibr.r",sep=""))
#lpjoutput.path <- paste(wkdir,"lpjoutputs/med/",sep="")
#source(paste(wkdir,"src/tree_calibr.r",sep=""))

#check baseyear



