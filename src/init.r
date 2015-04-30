
wkdir<-"/Users/sinan/workspace/OT-Med/LPJmL/tree_density/LPJmL_Calibration/"
#selectCountry.r path setup
cowfile.global<-paste(wkdir,"lpjinputs/cow_mg_2006.bin",sep="")
cowfile.local<-paste(wkdir,"lpjinputs/med/cow_mg_2006.bin",sep="")
countryfao.path<-paste(wkdir,"FAO/",sep="")



#crop_calibr.r path setup
lpjinput.path <- paste(wkdir,"lpjinputs/med/",sep="")
lpjoutput.path <- paste(wkdir,"lpjoutputs/med_crops/",sep="")
output.path <-paste(wkdir,"lpjoutputs/med/",sep="")
fao.path<-paste(wkdir,"FAO/",sep="")
src.path<-paste(wkdir,"src/",sep="")
plot.path<-paste(wkdir,"plots/",sep="")



source(paste(wkdir,"src/crop_calibr.r",sep=""))
#source(paste(wkdir,"src/tree_calibr.r",sep=""))

