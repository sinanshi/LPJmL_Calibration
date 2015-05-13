
RUN<-7

BAND_RAINF<-c(1:13)
BAND_IRRG<-c(27:39)
BAND_ALL<-c(BAND_RAINF,BAND_IRRG )

BAND_EXAM<-list()
BAND_EXAM$lpj<-c(1:13)
BAND_EXAM$fao<-c(1:13)

source(paste(src.path,"readFAO.r",sep=""))
source(paste(src.path,"selectCountry.r",sep=""))
source(paste(src.path,"lpjutil.r",sep=""))
source(paste(src.path,"read_lpj_calibration_output.r",sep=""))

read.input.grid(paste(lpjinput.path,"grid.bin",sep=""))
cow<<-read.input.yearband(paste(lpjinput.path,"cow_mg_2006.bin",sep=""),2,0,1)

# read in FAO to LPJmL country code key
country.key <- read.table(paste(fao.path,"countries_fao2lpjml.txt",sep=""),header=T)
country.lpjml2wr <- country.key$Region_Code_MagPIE[1:197]

#----------
# read fao data
#----------
country.selected<<-selectCountries(cowfile.global,cowfile.local,PROPORTION) #this index start from 0 not from 1
fao.yields<-getYields(FAO_YearChosen=fao.years )/10000
#remove exclude countries fao data
countries.exclude.index<-is.na(match(c(1:n.country.lpjml),country.selected+1))
fao.yields[,countries.exclude.index]<-NA


#read lpj calibration data
lpj.yields<-readCalibOutput(dir=lpjoutput.path,
		ncells=ncell,
		nbands=nbands,
		startyear=baseyear,
		year=fao.years,
		RUN)


lpj.yields<-lpj.yields[BAND_EXAM$lpj,,]
fao.yields<-fao.yields[BAND_EXAM$fao,]

lpj.yields<-lpj.yields[,country.selected+1,]
fao.yields<-fao.yields[,country.selected+1]
fao.yields[fao.yields==0]<-NA


#write csv by crops
delta<-array(NA,dim(lpj.yields))
for(i in 1:RUN)
delta[,,i]<-(lpj.yields[,,i]-fao.yields)/fao.yields



cat("computing best LAIMAX\n")
for(i in 1:RUN)
	delta[,,i]<-abs(lpj.yields[,,i]-fao.yields)
	
best_laimax<-array(NA,c( length(BAND_EXAM$lpj),length(country.selected)))
best_yields<-array(NA, dim(best_laimax))

for(c in 1:length(country.selected)){
	for(b in 1:length(BAND_EXAM$lpj)){
		p<-which(delta[b,c,]==min(delta[b,c,]))
		if(length(p)!=0) best_laimax[b,c]<-p[1]
		else best_laimax[b,c]<-NA		
		best_yields[b,c]<-lpj.yields[b,c,best_laimax[b,c]]
	}
}


#create laimax table, like treedens

#treedens<-t(array(c(RUN,length(BAND_EXAM$lpj)),c(1:RUN)))
#treedens<-t(array(c(1:RUN),c(RUN,length(BAND_EXAM$lpj))))

#treedens<-array(dim=c(RUN,length(BAND_EXAM$lpj)))

treedens<-array(c(1:7),c(11,100))

	
#write tree density table
#treedens<-read.csv(paste(src.path,"tree_dens.csv",sep=""),sep="\t",header=F)
#treedens<-t(treedens)
#out<-array("NA",c(length(country.selected),RUN+4))
#for(i in 1:(length(BAND_EXAM$lpj-1))){
#	out[,2:(RUN+1)]<-round(lpj.yields[i,,],digits=2)
#	out[,1]<-as.vector(country.key$Country_MAgPIE[country.selected+1])
#	out[,(RUN+2)]<-round(fao.yields[i,],digits=2)
#	out[,(RUN+3)]<-round(best_yields[i,],digits=2)
#	out[,(RUN+4)]<-round(digits=3,treedens[k_est[i,],i+1])
#	colnames(out)<-c("country",round(treedens[1:11,(i+1)],digits=3),"fao","yields_chosen","k_est")

#	write.csv(file=paste("yields_",cropnames[BAND_EXAM$lpj[i]],".csv",sep=""),out)

#}

#write .par file

#a<-read.csv("src/laimax.par",sep="\t")
#output<-data.frame(a[1:2])
#for(b in 1:length(BAND_EXAM$lpj)){
#	bandcountry<-array(-1,197)
#	for(i in 1:length(country.selected)){
#		bandcountry[country.selected[i]+1]<-as.numeric(treedens[k_est[b,i],(b+1)])
#	}
#	bandcountry[is.na(bandcountry)]<- -1
#	output<-data.frame(output,bandcountry)
# }
# zz<-file(paste("_lpjlaimax_output", ".par",sep=""),"wb")
# writeLines("#include \"../include/managepar.h\"",zz,useBytes=FALSE)
# writeLines("197",zz,useBytes=FALSE)
# for(i in 1:dim(output)[1]){
# 	text<-paste(as.character(output[i,1]),
# 			paste("\"",as.character(output[i,2]),"\"",sep=""),
# 			paste(as.character(round(output[i,3:dim(output)[2]],digits=4)),collapse="\t"),
# 			"-1","-1","-1",
# 			sep="\t")
# 	writeLines(text, con =zz, useBytes = FALSE)
# }
# close(zz)


source("src/plot.r")
