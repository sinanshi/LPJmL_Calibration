# deletes all variables
# rm(list=ls(all=TRUE))

# load required libaries
require(fields)
library(maps)
# assignments
ncell <- 67420 # no of cells recorded in file5794#
ncell.in <- 67420#5794#
res <- .5 # resolution in degrees
baseyear <- 1901 # first year recorded in file
startyear <- 40#99 #96 #first year to evaluate
endyear <- 45 #100 #last year to evaluate
fao.years <- c(1961:2003)
avg <- endyear-startyear +1
experiment <- "revision 1101"
ncols <- 360/res
nrows <- 180/res
nreg <- 197
nbands <- 52#32
nwr <- 10 # MAgPIE world regions10
ncft <<- 19#13 #skipping gras & others 12  19
#n.country.fao <- 351
n.country.lpjml <- 197
nband.date <- 22   # no of bands in sdate/hdate files
outgrid.scalar<-0.01

format <- 4

NOIRRIG <- F
PLOTALL <- F



cropnames <-c("Temperate Cereals","Rice","Maize","Tropical Cereals","Pulses","Temperate Roots",
	   "Potatoes","Tropical Roots","Sunflower","Soybeans","Groundnuts","Rapeseed","Sugar Cane",
	   "Others","Manage Grasslands","Bioenergy Grass","Bioenergy Tree","Citrus","Non Citrus Orchards",
	   "Date Palm","Olives","Nuts Trees","Grapes","Vegetables","Cotton","Fodder grass")

FAO_Item<-c("Wheat", "Rice, paddy", "Maize", "Millet","Peas, dry", "Sugar beet", 
 		"Potatoes", "Cassava","Sunflower seed","Soybeans", "Groundnuts, with shell","Rapeseed", "Sugar cane", 
 		NA,NA,NA,NA,
 		"Oranges","Apples","Dates","Olives","Almonds, with shell", "Grapes", 
 		#NA,NA,NA,NA,NA,NA,
 		NA, NA, NA)
if(PLOTALL==T){
cat("List of Crop names in LPJ and FAO\n=======\n")
cat(paste(cropnames,"->",FAO_Item,"\n"))
}


band_index<-(which(is.na(FAO_Item)==FALSE))
FRESHMATTER <-  100 / c(88, 87, 88, 88, 90, 24,20, 35, 93, 91, 94, 92, 27, 100, 100 ,100 ,100,13,16,70,30,90,10,10,91,100) 

map.build<-function(raw_){
map<-array(NA, dim=c(NR,NC))
for(i in 1:length(raw_))
    map[ind_lon[i],ind_lat[i]]<-raw_[i]
    return(map)
}

# 
# read.output.oneband<-function(file,year,band,nband,ncell,cellsize){
# 	seek(file, where=(year-1)*nbands*ncell*cellsize+(band-1)*ncell*cellsize,origin="start")
# 	buf <- readBin(file.yield.lpj,double(),size=cellsize,n=ncell)
# 	return(buf)
# 	}



makeCountryMap<-function(country.val, country.index,cow.country,withNa=TRUE){
       map<-array(NA, dim=c(NR,NC))
     for(i  in 1:length(country.index)){
	     p<-which(cow.country==country.index[i])
	     for(j in p)
	     map[ind_lon[j],ind_lat[j]]<-country.val[i]
	  }
     if(withNa==FALSE){
	     map[is.na(map)]<--9999
	 }
return(map)
}
######################
## quality measures ##
######################
N <- function (o, p) {
  if (length(o) == 0 || length(p) == 0) stop ("vector of length zero")
  if (length(o) != length(p) && length(p) != 1) stop ("incompatible dimensions")
  length(o)
}
Willmott <- function (o, p) {
  N(o, p)
  willmott <- 1 - sum((p - o)^2) / sum((abs(p - mean(o)) + abs(o - mean(o)))^2)
  willmott
}
EF <- function (o, p) {
  N(o, p)
  EF <- 1 - sum((p - o)^2) / sum((o - mean(o))^2)
  EF
}

# function to draw contour lines
masklines1 <- function(clines){
  lines(clines[[2]],clines[[3]],lwd=.5)
}
masklines05 <- function(clines){
  lines(clines[[2]],clines[[3]],lwd=.5)
}

deg2rad <- function(deg){
  return (deg*pi*0.00555555555555)
}

# read country codes
lpjinput.path <- "/home/sinan/workspace/lpj_calibration/lpjinputs/"#med/"
lpjoutput.path <- "/home/sinan/workspace/lpj_calibration/lpjoutputs/"#med/"
textflag <- "revision_1200"
fao.path<-"~/workspace/lpj_calibration/FAO/"


map.region.nr <- array(-9999,dim=c(ncols,nrows))
region.nr <- array(0,dim=ncell)
source(paste(fao.path,"readFAO.r",sep=""))
source("lpjutil.r")

# fao.yields<-(getYields()/10000)


read.input.grid(lpjinput.path)

Longitude<<-seq(WEST,EAST,RES)
Latitude<<-seq(SOUTH,NORTH,RES)

read.output.grid(lpjoutput.path,ncells=ncell,scalar=outgrid.scalar,cellsize=res)
cow<-read.input.files(paste(lpjinput.path,"cow_mg_2006.bin",sep=""),data.size=2)
cow.country<-cow[1,1,]
cow.region<-cow[2,1,]
cow.country.list<-sort(unique(cow.country))
cow.country.list<-cow.country.list[which(cow.country.list>=0)]
num.lpjcountry<-length(cow.country.list)
grid.area<-  (111e3*res)*(111e3*res)*cos(deg2rad(lat))/10000#ha
file.yield.lpj <- file(paste(lpjoutput.path,"pft_harvest.pft.bin",sep=""),"rb")
file.cropfrac <- file(paste(lpjoutput.path,"cftfrac.bin",sep=""),"rb")


rainfed <- array(0,dim=c(ncell,ncft))
irrigated <- array(0,dim=c(ncell,ncft))
rainfed.cropfrac <- array(0,dim=c(ncell,ncft))
irrigated.cropfrac <- array(0,dim=c(ncell,ncft))
grid.area.band<-matrix(grid.area,ncell,ncft)

#------------------------------
#yearly mean
#------------------------------
# country.average.year<-array(NA, c(num.lpjcountry, ncft,avg))  # country, ncft, years
# for(year in startyear:endyear){
# 	for(band in 1:ncft){
# 		seek(file.yield.lpj, where=(year-1)*nbands*ncell*format+(band_index[band]-1)*ncell*format,origin="start")
# 		buf <- readBin(file.yield.lpj,double(),size=format,n=ncell)
# 		rainfed[,band]<-buf/0.45*0.01*FRESHMATTER[band_index[band]] #t FM/ha
# 
# 
#     seek(file.cropfrac, where=(year-1)*nbands*ncell*format+(band_index[band]-1)*ncell*format,origin="start")
#     buf <- readBin(file.cropfrac,double(),size=format,n=ncell)
#     rainfed.cropfrac[,band] <-buf
# 
#      band2 <- band_index[band]+nbands/2
#      seek(file.yield.lpj, where=(year-1)*nbands*ncell*format+(band2-1)*ncell*format,origin="start")
#      buf <- readBin(file.yield.lpj,double(),size=format,n=ncell)
#      irrigated[,band]<-buf/0.45*0.01*FRESHMATTER[band_index[band]] #t FM/ha
# 
#      seek(file.cropfrac, where=(year-1)*nbands*ncell*format+(band2-1)*ncell*format,origin="start")
#      buf <- readBin(file.cropfrac,double(),size=format,n=ncell)
#       irrigated.cropfrac[,band] <-buf
# }
# 	rainfed.croparea <- rainfed.cropfrac*grid.area.band
# 	irrigated.croparea<-irrigated.cropfrac*grid.area.band
# 	total.croparea<-rainfed.croparea+irrigated.croparea
# 	for(i in 1:num.lpjcountry){
# 		for(band in 1: ncft){
# 			this.country<-which(cow.country==cow.country.list[i])
# 		if(NOIRRIG){
# 			country.average.year[i,band,(year-startyear+1)]<-sum(rainfed[this.country, band]*rainfed.croparea[this.country,band])/sum(rainfed.croparea[this.country,band])
# 			}
# 		else{
# 			country.average.year[i,band,(year-startyear+1)]<-sum(rainfed[this.country, band]*rainfed.croparea[this.country,band]
# 			                                                      +irrigated[this.country,band]*irrigated.croparea[this.country,band])/sum(total.croparea[this.country,band])
# 			}
# 		}
# 	}
# }
# 
# 
# close(file.yield.lpj)
# close(file.cropfrac)
# country.average.year[is.nan(country.average.year)]<-0.0
# country.average<-rowMeans(country.average.year,dims=2)


# #------------------------------
# #Correct
# #------------------------------
# country.average.year<-array(0, c(num.lpjcountry, ncft))  # country, ncft, years
# for(year in startyear:endyear){
# 	for(band in 1:ncft){
# 		seek(file.yield.lpj, where=(year-1)*nbands*ncell*format+(band_index[band]-1)*ncell*format,origin="start")
# 		buf <- readBin(file.yield.lpj,double(),size=format,n=ncell)
# 		rainfed[,band]<-buf/0.45*0.01*FRESHMATTER[band_index[band]] #t FM/ha
# 
# 
#     seek(file.cropfrac, where=(year-1)*nbands*ncell*format+(band_index[band]-1)*ncell*format,origin="start")
#     buf <- readBin(file.cropfrac,double(),size=format,n=ncell)
#     rainfed.cropfrac[,band] <-buf
# 
#      band2 <- band_index[band]+nbands/2
#      seek(file.yield.lpj, where=(year-1)*nbands*ncell*format+(band2-1)*ncell*format,origin="start")
#      buf <- readBin(file.yield.lpj,double(),size=format,n=ncell)
#      irrigated[,band]<-buf/0.45*0.01*FRESHMATTER[band_index[band]] #t FM/ha
# 
#      seek(file.cropfrac, where=(year-1)*nbands*ncell*format+(band2-1)*ncell*format,origin="start")
#      buf <- readBin(file.cropfrac,double(),size=format,n=ncell)
#       irrigated.cropfrac[,band] <-buf
# }
# 	rainfed.croparea <- rainfed.cropfrac*grid.area.band
# 	irrigated.croparea<-irrigated.cropfrac*grid.area.band
# 	total.croparea<-rainfed.croparea+irrigated.croparea
# 	for(i in 1:num.lpjcountry){
# 		for(band in 1: ncft){
# 			this.country<-which(cow.country==cow.country.list[i])
# 		if(NOIRRIG){
# 			country.average.year[i,band]<-country.average.year[i,band]+sum(rainfed[this.country, band]*rainfed.croparea[this.country,band])/sum(rainfed.croparea[this.country,band])
# 			}
# 		else{
# 			country.average.year[i,band]<-country.average.year[i,band]+sum(rainfed[this.country, band]*rainfed.croparea[this.country,band]
# 			                                                      +irrigated[this.country,band]*irrigated.croparea[this.country,band])/sum(total.croparea[this.country,band])
# 			}
# 		}
# 	}
# }
# 
# 
# close(file.yield.lpj)
# close(file.cropfrac)
# country.average[is.nan(country.average)]<-0.0
# 
# country.average<-country.average#*avg

#------------------------------
#CM
#------------------------------
country.average<-array(NA, c(num.lpjcountry, ncft)) 
for(band in 1:ncft){
	
  for(year in startyear:endyear){
	  seek(file.yield.lpj, where=(year-1)*nbands*ncell*format+(band_index[band]-1)*ncell*format,origin="start")
    buf <- readBin(file.yield.lpj,double(),size=format,n=ncell)
     rainfed[,band] <- rainfed[,band]  + buf/0.45*0.01*FRESHMATTER[band_index[band]] #t FM/ha
   seek(file.cropfrac, where=(year-1)*nbands*ncell*format+(band_index[band]-1)*ncell*format,origin="start")
   buf <- readBin(file.cropfrac,double(),size=format,n=ncell)
    rainfed.cropfrac[,band] <- rainfed.cropfrac[,band]  + buf
    band2 <- band_index[band]+nbands/2
    seek(file.yield.lpj, where=(year-1)*nbands*ncell*format+(band2-1)*ncell*format,origin="start")
    buf <- readBin(file.yield.lpj,double(),size=format,n=ncell)
    irrigated[,band] <- irrigated[,band] + buf/0.45*0.01*FRESHMATTER[band_index[band]] #t FM/ha
    seek(file.cropfrac, where=(year-1)*nbands*ncell*format+(band2-1)*ncell*format,origin="start")
    buf <- readBin(file.cropfrac,double(),size=format,n=ncell)
    irrigated.cropfrac[,band] <- irrigated.cropfrac[,band] + buf
  } # end for year 
  
  rainfed <- rainfed/avg
  irrigated <- irrigated/avg
  #rainfed.cropfrac <- rainfed.cropfrac/avg
  #irrigated.cropfrac <- irrigated.cropfrac/avg
  rainfed.croparea <- rainfed.cropfrac*grid.area.band/avg
  irrigated.croparea<-irrigated.cropfrac*grid.area.band/avg
  total.croparea<-rainfed.croparea+irrigated.croparea
  for(i in 1:num.lpjcountry){
	  for(band in 1: ncft){
		  this.country<-which(cow.country==cow.country.list[i])
		  if(NOIRRIG){
			  country.average[i,band]<-sum(rainfed[this.country, band]/avg*rainfed.croparea[this.country,band]/avg)/sum(rainfed.croparea[this.country,band]/avg)
			}
		else{
			country.average[i,band]<-sum(rainfed[this.country, band]*rainfed.croparea[this.country,band]
			                                                      +irrigated[this.country,band]*irrigated.croparea[this.country,band])/sum(total.croparea[this.country,band])
			}
		}
	}
}	
close(file.yield.lpj)
close(file.cropfrac)
country.average[is.nan(country.average)]<-0.0
# country.average<-country.average


  color1 <- colorRampPalette(c("red4","red","orange","lightgoldenrod","yellow","green","green4"))
  color2 <- rep(rainbow(50)[1:45],times=100)
  col.yields <- color1(25)
  col.yields[1] <- "grey"
  

  
for(band in 1:ncft){
	cat("Making Plots",band,"\n")
	#   #overall picture (one2one_line, LPJmL yield map, FAO yield map)
	filename <- (paste(lpjoutput.path,"lpjml_vs_fao_all_",cropnames[band_index[band]],"_","d.png",sep=""))
	png(filename,height=3.4*300,width=5*300,res=300,pointsize=6,type="cairo")
	par(oma=c(0,0,0,0))
	# first divide up the figure region\
	split.screen(c(1,2))
	# now divide screen into the figure region and legend colorbar on the
	# right to put a legend.
	split.screen( rbind(c(0,0.5,0.9,1), c(0,0.5,0.55,.9), c(0,0.5,0.55,1), c(0.5,1,0,0.5), c(0.5,1,0.5,1)))->ind
	split.screen( rbind(c(0,1,0.01,1), c(0,1,0,.01)),screen=1)->ind
	split.screen( rbind(c(0,1,0.55,1), c(0,1,0.1,.55), c(0,1,0,.1)),screen=2)->ind2
	#======================
	screen(ind[1])
	par(mar=c(3,2.8,3,3.5))
	tit <- sprintf("%s yields FAO vs. LPJmL, 1996-2000 ",cropnames[band_index[band]])
	plot(fao.yields[band,],country.average[,band],main="",xlab="",ylab="",col="green",pch=3,lwd=0.8)
	title(main=tit,xlab=list("FAO yields [t FM/ha]"),ylab=list("LPJmL yields [t FM/ha]"),line=1.5)
	if(max(fao.yields[band,])>0){
		tt <- fao.yields[band, ]*country.average[,band]
		fao1 <- fao.yields[band,]
		lpj1 <- country.average[,band]
		wiltext <- sprintf("Willmott: %g",round(Willmott(fao1[tt>0],lpj1[tt>0]),2))#
		text(max(fao.yields[band,])*1/3,max(fao.yields[band,],country.average[,band]),labels=wiltext)
		eftext <- sprintf("EF: %g",round(EF(fao1[tt>0],lpj1[tt>0]),2))
		text(max(fao.yields[band,])*2/3,max(country.average[,band]),labels=eftext)
		}
  onetoone <- array(0,2)
  onetoone[1] <- min(country.average[,band],fao.yields[band,])
  onetoone[2] <- max(country.average[,band],fao.yields[band,])
  lines(onetoone,onetoone,lwd=1)
   legend("bottomright",legend=c("acceptable","strong overestimation (>200%)","underestimation (<50%)","no yields"),col=c("green","blue","red","orange"),pch=1,pt.lwd=1,pt.cex=1,cex=0.7)
#======================
  
  
  map.lpj<-makeCountryMap(country.val=country.average[,band],country.index=cow.country.list,
			                                     cow.country=cow.country)
  
  map.fao<-makeCountryMap(country.val=fao.yields[band,],country.index=seq(0,196),
			                                     cow.country=cow.country)
  
  screen(ind2[1])
  par(mar=c(0,2,2,3))
  maxyield <- max(map.lpj,map.fao,na.rm=TRUE)
  tit <- sprintf("LPJmL %s yields, 1996-2000 ",cropnames[band_index[band]])
  
  image(x=Longitude, y=Latitude,map.lpj,zlim=c(0,maxyield), col=col.yields,xlab="",ylab="",
             main=tit)
  #  map(add=T)
  screen(ind2[2])
  par(mar=c(0,2,2,3))
  tit <- sprintf("FAO %s yields, 1996-2000",cropnames[band_index[band]])    
  image(x=Longitude,y=Latitude,map.fao,zlim=c(0,maxyield), col=col.yields,xlab="",ylab="",
             main=tit)
  #map(add=T)
  screen(ind2[3])
  par(mar=c(.5,0,1,3),lwd=0.5)
  image.plot(zlim=c(0,maxyield), col=col.yields,horizontal=T,legend.only=T,line=0,
            legend.args=list( text="tFM/ha",cex=1, side=2, line=3, adj=0,las=1),
            smallplot=c(0.1,0.9, 0.5,.9),axis.args=list(lwd=0.5,mgp=c(0, .5, 0)))

  
# 
# 
   close.screen( all=TRUE)
   dev.off()
}
			
			
			
			
	
