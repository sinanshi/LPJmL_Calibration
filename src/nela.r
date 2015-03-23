# deletes all variables
rm(list=ls(all=TRUE))

#Last change M. Fader 23.02.2014

#LPJmL calibration script -> choosing LAImax for best yield match to FAO 

#for 52 bands output, global scale and longheader lpj inputs (grid and cow)

#A new calibration has to be done for every code development affecting annual crops and for any change 
#in inputs (for example changing CRU version)

#this code needs input yields from FAO all.csv, R function readFAO.r;country key countries_fao2lpjml.txt and 
#7 LPJ run for LAImax from 1 to 7 (cftfrac, pftharvest and grid)

#produce manage.par parameter table for calibrated run with LPJ; x/y plots and maps on agreement with FAO data; 
#statistical agreement coefficients (Willmott, EF)

#To use change paths of inputs and outputs; period of time for calibration; lpjoutput start year; 
#the revision number/name you are calibrating. Choose flags: if you want all plots (PLOTALL) and 
#whether the calibration will be done with rainfed yields or with mean of rainfed and irrigated 
#yields (NOIRRIG)

#Process developed by M. Fader. Script developed by M.Fader and C. Müller 
#Code modifications on reading FAO data and including new bands by S. Shi 

#source("/home/mfader/_Calibration/nela.r")
 
# load required libaries
require(fields)

# assignments
ncell <- 5794#67420 # no of cells recorded in file
ncell.in <- 5794#67420
res <- .5 # resolution in degrees
baseyear <- 1901 #first year recorded in lpj output file
startyear1 <- 2000 #first year to evaluate 
endyear1 <- 2009 #last year to evaluate 2009
fao.years <- c(2000:2009)#chose the years for fao data(Notice, the total years of FAO data is from 1961-2012)
experiment <- "revision Nela new crops"
ncols <- 360/res
nrows <- 180/res
nbands <- 52
nwr <- 10 # MAgPIE world regions10
ncft <<- 19 # ncft/2 is 26 but skipping man. gras, others, cotton, vegetables, fodder grass, bioenergy grass, bioenergy tree
#n.country.fao <- 351
n.country.lpjml <- 197
nreg <- 197
nband.date <- 22   # no of bands in sdate/hdate files

format <- 4 #output data size

NOIRRIG <- F # if you would like to calibrate only using rainfed yields, if false mean of rainfed and irrigated
PLOTALL <- F # if F do not write out sole plots


# # read country codes
# lpjinput.path <- "/home/mfader/_R/PlotInputs/in/global/inputs_longheader/" 
# lpjoutput.path <- "/home/mfader/_R/Plotoutputs/in/global/LAI/"
# output.path <- "/home/mfader/_R/Plotoutputs/out/Mediterranean/out_latest_1911_2009_repar/" #outputs of this code
# 

# read country codes
#lpjinput.path <- "/home/mfader/_R/PlotInputs/in/global/inputs_longheader/"
# lpjinput.path <- "/home/mfader/_R/PlotInputs/in/Mediterranean/inputs_basins/"
# lpjoutput.path <- "/home/mfader/_R/Plotoutputs/in/Mediterranean/out_latest_1961_2009_25/"
# output.path <-"/home/mfader/_R/Plotoutputs/out/Mediterranean/out_latest_1961_2009_25/calibration2/"


# lpjinput.path <- "/home/sinan/workspace/lpj_calibration/lpjinputs/med/"
# lpjoutput.path <- "/home/sinan/workspace/lpj_calibration/lpjoutputs/med/"
# output.path <-"/home/sinan/workspace/lpj_calibration/lpjoutputs/med/"
lpjinput.path <- "/home/sinan/workspace/tree_density/lpj_calibration/lpjinputs/med/"
lpjoutput.path <- "/home/sinan/workspace/tree_density/lpj_calibration/lpjoutputs/med/"
output.path <-"/home/sinan/workspace/tree_density/lpj_calibration/lpjoutputs/med/"
fao.path<-"~/workspace/tree_density/lpj_calibration/FAO/"
src.path<-"/home/sinan/workspace/tree_density/lpj_calibration/src/"


textflag <- "revision Nela new crops"
#fao.path<-"~/_Calibration/"
# fao.path<-"~/workspace/lpj_calibration/FAO/"
#############################################
startyear <- (startyear1-baseyear+1) 
endyear <- (endyear1-baseyear+1) 
eval.years <- paste(startyear1,"-",endyear1,sep="")
avg <- endyear-startyear+1
#display boundary
pxlim<-c(-15,52)# for mediterrian grid
pylim<-c(-5,55)

cropnames <-c("Temp_Cereals","Rice","Maize","Trop_Cereals","Pulses","Temp_Roots",
	   "Potatoes","Trop_Roots","Sunflower","Soybeans","Groundnuts","Rapeseed","Su_Cane",
	   "Others","Man_Grasslands","Bioen_Grass","Bioen_Tree","Citrus","Orchards",
	   "Date_Palm","Olives","Nuts_Trees","Grapes","Vegetables","Cotton","Fodder_grass")

FAO_Item<-c("Wheat", "Rice, paddy", "Maize", "Millet","Peas, dry", "Sugar beet", "Potatoes", "Cassava",
	  "Sunflower seed","Soybeans", "Groundnuts, with shell","Rapeseed", "Sugar cane", NA,NA,NA,NA,
	  "Oranges","Apples","Dates","Olives","Almonds, with shell", "Grapes", NA, NA, NA)
cat("List of Crop names in LPJ and FAO\n=======\n")
cat(paste(cropnames,"->",FAO_Item,"\n"))
cat("=======\n")

band_index<-(which(is.na(FAO_Item)==FALSE))

FRESHMATTER <-  100 / c(88, 87, 88, 88, 90, 24,20, 35, 93, 91, 94, 92, 27, 100, 25 ,100 ,100,13,16,70,30,90,10,10,91,25) 
# values in percent of drymatter, e.g. wheat has 88 % of drymatter and 22% of water

#Wirsenius for all crops
#water content of fruits (university of Kentuckcky)
#for grapes, 90 from wirsinius fruits
#for dates Encyclopaedia of Fresh Fruit
#for potatoes MECİT HALİL ÖZTOP OPTIMIZATION OF MICROWAVE FRYING OF POTATO SLICES
#for olives Producing Table Olives, Par Stan Kailis,David Harris
#for vegetables 80% water but we harvest all grass, actually the vegetables are only a part of the plant, let say 50%. So 30%
#for grass depends if hay, silage or fresh, between 90% and and  25% drymatter, 25 for pastures and foragge, 100 for bioenergy

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
#   willmott <- 1 - sum((p - o)^2) / sum((abs(p - mean(o)) + abs(o - mean(o)))^2)
   willmott <- 1 - sum(abs(p - o)) / sum((abs(p - mean(o)) + abs(o - mean(o))))
  willmott
}
EF <- function (o, p) {
  N(o, p)
  EF <- 1 - sum((p - o)^2) / sum((o - mean(o))^2)
  EF
}

  putTextOnBubbles<-function(fao,region,circle.radius){
	  index.far<-which(abs(region-fao)>0.1*max(fao))
	  index.big<-which(circle.radius>=mean(circle.radius[which(circle.radius!=0)]))
	  index.put<-intersect(index.far,index.big)
	  if(length(index.put)!=0)
	  text(fao[index.put],region[index.put],as.vector(country.key$Country_MAgPIE[index.put]),font=2)
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

map.region.nr <- array(-9999,dim=c(ncols,nrows))
 region.nr <- array(0,dim=ncell)

source(paste(src.path,"readFAO.r",sep=""))
source(paste(src.path,"selectCountry.r",sep=""))
source(paste(src.path,"lpjutil.r",sep=""))
source(paste(src.path,"read_lpj_calibration_output.r",sep=""))
# read in FAO to LPJmL country code key
country.key <- read.table(paste(fao.path,"countries_fao2lpjml.txt",sep=""),header=T)
# fao2lpjml.country <- array(-9,n.country.fao)
country.lpjml2wr <- array(-9,n.country.lpjml)
country.lpjml2wr <- country.key$Region_Code_MagPIE[1:197]

best.lai <- array(0,dim=c(nreg,ncft))  #To store the results of best  LAI
all.yield <- array(0,dim=c(nreg,ncft,11))
best.delta <- array(1000,dim=c(nreg,ncft))

ilat.in <- array(0,ncell.in)
ilon.in <- array(0,ncell.in)
ilat.out <- array(0,ncell.in)
ilon.out <- array(0,ncell.in)
lat.out <- array(0,ncell.in)
lon.out <- array(0,ncell.in)
lat.in <- array(0,ncell.in)
lon.in <- array(0,ncell.in)
grid.area <- array(0,ncell.in)
country <- array(-9,ncell.in)

file.grid.input <- file(paste(lpjinput.path,"grid.bin",sep=""),"rb")
file.grid.output <- file(paste(lpjoutput.path,"grid.bin",sep=""),"rb")
file.country.input <- file(paste(lpjinput.path,"cow_mg_2006.bin",sep=""),"rb")
seek(file.grid.input,where=43,start="origin") 
seek(file.country.input,where=43,start="origin") 
for(i in 1:ncell.in){
  lon.in[i] <- readBin(file.grid.input,integer(),size=2,n=1)/100
  lat.in[i] <- readBin(file.grid.input,integer(),size=2,n=1)/100
  ilon.in[i] <- as.integer((lon.in[i]+180)/res + 1.01)
  ilat.in[i] <- as.integer((lat.in[i]+90)/res + 1.01)
  bufi <- readBin(file.country.input,integer(),size=2,n=1)
  country[i] <- bufi + 1
  buf <- readBin(file.country.input,integer(),size=2,n=1)
  region.nr[i] <- country[i]
  map.region.nr[ilon.in[i],ilat.in[i]] <- region.nr[i]
}
# cat(lon.in[26415],lat.in[26415],"\n")
for(i in 1:ncell){
  lon.out[i] <- readBin(file.grid.output,integer(),size=2,n=1)/100
  lat.out[i] <- readBin(file.grid.output,integer(),size=2,n=1)/100
#   if(lon.out[i] == lon.in[26415] && lat.out[i]==lat.in[26415]){
#     #cat("Beauce output pix at ",i,"(",lon.out[i],lat.out[i],")\n")
#   }
  ilon.out[i] <- as.integer((lon.out[i]+180)/res + 1.01)
  ilat.out[i] <- as.integer((lat.out[i]+90)/res + 1.01)
  grid.area[i] <-  (111e3*res)*(111e3*res)*cos(deg2rad(lat.out[i]))/10000 #ha
}
close(file.grid.input)
close(file.grid.output)
close(file.country.input)

#----------
# read fao data
#----------
fao.yields<-getYields(FAO_YearChosen=fao.years )/10000
country.selected<<-selectCountries(cowfile.global,cowfile.local,PROPORTION) #this index start from 0 not from 1
#remove exclude countries fao data
countries.exclude.index<-is.na(match(c(1:n.country.lpjml),country.selected+1))
fao.yields[,countries.exclude.index]<-0


#loop through LAI values
for(lai in 1:11){ #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
textflag2 <- sprintf("lai_%d",lai)
cat("doing LAI",lai,"\n")

#read LPJmL yield maps
rainfed <- array(0,dim=c(ncell,ncft))
irrigated <- array(0,dim=c(ncell,ncft))
rainfed.cropfrac <- array(0,dim=c(ncell,ncft))
irrigated.cropfrac <- array(0,dim=c(ncell,ncft))

region.rainfed <- array(0,dim=c(nreg,ncft))
region.irrigated <- array(0,dim=c(nreg,ncft))
region.rainfed.croparea <- array(0,dim=c(nreg,ncft))
region.irrigated.croparea <- array(0,dim=c(nreg,ncft))
region.total.croparea <- array(0,dim=c(nreg,ncft))
wr.total.croparea <- array(0,dim=c(nwr,ncft))
avg.wr.lai <- array(0,dim=c(nwr,ncft))
region.average <- array(0,dim=c(nreg,ncft))
region.fao <- array(0,dim=c(nreg,ncft))
map.rainfed <- array(-9999,dim=c(ncols,nrows,ncft))
map.irrigated <- array(-9999,dim=c(ncols,nrows,ncft))
map.rainfed.cropfrac <- array(-9999,dim=c(ncols,nrows,ncft))
map.irrigated.cropfrac <- array(-9999,dim=c(ncols,nrows,ncft))
map.region.rainfed <- array(-9999,dim=c(ncols,nrows,ncft))
map.region.irrigated <- array(-9999,dim=c(ncols,nrows,ncft))
map.region.average <- array(-9999,dim=c(ncols,nrows,ncft))
map.fao <- array(-9999,dim=c(ncols,nrows,ncft))


file.yield.lpj <- file(paste(lpjoutput.path,"pft_harvest",lai,".pft.bin",sep=""),"rb")
file.cropfrac <- file(paste(lpjoutput.path,"cftfrac",lai,".bin",sep=""),"rb")
pix.test <- array(0,100)
for(band in 1:ncft){
  for(i in 1:ncell.in){
    if(country[i]>0){
      region.fao[country[i],band] <- fao.yields[band,country[i]]
      map.fao[ilon.in[i],ilat.in[i],band] <- fao.yields[band,country[i]]
    }
  }
  for(year in startyear:endyear){
    seek(file.yield.lpj, where=(year-1)*nbands*ncell*format+(band_index[band]-1)*ncell*format,origin="start")
    buf <- readBin(file.yield.lpj,double(),size=format,n=ncell)
    rainfed[,band] <- rainfed[,band]  + buf/0.45*0.01*FRESHMATTER[band_index[band]] #t FM/ha
    #pix.test[year] <- buf[21205]/0.45*0.01*FRESHMATTER[band_index[band]] #corresponds to Beauce 26415
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
  rainfed.cropfrac <- rainfed.cropfrac/avg
  irrigated.cropfrac <- irrigated.cropfrac/avg
  for(i in 1:ncell){
	  #TO ADD COUNTRY CRITERIA, Notice: +1 because country=cow+1
	  if(any(country.selected+1==country[i])){ 
    region <- map.region.nr[ilon.out[i],ilat.out[i]]
    # setting tropical pulses areas to ZERO for comparison with temperate FPEA
    if(band==5 && (lat.out[i]<30 && lat.out[i]> (-30))){
      rainfed.cropfrac[i,band] <- 0
      irrigated.cropfrac[i,band] <- 0
    }
    
    if(region>0){
      region.rainfed[region,band] <- region.rainfed[region,band] +
        rainfed[i,band]*rainfed.cropfrac[i,band]*grid.area[i]
      region.irrigated[region,band] <- region.irrigated[region,band] +
        irrigated[i,band]*irrigated.cropfrac[i,band]*grid.area[i]
      if(NOIRRIG){
        region.average[region,band] <- region.average[region,band] +
          (rainfed[i,band]*rainfed.cropfrac[i,band] +
          rainfed[i,band]*irrigated.cropfrac[i,band])*grid.area[i]
      } else {
        region.average[region,band] <- region.average[region,band] +
          (rainfed[i,band]*rainfed.cropfrac[i,band] +
          irrigated[i,band]*irrigated.cropfrac[i,band])*grid.area[i]
      }
      region.rainfed.croparea[region,band] <- region.rainfed.croparea[region,band] +
        rainfed.cropfrac[i,band]*grid.area[i]
      region.irrigated.croparea[region,band] <- region.irrigated.croparea[region,band] +
        irrigated.cropfrac[i,band]*grid.area[i]
      region.total.croparea[region,band] <- region.total.croparea[region,band] +
        (rainfed.cropfrac[i,band]+irrigated.cropfrac[i,band])*grid.area[i]
    }
    map.rainfed[ilon.out[i],ilat.out[i],band] <- rainfed[i,band]
    map.irrigated[ilon.out[i],ilat.out[i],band] <- irrigated[i,band]
  } # end for i in 1:ncell
}
   



  region.average[] <- region.average[]/region.total.croparea[]
  region.average[is.nan(region.average)] <- 0
  all.yield[,band,lai] <- region.average[,band]
  cat("region 55 France",region.average[55,band],"for",cropnames[band_index[band]],"\n")
  for(i in 1:nreg){
    delta <- abs(region.average[i,band]-fao.yields[band,i])
    if(best.delta[i,band]>delta && ((region.average[i,band]*fao.yields[band,i])>0)){
      best.delta[i,band] <- delta
      best.lai[i,band] <- lai
    }
  }

  for(i in 1:ncell){
    region <- map.region.nr[ilon.out[i],ilat.out[i]]
    if(region>0){
      map.region.rainfed[ilon.out[i],ilat.out[i],band] <- region.rainfed[region,band]/region.rainfed.croparea[region,band]
      map.region.irrigated[ilon.out[i],ilat.out[i],band] <- region.irrigated[region,band]/region.irrigated.croparea[region,band]
      map.region.average[ilon.out[i],ilat.out[i],band] <- region.average[region,band]
    }
  }
  map.region.rainfed[is.nan(map.region.rainfed)] <- 0
  map.region.irrigated[is.nan(map.region.irrigated)] <- 0
  map.region.average[is.nan(map.region.average)] <- 0
  map.rainfed[is.nan(map.rainfed)] <- 0
  map.irrigated[is.nan(map.irrigated)] <- 0
  map.region.rainfed[is.na(map.region.rainfed)] <- 0
  map.region.irrigated[is.na(map.region.irrigated)] <- 0
  map.region.average[is.na(map.region.average)] <- 0
  map.rainfed[is.na(map.rainfed)] <- 0
  map.irrigated[is.na(map.irrigated)] <- 0

  color1 <- colorRampPalette(c("red4","red","orange","lightgoldenrod","yellow","green","green4"))
  color2 <- rep(rainbow(50)[1:45],times=100)
  col.yields <- color1(25)
  image.plot(map.region.nr,zlim=c(0,max(map.region.nr)),col=rep(topo.colors(50),4))#col=color1(200))

  onetoone <- array(0,2)
  onetoone[1] <- min(region.average[,band],region.fao[,band])
  onetoone[2] <- max(region.average[,band],region.fao[,band])


if(PLOTALL){  
  filename <- sprintf("fao_vs_lpj_%s_yields_%s_%s.ong",cropnames[band_index[band]],eval.years,textflag2)
  png(paste(output.path,filename,sep=""),height=3.4*300,width=5*300,res=300,pointsize=8,type="cairo")
  tit <- sprintf("%s yields FAO vs. LPJmL, %s %s",cropnames[band_index[band]],eval.years,textflag2)
  plot(region.fao[,band],region.average[,band],main=tit,xlab="FAO yields [t FM/ha]",
       ylab="LPJmL yields [t FM/ha]",col="green",pch=3,lwd=0.8)
  circle.radius <- (region.total.croparea[,band]/max(region.total.croparea[,band])*max(region.fao[,band])/5)
  circle.radius[is.infinite(circle.radius)] <- 0
  symbols(region.fao[,band],region.average[,band],circles=circle.radius,add=T,cex=0.5,inches=F)
  points(region.fao[region.average[,band]>region.fao[,band],band],
         region.average[region.average[,band]>region.fao[,band],band],col="green",pch=3,lwd=0.8)
  text(region.fao[region.average[,band]>region.fao[,band],band],
         region.average[region.average[,band]>region.fao[,band],band],col="green",
       labels=as.vector(country.key$Country_MAgPIE[which(region.average[,band]/region.fao[,band]>2)]))
  
  
  points(region.fao[region.average[,band]/region.fao[,band]>2,band],
         region.average[region.average[,band]/region.fao[,band]>2,band],col="blue",pch=3,lwd=0.8)
  text(region.fao[region.average[,band]/region.fao[,band]>2,band],
       region.average[region.average[,band]/region.fao[,band]>2,band], col="blue",
       labels=as.vector(country.key$Country_MAgPIE[which(region.average[,band]/region.fao[,band]>2)]))
  
  points(region.fao[region.average[,band]/region.fao[,band]<0.5,band],
         region.average[region.average[,band]/region.fao[,band]<0.5,band],col="red",pch=3,lwd=0.8)
  
  text(region.fao[region.average[,band]/region.fao[,band]<0.5,band],
         region.average[region.average[,band]/region.fao[,band]<0.5,band],col="red",
         labels=as.vector(country.key$Country_MAgPIE[region.average[,band]/region.fao[,band]<0.5]))
  
  points(region.fao[region.average[,band]==0,band],
         region.average[region.average[,band]==0,band],col="orange",pch=3,lwd=0.8)
  lines(onetoone,onetoone,lwd=1)
  legend("bottomright",legend=c("acceptable","strong overestimation (>200%)","underestimation (<50%)","no yields"),col=c("green","blue","red","orange"),pch=1,pt.lwd=1,pt.cex=1,cex=0.7)
  if(max(region.fao[,band])>0){
    tt <- region.fao[,band]*region.average[,band]
    fao1 <- region.fao[,band]
    lpj1 <- region.average[,band]
    wiltext <- sprintf("Willmott: %g",round(Willmott(fao1[tt>0],lpj1[tt>0]),2))
    text(max(region.fao[,band])*0.3/3,max(region.average[,band]),labels=wiltext)
    eftext <- sprintf("EF: %g",round(EF(fao1[tt>0],lpj1[tt>0]),2))
    text(max(region.fao[,band])*2/3,max(region.average[,band]),labels=eftext)
  }
  dev.off()
  
  #plotting FAO yield map
  maxyield <- max(map.region.average[,,band],map.fao[,,band])
  filename <- sprintf("fao_%s_yields_%s_%s.ong",cropnames[band_index[band]],eval.years,textflag2)
  png(paste(output.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("%s yields according to FAO, %s %s",cropnames[band_index[band]],eval.years,textflag2)    
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.fao[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting rainfed yield map (pixel)
  maxyield <- max(map.rainfed[,,band])
  filename <- sprintf("lpjml_rainfed_%s_yields_%s_%s.ong",cropnames[band_index[band]],eval.years,textflag2)
  png(paste(output.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("%s yields (rainfed) according to LPJmL, %s %s",cropnames[band_index[band]],eval.years,textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.rainfed[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting rainfed yield map (country)
  maxyield <- max(map.region.rainfed[,,band])
  filename <- sprintf("lpjml_rainfed_regional_%s_yields_%s_%s.ong",cropnames[band_index[band]],eval.years,textflag2)
  png(paste(output.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("Regional %s yields (rainfed) according to LPJmL, %s %s",cropnames[band_index[band]],eval.years,textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.rainfed[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit)
#   #invisible(lapply(country.line,masklines05))
#   #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting average yield map (country)
  maxyield <- max(map.region.average[,,band],map.fao[,,band])
  filename <- sprintf("lpjml_average_regional_%s_yields_%s_%s.ong",cropnames[band_index[band]],eval.years,textflag2)
  png(paste(output.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("Regional %s yields according to LPJmL, %s %s",cropnames[band_index[band]],eval.years,textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.average[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit)
#   #invisible(lapply(country.line,masklines05))
#   #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting irrigated yield map (pixel)
  maxyield <- max(map.irrigated[,,band])
  filename <- sprintf("lpjml_irrigated_%s_yields_%s_%s.ong",cropnames[band_index[band]],eval.years,textflag2)
  png(paste(output.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("%s yields (irrigated) according to LPJmL, %s %s",cropnames[band_index[band]],eval.years,textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.irrigated[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting irrigated yield map (country)
  maxyield <- max(map.region.irrigated[,,band])
  filename <- sprintf("lpjml_irrigated_regional_%s_yields_%s_%s.ong",cropnames[band_index[band]],eval.years,textflag2)
  png(paste(output.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("Regional %s yields (irrigated) according to LPJmL, %s %s",cropnames[band_index[band]],eval.years,textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.irrigated[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  dev.off()
  
} # end if(PLOTALL)
  
  #overall picture (one2one_line, LPJmL yield map, FAO yield map)
  filename <- (paste(output.path,"lpjml_vs_fao_all_",eval.years,"_",cropnames[band_index[band]],"_",textflag2,"d.png",sep=""))
  png(filename,height=3.4*300,width=5*300,res=300,pointsize=6,type="cairo")
  par(oma=c(0,0,0,0))
  # first divide up the figure region\
  split.screen(c(1,2))
  # now divide screen into the figure region and legend colorbar on the
  # right to put a legend.
  #split.screen( rbind(c(0,0.5,0.9,1), c(0,0.5,0.55,.9), c(0,0.5,0.55,1), c(0.5,1,0,0.5), c(0.5,1,0.5,1)))->ind
  split.screen( rbind(c(0,1,0.01,1), c(0,1,0,.01)),screen=1)->ind
  split.screen( rbind(c(0,1,0.55,1), c(0,1,0.1,.55), c(0,1,0,.1)),screen=2)->ind2

  screen(ind[1])
  par(mar=c(3,2.8,3,3.5))
  tit <- sprintf("%s yields FAO vs. LPJmL, %s %s",cropnames[band_index[band]],eval.years,textflag2)
  plot(region.fao[,band],region.average[,band],main="",xlab="",
       ylab="",col="green",pch=3,lwd=0.8)
  title(main=tit,xlab=list("FAO yields [t FM/ha]"),ylab=list("LPJmL yields [t FM/ha]"),line=1.5)
  circle.radius <- (region.total.croparea[,band]/max(region.total.croparea[,band])*max(region.fao[,band])/5)
  circle.radius[is.infinite(circle.radius)] <- 0
  circle.radius[circle.radius<0] <- 0
  symbols(region.fao[,band],region.average[,band],circles=circle.radius,add=T,cex=0.5,inches=F)
  points(region.fao[region.average[,band]>region.fao[,band],band],
         region.average[region.average[,band]>region.fao[,band],band],col="green",pch=3,lwd=0.8)
  points(region.fao[region.average[,band]/region.fao[,band]>2,band],
         region.average[region.average[,band]/region.fao[,band]>2,band],col="blue",pch=3,lwd=0.8)
  points(region.fao[region.average[,band]/region.fao[,band]<0.5,band],
         region.average[region.average[,band]/region.fao[,band]<0.5,band],col="red",pch=3,lwd=0.8)
  points(region.fao[region.average[,band]==0,band],
         region.average[region.average[,band]==0,band],col="orange",pch=3,lwd=0.8)
  putTextOnBubbles(region.fao[,band],region.average[,band],circle.radius)
  
  lines(onetoone,onetoone,lwd=1)
  legend("bottomright",legend=c("acceptable","strong overestimation (>200%)","underestimation (<50%)","no yields"),col=c("green","blue","red","orange"),pch=1,pt.lwd=1,pt.cex=1,cex=0.7)
  if(max(region.fao[,band])>0){
    tt <- region.fao[,band]*region.average[,band]
    fao1 <- region.fao[,band]
    lpj1 <- region.average[,band]
    wiltext <- sprintf("Willmott: %g",round(Willmott(fao1[tt>0],lpj1[tt>0]),2))
    text(max(region.fao[,band])*0.6/3,max(region.average[,band]),labels=wiltext)
    eftext <- sprintf("EF: %g",round(EF(fao1[tt>0],lpj1[tt>0]),2))
    text(max(region.fao[,band])*2/3,max(region.average[,band]),labels=eftext)
    }
  screen(ind2[1])
  par(mar=c(0,2,2,3))
  maxyield <- max(map.region.average[,,band],map.fao[,,band])
  tit <- sprintf("LPJmL %s yields, %s %s",cropnames[band_index[band]],eval.years,textflag2)
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.average[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit)
  #add zero values
  zero<-map.region.average[,,band]
  zero[which(zero!=0)]<-NA
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),zero, add=T,
	     zlim=c(0,maxyield), xlim=pxlim,ylim=pylim, col="grey",xlab="",ylab="")
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  screen(ind2[2])
  par(mar=c(0,2,2,3))
  tit <- sprintf("FAO %s yields, %s",cropnames[band_index[band]],eval.years)    
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.fao[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit)
  #add zero values
  zero<-map.fao[,,band]
  zero[which(zero!=0)]<-NA
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),zero, add=T,
	     zlim=c(0,maxyield), xlim=pxlim,ylim=pylim, col="grey",xlab="",ylab="")
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  screen(ind2[3])
  par(mar=c(.5,0,1,3),lwd=0.5)
  image.plot(zlim=c(0,maxyield), col=col.yields,horizontal=T,legend.only=T,line=0,
            legend.args=list( text="tFM/ha",cex=1, side=2, line=3, adj=0,las=1),
            smallplot=c(0.1,0.9, 0.5,.9),axis.args=list(lwd=0.5,mgp=c(0, .5, 0)))


  close.screen( all=TRUE)
  dev.off()

} #end for band in 1:nbands
close(file.yield.lpj)
close(file.cropfrac)
} # end for(lai)


col.lai <- color1(11)
map.region.bestlai <- array(-9999,dim=c(ncols,nrows,ncft))
map.region.bestyield <- array(-9999,dim=c(ncols,nrows,ncft))

#computing average.world.region best LAImax
for(i in 1:nreg){
    avg.wr.lai[country.lpjml2wr[i],] <- avg.wr.lai[country.lpjml2wr[i],] + best.lai[i,] * region.total.croparea[i,]
    wr.total.croparea[country.lpjml2wr[i],] <- wr.total.croparea[country.lpjml2wr[i],] + region.total.croparea[i,]
}
avg.wr.lai2 <- avg.wr.lai/wr.total.croparea
avg.wr.lai2[is.na(avg.wr.lai2)] <- 0
for(w in 1:nwr){
  avg.wr.lai[w,] <- as.integer(avg.wr.lai2[w,]+.5)
}
avg.wr.lai[avg.wr.lai < 1] <- 1

for(band in 1:ncft){

  reg.yield <- array(0,nreg)  
  for(i in 1:nreg){
    if(best.lai[i,band]>0){
      reg.yield[i] <- all.yield[i,band,best.lai[i,band]]
    } else {
      best.lai[i,band] <- avg.wr.lai[country.lpjml2wr[i],band]
    }
  }

  for(i in 1:ncell){
    region <- map.region.nr[ilon.out[i],ilat.out[i]]
    if(region>0 && (best.lai[region,band]>0)){
      if(region.average[region,band]>0) {
        map.region.bestlai[ilon.out[i],ilat.out[i],band] <- best.lai[region,band]
      } else {
        map.region.bestlai[ilon.out[i],ilat.out[i],band] <- best.lai[region,band]
      }
      map.region.bestyield[ilon.out[i],ilat.out[i],band] <- all.yield[region,band,best.lai[region,band]]
    }
  }
  map.region.bestlai[is.nan(map.region.rainfed)] <- 0
  map.region.bestyield[is.nan(map.region.irrigated)] <- 0
  map.region.bestlai[is.na(map.region.rainfed)] <- 0
  map.region.bestyield[is.na(map.region.average)] <- 0

  #overall picture (one2one_line, best LAI, LPJmL yield map, FAO yield map)
  filename <- (paste(output.path,"lpjmlcalib_vs_fao_all_",eval.years,"_",cropnames[band_index[band]],"d.png",sep=""))
  png(filename,height=3.4*300,width=5*300,res=300,pointsize=6,type="cairo")
  par(oma=c(0,0,0,0))
  # first divide up the figure region
  split.screen(c(1,2))
  # now divide screen into the figure region and legend colorbar on the
  # right to put a legend.
  split.screen( rbind(c(0,1,0.55,1), c(0,1,0.1,.55), c(0,1,0,.1)),screen=1)->ind
  split.screen( rbind(c(0,1,0.55,1), c(0,1,0.1,.55), c(0,1,0,.1)),screen=2)->ind2

  screen(ind[1])
  par(mar=c(3,2.8,3,3.5),cex=0.9)
  onetoone[1] <- min(reg.yield,region.fao[,band])
  onetoone[2] <- max(reg.yield,region.fao[,band])
  tit <- sprintf("%s yields, %s %s",cropnames[band_index[band]],eval.years,textflag)
  plot(region.fao[,band],reg.yield,main="",xlab="",
       ylab="",col="green",pch=3,lwd=0.8)
  title(main=tit,xlab=list("FAO yields [t FM/ha]"),ylab=list("LPJmL yields [t FM/ha]"),line=1.5)
  circle.radius <- (region.total.croparea[,band]/max(region.total.croparea[,band])*max(region.fao[,band])/5)
  circle.radius[is.infinite(circle.radius)] <- 0
  circle.radius[circle.radius<0] <- 0
  symbols(region.fao[,band],reg.yield,circles=circle.radius,add=T,cex=0.5,inches=F)
  points(region.fao[reg.yield>region.fao[,band],band],
         reg.yield[reg.yield>region.fao[,band]],col="green",pch=3,lwd=0.8)
    points(region.fao[reg.yield/region.fao[,band]>2,band],
         reg.yield[reg.yield/region.fao[,band]>2],col="blue",pch=3,lwd=0.8)
  points(region.fao[reg.yield/region.fao[,band]<0.5,band],
         reg.yield[reg.yield/region.fao[,band]<0.5],col="red",pch=3,lwd=0.8)
  points(region.fao[reg.yield==0,band],
         reg.yield[reg.yield==0],col="orange",pch=3,lwd=0.8)
  lines(onetoone,onetoone,lwd=1)
  legend("bottomright",legend=c("acceptable","strong overestimation (>200%)","underestimation (<50%)","no yields"),col=c("green","blue","red","orange"),pch=1,pt.lwd=1,pt.cex=1,cex=0.7)
  if(max(region.fao[,band])>0){
    tt <- region.fao[,band]*region.average[,band]
    fao1 <- region.fao[,band]
    lpj1 <- reg.yield
    print(fao1[tt>0])
    print(lpj1[tt>0])
    wiltext <- sprintf("Willmott: %g",round(Willmott(fao1[tt>0],lpj1[tt>0]),2))
    text(max(region.fao[,band])*0.6/3,max(reg.yield),labels=wiltext)
    eftext <- sprintf("EF: %g",round(EF(fao1[tt>0],lpj1[tt>0]),2))
    text(max(region.fao[,band])*2/3,max(reg.yield),labels=eftext)
  }
  screen(ind[2])
  par(mar=c(0,2,2,3),cex=0.9,lwd=0.5)
  tit <- sprintf("LPJmL %s best LAImax",cropnames[band_index[band]],textflag)
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.bestlai[,,band],zlim=c(1,7), xlim=pxlim,
             ylim=pylim, col=col.lai,xlab="",ylab="",
             main=tit,axes=F)
  box(lwd=0.5)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  screen(ind[3])
  par(mar=c(.5,0,1,3),lwd=0.5)
  image.plot(zlim=c(1,7), col=col.lai,horizontal=T,legend.only=T,line=0,
            legend.args=list( text="LAImax",cex=.9, side=2, line=3, adj=0,las=1),
            smallplot=c(0.1,0.9, 0.5,.9),axis.args=list(lwd=0.5,mgp=c(0, .5, 0)))


  screen(ind2[1])
  par(mar=c(0,2,2,3),cex=0.9,lwd=0.5)
  maxyield <- max(map.region.bestyield[,,band],map.fao[,,band])
  tit <- sprintf("LPJmL %s yields, %s %s",cropnames[band_index[band]],eval.years,textflag)
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.bestyield[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit,axes=F)
   #add zero values
  zero<- map.region.bestyield[,,band]
  zero[which(zero!=0)]<-NA
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),zero, add=T,
	     zlim=c(0,maxyield), xlim=pxlim,ylim=pylim, col="grey",xlab="",ylab="")
  box(lwd=0.5)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  screen(ind2[2])
  par(mar=c(0,2,2,3),cex=0.9,lwd=0.5)
  tit <- sprintf("FAO %s yields, %s",cropnames[band_index[band]],eval.years)    
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.fao[,,band],zlim=c(0,maxyield), xlim=pxlim,
             ylim=pylim, col=col.yields,xlab="",ylab="",
             main=tit,axes=F)
   #add zero values
  zero<-map.fao[,,band]
  zero[which(zero!=0)]<-NA
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),zero, add=T,
	     zlim=c(0,maxyield), xlim=pxlim,ylim=pylim, col="grey",xlab="",ylab="")
  
  box(lwd=0.5)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  screen(ind2[3])
  par(mar=c(.5,0,1,3),lwd=0.5,cex=.9)
  image.plot(zlim=c(0,maxyield), col=col.yields,horizontal=T,legend.only=T,line=0,
            legend.args=list( text="tFM/ha", side=2, line=3, adj=0,las=1),
            smallplot=c(0.1,0.9, 0.5,.9),axis.args=list(lwd=0.5,mgp=c(0, .5, 0)))


  close.screen( all=TRUE)
  dev.off()
} # end for band
write.table(best.lai,file=paste(output.path,"copy_me_by_hand_to_manage_laimax_alphaa_",textflag,"_sc.par.txt",sep=""))


zz <- file(paste(output.path,"fao_yields_table",eval.years,"_.csv",sep=""),"w")
write.csv2(t(fao.yields),zz) # countries in rows, cfts in cols
close(zz)