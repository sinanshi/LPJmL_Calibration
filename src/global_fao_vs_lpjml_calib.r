# deletes all variables
 rm(list=ls(all=TRUE))

# load required libaries
require(fields)

# assignments
ncell <- 67420 # no of cells recorded in file
ncell.in <- 67420
res <- .5 # resolution in degrees
baseyear <- 1901 # first year recorded in file
startyear <- 101#99 #96 #first year to evaluate
endyear <- 103 #100 #last year to evaluate
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

format <- 4

NOIRRIG <- F
PLOTALL <- F

# <- c("wheat","rice","maize","millet","fpea","sugarbeet",
#               "cassava","sunflower","soybeans","groundnuts","rapeseed","sugarcane",
#               "others","grasses")

cropnames <-c("Temperate Cereals","Rice","Maize","Tropical Cereals","Pulses","Temperate Roots",
	   "Potatoes","Tropical Roots","Sunflower","Soybeans","Groundnuts","Rapeseed","Sugar Cane",
	   "Others","Manage Grasslands","Bioenergy Grass","Bioenergy Tree","Citrus","Non Citrus Orchards",
	   "Date Palm","Olives","Nuts Trees","Grapes","Vegetables","Cotton","Fodder grass")

# FAO_Item<-c("Wheat", "Rice, paddy", "Maize", "Millet","Peas, dry", "Sugar beet", 
# 		"Potatoes", "Cassava","Sunflower seed","Soybeans","Groundnuts","Rapeseed", "Sugar cane", NA,NA,NA,NA,
# 		"Oranges","Apples","Dates","Olives","Almonds, with shell", "Grapes", NA, NA, NA)


FAO_Item<-c("Wheat", "Rice, paddy", "Maize", "Millet","Peas, dry", "Sugar beet", 
 		"Potatoes", "Cassava","Sunflower seed","Soybeans", "Groundnuts, with shell","Rapeseed", "Sugar cane", 
 		NA,NA,NA,NA,
 		"Oranges","Apples","Dates","Olives","Almonds, with shell", "Grapes", 
 		#NA,NA,NA,NA,NA,NA,
 		NA, NA, NA)
cat("List of Crop names in LPJ and FAO\n=======\n")
cat(paste(cropnames,"->",FAO_Item,"\n"))


band_index<-(which(is.na(FAO_Item)==FALSE))




# cropnames.fao <- c("wheat","rice","maize","millet","lentils","sugarbeet",
#               "cassava","sunflower","soybeans","groundnuts","rapeseed","sugarcane")
# bandnames <- c("wheat_rf","rice_rf","maize_rf","millet_rf","lentils_rf","sugarbeet_rf",
#               "cassava_rf","sunflower_rf","soybeans_rf","groundnuts_rf","rapeseed_rf","sugarcane_rf",
#               "others_rf","grasses_rf","bmtree_rf","bmgrass_rf","wheat_ir","rice_ir","maize_ir","millet_ir",
#               "lentils_ir","sugarbeet_ir","cassava_ir","sunflower_ir","soybeans_ir",
#               "groundnuts_ir","rapeseed_ir","sugarcane_ir","others_ir","grasses_ir","bmtree_ir","bmgrass_ir")
# FRESHMATTER <- 100 / c(88, 87, 88, 88, 90, 24, 35, 93, 91, 94, 92, 27, 100, 100)  # cassava + sugar cane
# from Wirsenius (2000) PhD-Thesis, dummy values for grass & others

FRESHMATTER <-  100 / c(88, 87, 88, 88, 90, 24,20, 35, 93, 91, 94, 92, 27, 100, 100 ,100 ,100,13,16,70,30,90,10,10,91,100) 


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

# set script working path
# script.path <- "/home/cmueller/christoph/tools/R_scripts/"

# # read LPJ binary land mask file and generate contour line
# zz <- file(paste(script.path,"lpjLandis0_mask.img",sep=""),"rb")
# x <- readBin(zz,integer(),n=720*360,size=1)
# close(zz)
# lpjMask <- array(0,dim=c(720,360))
# dummy <- array(x,dim=c(720,360))
# for(i in c(1:360)) lpjMask[,i]=dummy[,360-i+1]
# land.line <- contourLines(x=seq(-179.75,179.75,len=720),y=seq(-89.75,89.75,len=360),lpjMask,nlevels=0.2)
# rm(dummy,lpjMask)
# 
# # country line
# zz <- file(paste(script.path,"countryMask.img",sep=""),"rb")
# x <- readBin(zz,integer(),n=720*360,size=1)
# close(zz)
# line <- array(0,dim=c(720,360))
# dummy <- array(x,dim=c(720,360))
# for(i in c(1:360)) line[,i]=dummy[,360-i+1]
# country.line <- contourLines(x=seq(-179.75,179.75,len=720),y=seq(-89.75,89.75,len=360),line,nlevels=0.2)
# rm(dummy,x)

# read country codes
lpjinput.path <- "/home/sinan/workspace/lpj_calibration/lpjinputs/"
lpjoutput.path <- "/home/sinan/workspace/lpj_calibration/lpjoutputs/"

textflag <- "revision_1200"
fao.path<-"~/workspace/lpj_calibration/FAO/"


map.region.nr <- array(-9999,dim=c(ncols,nrows))
 region.nr <- array(0,dim=ncell)
source(paste(fao.path,"readFAO.r",sep=""))

# fao.yields <- array(0,dim=c(ncft,nreg))

# # read in FAO to LPJmL country code key
 country.key <- read.table(paste(fao.path,"countries_fao2lpjml.txt",sep=""),header=T)
# fao2lpjml.country <- array(-9,n.country.fao)
 country.lpjml2wr <- array(-9,n.country.lpjml)
# 
country.lpjml2wr <- country.key$Region_Code_MagPIE[1:197]
# 
# for(i in 1:n.country.fao){
#   for(j in 1:n.country.lpjml){
#     if(i==country.key$Country_Code_FAO[j]){
#       fao2lpjml.country[i] <- j
#       break()
#     }
#   }
# }
# # adding additional country codes for Be,Lux,Serbia-Montenergo
# fao2lpjml.country[186] <- 110 #Montenegro
# fao2lpjml.country[255] <- 12 #Belgium
# 
# # reading FAO data
# for(i in fao.years){
#   fao.data <- read.csv2(paste(fao.path,"fao",i,"_yields_in_hgha.csv",sep=""),header=T)
#   fao.data[is.na(fao.data)] <- 0
#   for(k in 1:length(fao.data$country.codes)){
#     if(fao.data$country.codes[k]>0){
#       for(r in 1:n.country.lpjml){
#         if(r==fao2lpjml.country[fao.data$country.codes[k]]){
#           if(r==89) cat("lpjml country",r,"fao.country",fao.data$country.codes[k],"row",k,"wheat",fao.data$Wheat[k],"\n")
#           fao.yields[1,r] <- fao.yields[1,r] + fao.data$Wheat[k]
#           fao.yields[2,r] <- fao.yields[2,r] + fao.data$Rice..paddy[k]
#           fao.yields[3,r] <- fao.yields[3,r] + fao.data$Maize[k]
#           fao.yields[4,r] <- fao.yields[4,r] + fao.data$Millet[k]      
#           fao.yields[5,r] <- fao.yields[5,r] + fao.data$Peas..dry[k]
#           fao.yields[6,r] <- fao.yields[6,r] + fao.data$Sugar.beet[k]
#           #fao.yields[7,r] <- fao.yields[7,r] + fao.data$Sweet.potatoes[k]
#           fao.yields[7,r] <- fao.yields[7,r] + fao.data$Cassava[k]
#           fao.yields[8,r] <- fao.yields[8,r] + fao.data$Sunflower.seed[k]
#           fao.yields[9,r] <- fao.yields[9,r] + fao.data$Soybeans[k]
#           fao.yields[10,r] <- fao.yields[10,r] + fao.data$Groundnuts..with.shell[k]
#           fao.yields[11,r] <- fao.yields[11,r] + fao.data$Rapeseed[k]
#           fao.yields[12,r] <- fao.yields[12,r] + fao.data$Sugar.cane[k]
#         }
#       }
#     }
#   }
# }
# fao.yields <- fao.yields/length(fao.years)/10000 # averaging and converstion to t/ha

fao.yields<-getYields()/10000


best.lai <- array(0,dim=c(nreg,ncft))  #To store the results of best  LAI
all.yield <- array(0,dim=c(nreg,ncft,7))
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
seek(file.grid.input,where=43,start="origin") #35
seek(file.country.input,where=43,start="origin") #39
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
cat(lon.in[26415],lat.in[26415],"\n")
for(i in 1:ncell){
  lon.out[i] <- readBin(file.grid.output,integer(),size=2,n=1)/100
  lat.out[i] <- readBin(file.grid.output,integer(),size=2,n=1)/100
  if(lon.out[i] == lon.in[26415] && lat.out[i]==lat.in[26415]){
    cat("Beauce output pix at ",i,"(",lon.out[i],lat.out[i],")\n")
  }
  ilon.out[i] <- as.integer((lon.out[i]+180)/res + 1.01)
  ilat.out[i] <- as.integer((lat.out[i]+90)/res + 1.01)
  grid.area[i] <-  (111e3*res)*(111e3*res)*cos(deg2rad(lat.out[i]))/10000 #ha
}
close(file.grid.input)
close(file.grid.output)
close(file.country.input)

#loop through LAI values
for(lai in 1:2){
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


file.yield.lpj <- file(paste(lpjoutput.path,"pft_harvest.LAI",lai,".pft.bin",sep=""),"rb")
file.cropfrac <- file(paste(lpjoutput.path,"cftfrac.LAI",lai,".bin",sep=""),"rb")
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
    pix.test[year] <- buf[21205]/0.45*0.01*FRESHMATTER[band_index[band]] #corresponds to Beauce 26415
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

  region.average[] <- region.average[]/region.total.croparea[]
  region.average[is.nan(region.average)] <- 0
  all.yield[,band,lai] <- region.average[,band]
  cat("region 89 Kyrgisistan",region.average[89,band],"for",cropnames[band_index[band]],"\n")
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
  col.yields[1] <- "grey"
  image.plot(map.region.nr,zlim=c(0,max(map.region.nr)),col=rep(topo.colors(50),4))#col=color1(200))

  onetoone <- array(0,2)
  onetoone[1] <- min(region.average[,band],region.fao[,band])
  onetoone[2] <- max(region.average[,band],region.fao[,band])
#   print("end process1")

if(PLOTALL){  
  filename <- sprintf("fao_vs_lpj_%s_yields_1996-2000_%s.ong",cropnames[band_index[band]],textflag2)
  png(paste(lpjoutput.path,filename,sep=""),height=3.4*300,width=5*300,res=300,pointsize=8,type="cairo")
  tit <- sprintf("%s yields FAO vs. LPJmL, 1996-2000 %s",cropnames[band_index[band]],textflag2)
  plot(region.fao[,band],region.average[,band],main=tit,xlab="FAO yields [t FM/ha]",
       ylab="LPJmL yields [t FM/ha]",col="green",pch=3,lwd=0.8)
  circle.radius <- (region.total.croparea[,band]/max(region.total.croparea[,band])*max(region.fao[,band])/5)
  circle.radius[is.infinite(circle.radius)] <- 0
  symbols(region.fao[,band],region.average[,band],circles=circle.radius,add=T,cex=0.5,inches=F)
  points(region.fao[region.average[,band]>region.fao[,band],band],
         region.average[region.average[,band]>region.fao[,band],band],col="green",pch=3,lwd=0.8)
  points(region.fao[region.average[,band]/region.fao[,band]>2,band],
         region.average[region.average[,band]/region.fao[,band]>2,band],col="blue",pch=3,lwd=0.8)
  points(region.fao[region.average[,band]/region.fao[,band]<0.5,band],
         region.average[region.average[,band]/region.fao[,band]<0.5,band],col="red",pch=3,lwd=0.8)
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
  filename <- sprintf("fao_%s_yields_1996-2000_%s.png",cropnames[band_index[band]],textflag2)
  png(paste(lpjoutput.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("%s yields according to FAO, 1996-2000 %s",cropnames[band_index[band]],textflag2)    
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.fao[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-90,90), col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting rainfed yield map (pixel)
  maxyield <- max(map.rainfed[,,band])
  filename <- sprintf("lpjml_rainfed_%s_yields_1996-2000_%s.png",cropnames[band_index[band]],textflag2)
  png(paste(lpjoutput.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("%s yields (rainfed) according to LPJmL, 1996-2000 %s",cropnames[band_index[band]],textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.rainfed[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-90,90), col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting rainfed yield map (country)
  maxyield <- max(map.region.rainfed[,,band])
  filename <- sprintf("lpjml_rainfed_regional_%s_yields_1996-2000_%s.png",cropnames[band_index[band]],textflag2)
  png(paste(lpjoutput.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("Regional %s yields (rainfed) according to LPJmL, 1996-2000 %s",cropnames[band_index[band]],textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.rainfed[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-90,90), col=col.yields,xlab="",ylab="",
             main=tit)
#   #invisible(lapply(country.line,masklines05))
#   #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting average yield map (country)
  maxyield <- max(map.region.average[,,band],map.fao[,,band])
  filename <- sprintf("lpjml_average_regional_%s_yields_1996-2000_%s.png",cropnames[band_index[band]],textflag2)
  png(paste(lpjoutput.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("Regional %s yields according to LPJmL, 1996-2000 %s",cropnames[band_index[band]],textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.average[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-90,90), col=col.yields,xlab="",ylab="",
             main=tit)
#   #invisible(lapply(country.line,masklines05))
#   #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting irrigated yield map (pixel)
  maxyield <- max(map.irrigated[,,band])
  filename <- sprintf("lpjml_irrigated_%s_yields_1996-2000_%s.png",cropnames[band_index[band]],textflag2)
  png(paste(lpjoutput.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("%s yields (irrigated) according to LPJmL, 1996-2000 %s",cropnames[band_index[band]],textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.irrigated[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-90,90), col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  dev.off()

  #plotting irrigated yield map (country)
  maxyield <- max(map.region.irrigated[,,band])
  filename <- sprintf("lpjml_irrigated_regional_%s_yields_1996-2000_%s.png",cropnames[band_index[band]],textflag2)
  png(paste(lpjoutput.path,filename,sep=""),height=3.4*200,width=5*200,res=200,pointsize=8,type="cairo")
  tit <- sprintf("Regional %s yields (irrigated) according to LPJmL, 1996-2000 %s",cropnames[band_index[band]],textflag2)
  image.plot(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.irrigated[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-90,90), col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  dev.off()
  
} # end if(PLOTALL)
  
  #overall picture (one2one_line, LPJmL yield map, FAO yield map)
  filename <- (paste(lpjoutput.path,"lpjml_vs_fao_all_",cropnames[band_index[band]],"_",textflag2,"d.png",sep=""))
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
  tit <- sprintf("%s yields FAO vs. LPJmL, 1996-2000 %s",cropnames[band_index[band]],textflag2)
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
  tit <- sprintf("LPJmL %s yields, 1996-2000 %s",cropnames[band_index[band]],textflag2)
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.average[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-90,90), col=col.yields,xlab="",ylab="",
             main=tit)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  screen(ind2[2])
  par(mar=c(0,2,2,3))
  tit <- sprintf("FAO %s yields, 1996-2000",cropnames[band_index[band]])    
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.fao[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-90,90), col=col.yields,xlab="",ylab="",
             main=tit)
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


col.lai <- color1(7)
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
  filename <- (paste(lpjoutput.path,"lpjmlcalib_vs_fao_all_",cropnames[band_index[band]],"d.png",sep=""))
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
  tit <- sprintf("%s yields, 1996-2000 %s",cropnames[band_index[band]],textflag)
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
    wiltext <- sprintf("Willmott: %g",round(Willmott(fao1[tt>0],lpj1[tt>0]),2))
    text(max(region.fao[,band])*0.6/3,max(reg.yield),labels=wiltext)
    eftext <- sprintf("EF: %g",round(EF(fao1[tt>0],lpj1[tt>0]),2))
    text(max(region.fao[,band])*2/3,max(reg.yield),labels=eftext)
  }
  screen(ind[2])
  par(mar=c(0,2,2,3),cex=0.9,lwd=0.5)
  tit <- sprintf("LPJmL %s best LAImax",cropnames[band_index[band]],textflag)
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.bestlai[,,band],zlim=c(1,7), xlim=c(-180,180),
             ylim=c(-60,80), col=col.lai,xlab="",ylab="",
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
  tit <- sprintf("LPJmL %s yields, 1996-2000 %s",cropnames[band_index[band]],textflag)
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.region.bestyield[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-60,80), col=col.yields,xlab="",ylab="",
             main=tit,axes=F)
  box(lwd=0.5)
  #invisible(lapply(country.line,masklines05))
  #invisible(lapply(land.line,masklines1))
  screen(ind2[2])
  par(mar=c(0,2,2,3),cex=0.9,lwd=0.5)
  tit <- sprintf("FAO %s yields, 1996-2000",cropnames[band_index[band]])    
  image(x=seq(-179.75,179.75,len=360/res), y=seq(-89.75,89.75,len=180/res),
             map.fao[,,band],zlim=c(0,maxyield), xlim=c(-180,180),
             ylim=c(-60,80), col=col.yields,xlab="",ylab="",
             main=tit,axes=F)
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
write.table(best.lai,file=paste(lpjoutput.path,"copy_me_by_hand_to_manage_laimax_alphaa_",textflag,"_sc.par.txt",sep=""))
