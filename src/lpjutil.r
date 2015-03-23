HEADER_SIZE<-43

read.input.header<-function(filename){
    file.in<-file(filename,"rb")
    
    seek(file.in,7, origin = "start")
    version<-readBin(file.in,integer(),n=1,size=4)
    order<-readBin(file.in,integer(),n=1,size=4)
    firstyear<-readBin(file.in,integer(),n=1,size=4)
    nyears<-readBin(file.in,integer(),n=1,size=4)
    firstcell<-readBin(file.in,integer(),n=1,size=4)   
    ncells<-readBin(file.in,integer(),n=1,size=4)
    nbands<-readBin(file.in,integer(),n=1,size=4)
    cellsize<-readBin(file.in,numeric(),n=1,size=4)
    scalar<-readBin(file.in,numeric(),n=1,size=4)
    header<-data.frame(version,order,firstyear,nyears,firstcell,ncells,nbands,cellsize,scalar)
    close(file.in)
    return(header)
     
}
 
read.input.grid<-function(path.in){
     input.list<-dir(path.in)
     grid.name<-paste(path.in,input.list[grep("grid",input.list)],sep="")
     grid.header<-read.input.header(grid.name)
     
     prec<-abs(log(grid.header$scalar)/log(10))
     gridfile<- file(grid.name,"rb")
     seek(gridfile,HEADER_SIZE, origin = "start")
    grid.temp<-readBin(gridfile,integer(),n=2*grid.header$ncells,size=2)
    grid.data<<-round(grid.temp,digits=0)*grid.header$scalar
    lon<<-grid.data[c(1:grid.header$ncells)*2-1]
    lat<<-grid.data[c(1:grid.header$ncells)*2]
    EAST<<-round(max(lon),prec)
    SOUTH<<-round(min(lat),prec)
    WEST<<-round(min(lon),prec)
    NORTH<<-round(max(lat),prec)
    RES<<-grid.header$cellsize
    NC<<-(NORTH-SOUTH)/RES+1
    NR<<-(EAST-WEST)/RES+1

    
    ind_lon<<-ceiling(lon/RES-min(lon)/RES+1)
    ind_lat<<-ceiling(lat/RES-min(lat)/RES+1)
    
    close(gridfile)
}



read.output.grid<-function(path.out,ncells,scalar,cellsize){
     input.list<-dir(path.out)
     grid.name<-paste(path.out,input.list[grep("grid",input.list)],sep="")
     grid.header<-read.input.header(grid.name)
     
     prec<-abs(log(scalar)/log(10))
     gridfile<- file(grid.name,"rb")
#      seek(gridfile,HEADER_SIZE, origin = "start")
    grid.temp<-readBin(gridfile,integer(),n=2*ncells,size=2)
    grid.data<<-round(grid.temp,digits=0)*scalar
    lon<<-grid.data[c(1:ncells)*2-1]
    lat<<-grid.data[c(1:ncells)*2]
    EAST<<-round(max(lon),prec)
    SOUTH<<-round(min(lat),prec)
    WEST<<-round(min(lon),prec)
    NORTH<<-round(max(lat),prec)
    RES<<-cellsize
    NC<<-(NORTH-SOUTH)/RES+1
    NR<<-(EAST-WEST)/RES+1

    
    ind_lon<<-ceiling(lon/RES-min(lon)/RES+1)
    ind_lat<<-ceiling(lat/RES-min(lat)/RES+1)
    
    close(gridfile)
}

read.input.files<-function(filename,data.size){
    fileHeader<-read.input.header(filename)
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(fileHeader$nbands,fileHeader$nyears,fileHeader$ncells))
    seek(file.in,where=HEADER_SIZE,origin="start")
   for(i in 1:fileHeader$nyears){
        for(j in 1:fileHeader$ncells){
               data.in[,i,j]<-readBin(file.in, integer(), n=fileHeader$nbands, size=data.size)*fileHeader$scalar
        }
     }
    close(file.in)
    return(data.in)
}
	
	
read.input.yearband<-function(filename,data.size,year,band){#year,band, start from 1
	fileHeader<-read.input.header(filename)
data.year<-year-fileHeader$firstyear+1
file.in <- file(sprintf(filename),"rb")
data.in<-array(NA,dim=c(fileHeader$ncells))
seek(file.in,where=HEADER_SIZE+data.size*((data.year-1)*fileHeader$nband*fileHeader$ncells+(band-1)),origin="start")
for(i in 1:fileHeader$ncells){
data.in[i]<-readBin(file.in, integer(), n=1, size=data.size)*fileHeader$scalar
seek(file.in,where=(fileHeader$nbands-1)*2,origin="current")
}
close(file.in)
return(data.in)
}


read.output.harvest.all<-function(filename,ncells,nbands,nyears){
	 harvest.fn.out<-file(paste(filename,sep=""),"rb")
	 harvest.temp<-array(NA,c(ncells,nbands,nyears))#the harvest file data were aligned as [pixels,plant funtional type,years]
	 harvest.temp[,,]<-readBin(harvest.fn.out,double(),ncells*nbands*nyears,size=4)
	 close(harvest.fn.out)
	 #  cat("done\n")
	 return(harvest.temp)
 }

#############
#convert 1 dimentional
#raw data to a map. raw[NPIX]
#############
map.build<-function(raw_){
map<-array(NA, dim=c(NR,NC))
for(i in 1:length(raw_))
map[ind_lon[i],ind_lat[i]]<-raw_[i]
return(map)
}


read.output.harvest<-function(filename, ncells, nbands, startyear, year,data.size=4, band="ALL", par=1){
	nyears<-file.info(filename)$size/ncells/nbands/data.size
	if(nyears/as.integer(nyears)!=1) stop("nyears:",nyears, " error\n")
	if(band[1]=="ALL") band<-c(1:nbands)
	harvest<-list()
	data<-array(NA, c(ncells, length(band), length(year)))

	fn<-file(filename,"rb")
	for(y in 1:length(year)){
		for(b in 1:length(band)){
			seek(fn, where=(year[y]-startyear)*nbands*ncells*data.size+(band[b]-1)*ncells*data.size,origin="start")
			data[,b,y] <- readBin(fn,double(),size=data.size,n=ncells)
		}
	}
	harvest$data<-data
	if(length(par)!=1){
		for(i in 1:length(band)){
			harvest$data[,i,]<-data[,i,]*par[i]
		}
	}else harvest$data<-harvest$data*par
	
	harvest$band<-band
	harvest$year<-year
	close(fn)
	return(harvest)
}
		
# 		
# 		    k<-read.output.harvest(f,5794,52,startyear=1961,
# 					     year=c(2000:2005),  
# 					     par= 1/0.45*0.01*FRESHMATTER[band_index[band]])
# 	


 deg2area<-function(lat,res=0.5){
	 deg2rad <- function(deg){
		 return (deg*pi*0.00555555555555)
	 }
	 area<-(111e3*res)*(111e3*res)*cos(deg2rad(lat))/10000#ha
	 return(area)
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