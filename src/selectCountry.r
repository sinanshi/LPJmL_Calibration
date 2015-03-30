#
#
#
HEADER_SIZE<-43
PROPORTION<-0.9# Threshold, e.g. 0.9 means lower than 90% will be excluded


# read.input.header<-function(filename){
#     file.in<-file(filename,"rb")
#     
#     seek(file.in,7, origin = "start")
#     version<-readBin(file.in,integer(),n=1,size=4)
#     order<-readBin(file.in,integer(),n=1,size=4)
#     firstyear<-readBin(file.in,integer(),n=1,size=4)
#     nyears<-readBin(file.in,integer(),n=1,size=4)
#     firstcell<-readBin(file.in,integer(),n=1,size=4)   
#     ncells<-readBin(file.in,integer(),n=1,size=4)
#     nbands<-readBin(file.in,integer(),n=1,size=4)
#     cellsize<-readBin(file.in,numeric(),n=1,size=4)
#     scalar<-readBin(file.in,numeric(),n=1,size=4)
#     header<-data.frame(version,order,firstyear,nyears,firstcell,ncells,nbands,cellsize,scalar)
#     close(file.in)
#     return(header)
#      
# }
# 
# read.input.files<-function(filename,data.size){
#     fileHeader<-read.input.header(filename)
#     file.in <- file(sprintf(filename),"rb")
#     data.in<-array(NA,dim=c(fileHeader$nbands,fileHeader$nyears,fileHeader$ncells))
#     seek(file.in,where=HEADER_SIZE,origin="start")
#    for(i in 1:fileHeader$nyears){
#         for(j in 1:fileHeader$ncells){
#                data.in[,i,j]<-readBin(file.in, integer(), n=fileHeader$nbands, size=data.size)*fileHeader$scalar
#         }
#      }
#     close(file.in)
#     return(data.in)
# }

selectCountries<-function(cowfile.global,cowfile.local,proportion){
	country.key<-read.table(paste(countryfao.path,"countries_fao2lpjml.txt",sep=""),header=T)#country code and name
	cow.global<-read.input.files(cowfile.global,data.size=2)[1,1,] #read only country band
	cow.local<-read.input.files(cowfile.local,data.size=2)[1,1,]#read only country band
	local.country<-sort(unique(cow.local))
	
	countryName<-character(c(length(local.country)))
	for(i in 1:length(local.country) ){
		countryName[i]<-as.character(country.key[which(country.key$Index_MagPIE==local.country[i])[1],2])
		}
	area.table<-data.frame("country.name"=countryName,"country"=local.country,
			"global.area"=rep(0, length(local.country)),  "local.area"=rep(0, length(local.country)),
			 "proportion"=rep(0, length(local.country))) 
	for(i  in 1:length(local.country)){
		area.table$global.area[i]<-length(which(cow.global==local.country[i]))
		area.table$local.area[i]<-length(which(cow.local==local.country[i]))
		area.table$proportion[i]<-area.table$local.area[i]/area.table$global.area[i]
		}
	if(length(which(area.table$global.area>=area.table$local.area))!=length(local.country)){
		stop("global area is smaller than local area, please check area.table")
		}
	country.selected<-local.country[area.table$local.area/area.table$global.area>=proportion]
	country.excluded<-local.country[area.table$local.area/area.table$global.area<proportion]
	print(area.table)
	cat("countries selected: ", country.selected, "\n")
	cat("countries excluded: ", country.excluded, "\n")
	return(country.selected)
}

