 
readCalibOutput<-function(dir, ncells,nbands,startyear, year,RUN){
	cat("reading lpjml outputs...\n")
	lpj.yields<-array(NA, c(nbands/2,197, RUN))
for( r in 1:RUN){
# 	if(NOIRRIG)		band.select<-BAND_RAINF
# 	else band.select<-BAND_ALL
	for( b in  BAND_RAINF){
		pft_harvest_r<-read.output.harvest(
			filename=paste(dir,"pft_harvest",r, ".pft.bin",sep=""),
				   ncells=ncells,
				    nbands=nbands, startyear=startyear,
				    year=year, band=b,
				    par= 1/0.45*0.01*FRESHMATTER[b])
		print("pft_harvest done")
		cft_frac_r<-read.output.harvest(
			filename=paste(dir,"cftfrac",r, ".bin",sep=""),
				 ncells=ncells,
				 nbands=nbands, startyear=startyear,			
				 year=year,band=b)
		print("cft frac done")

		if(!NOIRRIG){
			pft_harvest_i<-read.output.harvest(
				filename=paste(dir,"pft_harvest",r, ".pft.bin",sep=""),
				   ncells=ncells,
				    nbands=nbands, startyear=startyear,
				    year=year, band=b+nbands/2,
				    par= 1/0.45*0.01*FRESHMATTER[b])
			cft_frac_i<-read.output.harvest(
				filename=paste(dir,"cftfrac",r, ".bin",sep=""),
				 ncells=ncells,
				 nbands=nbands, startyear=startyear,			
				 year=year,band=b+nbands/2)
			yields<-rowMeans(
				pft_harvest_r$data*cft_frac_r$data+pft_harvest_i$data*cft_frac_i$data,
				dim=2)*deg2area(lat)			
			croparea<-rowMeans(cft_frac_r$data+cft_frac_i$data,dim=2)*deg2area(lat)	
		}else{
			yields<-rowMeans(pft_harvest$data*cft_frac$data,dim=2)*deg2area(lat)
			croparea<-rowMeans(cft_frac$data,dim=2)*deg2area(lat)
		}
		
		for(c in country.selected){#country.selected starts from 0, cow starts also from 0
			val<-sum(yields[c==cow])/sum(croparea[c==cow])
			if(is.na(val)) 	lpj.yields[b,c+1,r]<-0
			else lpj.yields[b,c+1,r]<-val #index starts from 1

		}
	}
}
	return(lpj.yields)	
}
		


readCalibOutputArea<-function(dir, ncells,nbands,startyear, year,RUN){
	cat("reading lpjml outputs...\n")
	lpj.yields<-array(NA, c(nbands/2,197, RUN))
	for( r in 1:RUN){
		for( b in  BAND_RAINF){
		cft_frac_r<-read.output.harvest(
			filename=paste(dir,"cftfrac",r, ".bin",sep=""),
				 ncells=ncells,
				 nbands=nbands, startyear=startyear,			
				 year=year,band=b)
		
		if(!NOIRRIG){
			cft_frac_i<-read.output.harvest(
				filename=paste(dir,"cftfrac",r, ".bin",sep=""),
				 ncells=ncells,
				 nbands=nbands, startyear=startyear,			
				 year=year,band=b+nbands/2)
			croparea<-rowMeans(cft_frac_r$data+cft_frac_i$data,dim=2)*deg2area(lat)	
		}else{
			croparea<-rowMeans(cft_frac$data,dim=2)*deg2area(lat)
		}
		
		for(c in country.selected){#country.selected starts from 0, cow starts also from 0
			val<-sum(croparea[c==cow])
			if(is.na(val)) 	lpj.yields[b,c+1,r]<-0
			else lpj.yields[b,c+1,r]<-val #index starts from 1
		}
	}
	}
	return(lpj.yields)	

}
				


			
			

		
		
		
		
