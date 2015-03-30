
Longitude<<-seq(WEST,EAST,RES)
Latitude<<-seq(SOUTH,NORTH,RES)	

selectcountryname<-vector()
for(i in 1:length(country.selected))
selectcountryname[i]<-as.character(
	country.key$Country_MAgPIE[country.key$Index_MagPIE==(country.selected[i])])[1]



lpj.area<-readCalibOutputArea(dir=lpjoutput.path,
		ncells=ncell,
		nbands=nbands,
		startyear=baseyear,
		year=fao.years,
		RUN)

lpj.area<-lpj.area[BAND_EXAM$lpj,,]
lpj.area<-lpj.area[,country.selected+1,]
lpj.area<-rowMeans(lpj.area,dims=2)



for(b in 1:length(BAND_EXAM$lpj)){
	mv<-array(NA, length(cow))
	mv.fao<-array(NA, length(cow))
	mv.kest<-array(NA, length(cow))
	mv.area<-array(NA, length(cow))
	for(i in 1:length(country.selected)){
		mv[(cow==(country.selected[i]))]<-best_yields[b,i]
		mv.fao[(cow==(country.selected[i]))]<-fao.yields[b,i]
		mv.kest[(cow==(country.selected[i]))]<-k_est[b,i]
 		mv.area[(cow==(country.selected[i]))]<-lpj.area[b,i]
	}
	maxyield<-max(mv,mv.fao,na.rm=T)
	map<-map.build(mv)
	map.fao<-map.build(mv.fao)





filename <- paste(plot.path,"lpjmlcalib_vs_fao_all_",cropnames[BAND_EXAM$lpj[b]],".png",sep="")
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
#     image(x=Longitude,y=Latitude,map,col=col.yields,zlim=c(0,maxyield))
  
  plot(fao.yields[b,],best_yields[b,],main="",xlab="", ylab="",col="green",pch=3,lwd=0.8)
   onetoone<-vector()
   onetoone[1] <- min(best_yields[b,],fao.yields[b,],na.rm=T)
   onetoone[2] <- max(best_yields[b,],fao.yields[b,],na.rm=T)
   tit <- cropnames[BAND_EXAM$lpj[b]]
#   plot(fao.yields[b,],reg.yield,main="",xlab="",
#        ylab="",col="green",pch=3,lwd=0.8)
   title(main=tit,xlab=list("FAO yields [t FM/ha]"),ylab=list("LPJmL yields [t FM/ha]"),line=1.5)
  

  
  
  
  
  
  
  
  circle.radius<-lpj.area[b,]/max(lpj.area[b,])*max(fao.yields[b,],na.rm=T)/5
#    circle.radius <- (region.total.croparea[,band]/max(region.total.croparea[,band])*max(fao.yields[b,])/5)
  circle.radius[is.infinite(circle.radius)] <- 0
  circle.radius[circle.radius<0] <- 0
  symbols(fao.yields[b,],best_yields[b,],circles=circle.radius,add=T,cex=0.5,inches=F)

points(fao.yields[b,best_yields[b,]>fao.yields[b,]],best_yields[b,best_yields[b,]>fao.yields[b,]],
       col="green",pch=3,lwd=0.8)
points(fao.yields[b,best_yields[b,]/fao.yields[b,]>2],best_yields[b,best_yields[b,]/fao.yields[b,]>2],
       col="blue",pch=3,lwd=0.8)
points(fao.yields[b,best_yields[b,]/fao.yields[b,]<0.5],best_yields[b,best_yields[b,]/fao.yields[b,]<0.5],
       col="red",pch=3,lwd=0.8)
points(fao.yields[b,best_yields[b,]==0],best_yields[b,best_yields[b,]==0],
       col="orange",pch=3,lwd=0.8)

#  index.far<-which(abs(best_yields[b,]-fao.yields[b,])>0.4*max(fao.yields[b,],na.rm=T))
index.far<-which(best_yields[b,]/fao.yields[b,]>2 )
index.far<-c(index.far,which(best_yields[b,]/fao.yields[b,]<0.5))
 index.big<-which(circle.radius>=0.5*mean(circle.radius[which(circle.radius!=0)]))
  index.put<-intersect(index.far,index.big)
#   print(index.far)
 # print(index.put)
  if(length(index.put)!=0){
  text(fao.yields[b,index.put],best_yields[b,index.put]-0.1*(max(best_yields[b,],na.rm=T)),
selectcountryname[index.put] , font=1)
  }





   legend("bottomright",legend=c("acceptable","strong overestimation (>200%)","underestimation (<50%)","no yields"),col=c("green","blue","red","orange"),pch=1,pt.lwd=1,pt.cex=1,cex=0.7)
  o<-fao.yields[b,!is.na(fao.yields[b,])]
  p<-best_yields[b,!is.na(fao.yields[b,])]
  o<-o[ p!=0]
  p<-p[p!=0]

    wiltext <- sprintf("Willmott: %g",round(Willmott(o,p),2))
    text(max(o)*0.6/3,max(p),labels=wiltext)
    eftext <- sprintf("EF: %g",round(EF(o,p),2))
    text(max(o)*2/3,max(p),labels=eftext)
  



 lines(onetoone,onetoone,lwd=1)



  screen(ind[2])
  par(mar=c(0,2,2,3),cex=0.9,lwd=0.5)
  image(x=Longitude,y=Latitude,z=map.build(as.numeric(treedens[mv.kest,(b+1)])),
	     col=rainbow(11),ylim=c(19,51),axes=F)
  box()
  #map(add=T)
  title("LPJmL best k_est")
  #image.plot(x=Longitude,y=Latitude,z=map.build(as.numeric(treedens[mv.kest,(b+1)])),
	#     col=rainbow(11),ylim=c(19,51),axes=F,legend.only=T)
  
  screen(ind2[1])
  par(mar=c(0,2,2,3),cex=0.9,lwd=0.5)
  image(x=Longitude,y=Latitude,map,col=col.yields,zlim=c(0,maxyield),ylim=c(19,51),axes=F)
  #map(add=T)
  box()
  title(paste("LPJmL yields",eval.years))
#   #invisible(lapply(country.line,masklines05))
#   #invisible(lapply(land.line,masklines1))
  screen(ind2[2])
  par(mar=c(0,2,2,3),cex=0.9,lwd=0.5)
  image(x=Longitude,y=Latitude,map.fao,col=col.yields,zlim=c(0,maxyield),ylim=c(19,51),axes=F)
  #map(add=T)
  box()
  title(paste("FAO yields",eval.years))
  #image.plot(x=Longitude,y=Latitude,map,col=col.yields,zlim=c(0,maxyield),legend.only=T,axes=F)
 

  close.screen( all=TRUE)
  dev.off()
  }
