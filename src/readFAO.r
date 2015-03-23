#fao.path<-"~/workspace/lpj_calibration/FAO/"

bandavail<-ncft #how many bands need to convert; for checking purpose.

#output of LPJmL will be arranged in the same order as following
# LPJ_Band<-c("Temperate Cereals","Rice","Maize","Tropical Cereals","Pulses","Temperate Roots",
# 	   "Potatoes","Tropical Roots","Sunflower","Soybeans","Groundnuts","Rapeseed","Sugar Cane",
# 	   "Others","Manage Grasslands","Bioenergy Grass","Bioenergy Tree","Citrus","Non Citrus Orchards",
# 	   "Date Palm","Olives","Nuts Trees","Grapes","Vegetables","Cotton","Fodder grass")
# 
# 
# FAO_Element<-c("Almonds, with shell","Apples","Grapes","Maize","Millet","Olives","Oranges",
# 	       "Potatoes","Rice, paddy","Sugar beet","Sugar cane","Sunflower seed",
# 	       "Wheat","Dates","Soybeans","Groundnuts, with shell","Peas, dry","Rapeseed","Cassava")




# FAO_Item<-c("Wheat", "Rice, paddy", "Maize", "Millet","Peas, dry", "Sugar beet", 
# 		"Potatoes", "Cassava","Sunflower seed","Soybeans", "Groundnuts","Rapeseed", "Sugar cane", NA,NA,NA,NA,"Oranges","Apples","Dates",
# 		"Olives","Almonds, with shell", "Grapes", NA, NA, NA)

#-------------------------------
#To get FAO Item code from the given FAO_Item
#-------------------------------
getFaoItemCode<-function(fao.table,FAO_Item){
	FAO_ItemCode<-array(NA,dim=length(FAO_Item))
	for(i in 1:length(FAO_Item)){
		if(is.na(FAO_Item[i])) next
		temp<-unique(fao.table$ItemCode[which(fao.table$Item==FAO_Item[i])])
		if(length(temp)!=1)       stop("can not find ",FAO_Item[i]," in FAO table.")
		FAO_ItemCode[i]<-temp
		}
	return(FAO_ItemCode)
	}
	
	
#-------------------------------
#create a yield table [crop, fao_country]
#-------------------------------
getFaoYieldTable<-function(fao.table, FAO_ItemCode,FAO_YearChosen){
	index<-(which(is.na(FAO_Item)==FALSE))
	if(bandavail!=length(index))	stop("bandvail is not equaal to the length of index")
	fao_country_code<-unique(fao.table[,"CountryCode"])
	fao_country_num<-length(unique(fao.table[,"CountryCode"])) #how many countries in FAO table
	fao_yields<-array(NA,c(bandavail, fao_country_num))
	for(i in 1:length(index)){
		for(j in 1:length(fao_country_code)){
			#Get the year of FAO for one crop one country
			total.year<-fao.table[(which(fao.table[,"ItemCode"]==FAO_ItemCode[index[1]] & fao.table[,"CountryCode"]==fao_country_code[1])) ,"Year"]
			#Search index of chosen year
			ChosenIndex<-which(is.na(match(total.year,FAO_YearChosen))==FALSE)
			#Mean value of chosen year of one crop and one country
			fao_yields[i,j]<-mean(fao.table[
				(which(fao.table[,"ItemCode"]==FAO_ItemCode[index[i]] & fao.table[,"CountryCode"]==fao_country_code[j])),"Value"][ChosenIndex])
		if(is.na(fao_yields[i,j]))  fao_yields[i,j]<-0 #set all NA value as zero
		}
	}
	return(fao_yields)
}
			
convFaocountry2lpjcountry<-function(country.key, fao.table,fao_yields){
	fao.index<-unique(fao.table[,"CountryCode"])#country code for fao_yields, in same sequence
	
	key.lpj<-country.key$Index_MagPIE
	key.fao<-country.key$Country_Code_FAO
	fao_yields_lpjcountry<-array(NA, c(dim(fao_yields)[1], 197))
	for(i in 1:197){ #LPJ countries are only 197 countries at the top of "countries_fao2lpjml.txt"
		if(key.fao[i]!=-9){
			pos.fao<-which(fao.index==key.fao[i])
			fao_yields_lpjcountry[,i]<-fao_yields[,pos.fao]
		}else{
			fao_yields_lpjcountry[,i]<-rep(0, dim(fao_yields)[1]) # set all 
		}
	}
	return(fao_yields_lpjcountry)
}
#=============
#start main program	
#=============
getYields<-function(FAO_YearChosen){
	fao.table<-read.csv(paste(fao.path,"all.csv",sep=""),sep=";")
	fao.table.years<-unique(fao.table[,"Year"])
	cat("FAO TABLE: \n")
	cat("Years:",min(fao.table.years),"-",max(fao.table.years),"\n",sep="")
	FAO_ItemCode<-getFaoItemCode(fao.table,FAO_Item)
	fao_yields<-getFaoYieldTable(fao.table,FAO_ItemCode,FAO_YearChosen)
	country.key <- read.table(paste(fao.path,"countries_fao2lpjml.txt",sep=""),header=T)
	fao_yields_lpjcountry<-convFaocountry2lpjcountry(country.key, fao.table,fao_yields)
	return(fao_yields_lpjcountry)
}


		


	
	













		





