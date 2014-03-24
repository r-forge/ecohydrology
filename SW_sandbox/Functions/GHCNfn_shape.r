##  Grab GHCN data centered on a particular location.

##  Arguments: 
#	lat (degrees Lat)
#	long (degrees Lon)
#	NSdeg (distance (in deg) N-S for search box)
#	EWdeg (distance (in deg) E-W for search box)
#	StartYear
#	EndYear
#	StnOpt:  Default, "Any" will return all stations within the search window that have at least a P or T dataset.  
#		"Full" returns only stations that have all three values
# 	Note:  Try to work inside a working directory that already contains the files "ghcndailytmp.dly" and "ghcnd-stations.txt"
	#	created previously so you don't need to re-download the inventory every time you run the function
#		inside your current working directory to run this function.
##  Returns :  List object with data frames containing date, PRCP (tenths of mm), TMAX (tenths of C), TMIN (tenths of C), 
					#	and Lat/Lon (decimal degrees) and Elev (m)
					
#  SPATIAL INFORMATION:
#	useShape:  defaults to FALSE - in that case uses the search box defined by NSdeg, EWdeg
# boundaryFold = character:  the folder containing the shapefile 
# boundary = character: shapefile name w/o extension (i.e. don't include ".shp") 
# projection =  proj4string of input shapefile (needed if not defined in the layer)
# TransformLatLon = default FALSE - change to true if the shapefile needs to be projected into lat/lon
# LLproj : the proj4string for the lat/lon coordinates, not needed if the shapefile doesn't need to be transformed (default is for the NE USA?)
					
					

getGHCN_shape <- function(lat=NULL, long=NULL, NSdeg=0.5, EWdeg=0.5, StartYear= 2000, EndYear=(as.POSIXlt(Sys.time())$year+1900), StnOpt = "Any", useShape=FALSE, boundaryFold=getwd(), boundary=NULL, projection=NULL, TransformLatLon = FALSE, LLproj = "+proj=longlat +ellps=WGS84 +datum=WGS84" ){
	
	require(SWATmodel)
	require(GhcnDaily)
	require(plyr)
	require(rgdal)
	require(maptools)
	
	read_ghcn_raw=function(GHCNfilename,startyear=StartYear,elements=c("TMIN","TMAX","PRCP")){
		 require(Hmisc)
		 suppressWarnings(rm(list=c("alldata")))
		 suppressWarnings(rm(list=objects(pattern="allghcndata")))
		 for (fileline in readLines(GHCNfilename)){
		  stn=substr(fileline,1,11)
		  year=substr(fileline,12,15)
		  mo=substr(fileline,16,17)
		  yearmo=paste(year,mo,sep="")
		  element=as.character(substr(fileline,18,21))
		  if(year<startyear | (length(grep(element,elements))<1)){next()}
		  dy=1:monthDays(paste(year,mo,"01",sep="/"))
		  date=as.Date(format="%Y%m/%d",paste(yearmo,"/",dy,sep=""))
		  tempdata=data.frame(date,var=as.numeric(as.character(substring(fileline,c(seq(22,((length(dy)-1)*8+22),8)),c(seq(26,((length(dy)-1)*8+26),8))))))
		  names(tempdata)=c("date",element)
		  if(!(exists(paste("allghcndata",element,sep="")))){
			assign(paste("allghcndata",element,sep=""),tempdata)
		  } else {
			assign(paste("allghcndata",element,sep=""),rbind(get(paste("allghcndata",element,sep="")),tempdata))
		  }
		 }
		 for (dfnames in objects(pattern="allghcndata")){
		   print(dfnames)
		   if(exists("alldata")){alldata=merge(alldata,get(dfnames),by="date",all=T)}
		else {alldata=get(dfnames)}
		 }
		 return(alldata)
	}
	
	
	if (!("ghcndailytmp.dly" %in% list.files()) ) downloadDailyInventory()  # only download if needed
	junk=readDailyInventory()

	##  OPTIONAL: USING A SHAPEFILE ----------------------------------------------------
	if (useShape) { ##  
		if (is.null(projection)){	
			SHP <- readOGR(boundaryFold, boundary)
		} else SHP <- readShapePoly(paste(boundaryFold, boundary, sep="/"), proj4string=CRS(projection)) 
		if (TransformLatLon) SHP <- spTransform(SHP, CRS(LLproj))  #  Transform the shapefile into lat/lon if it isn't already
		ALLgages <- junk
		coordinates(ALLgages) <- ALLgages[,3:2]
		attributes(ALLgages)$proj4string <- attributes(SHP)$proj4string 
		ALLgages$wshd <- over(ALLgages, SHP)[,1]   # This will be NA for gages outside basin
		Sub1 <- ALLgages[-which(is.na(ALLgages$wshd)),]  #  Removes gages outside watershed
		SubJunk <- subset(Sub1, LastYear> StartYear  & FirstYear < EndYear & 
		(Element =="TMAX" | Element=="TMIN" | Element=="PRCP"))  # Make sure it has data within our time of interest and have P or T
	# ---------------------------------------------------------------------------
	} else 	SubJunk <- subset(junk, 
	Lat > (lat-NSdeg) & Lat < (lat+NSdeg) & 
	Lon > (long-EWdeg) & Lon < (long+EWdeg)& 
	LastYear> StartYear  & FirstYear < EndYear & 
	(Element =="TMAX" | Element=="TMIN" | Element=="PRCP"))
	
	if (StnOpt == "Full"){   #  Only returns stations that have max/min temp and precip
	GHCNstns <- subset(count(SubJunk[1]) ,freq==3)
	} else GHCNstns <- SubJunk  #  Otherwise returns all stations with any information
	
	if (nrow(GHCNstns) < 1){ 
		print("no GHCN stations within your search window, you could try making NSdeg or EWdeg larger to expand it")
	} else {
	
	GHCN <-list()

	if (!("ghcnd-stations.txt" %in% list.files()) ) download.file(GHCN.DAILY.METADATA.URL, destfile="ghcnd-stations.txt")
	ghcnSta = readLines("ghcnd-stations.txt") 
		
	for (b in GHCNstns$Id){
		download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/", b, ".dly", sep=""), "ghcndailytmp.dly")
		GHCN[[b]] <- read_ghcn_raw("ghcndailytmp.dly")
	}	
	Stns <- names(GHCN)    
	Lats <- SubJunk$Lat[sapply(Stns, function(x) min(grep(x, SubJunk$Id)))]
	Lons <- SubJunk$Lon[sapply(Stns, function(x) min(grep(x, SubJunk$Id)))]
	subst =   ghcnSta[sapply(Stns, function(n) grep(n, ghcnSta))] 
	ghcnElev = 	as.numeric(sapply(strsplit(subst, " +"), function(s) s[4]))
	sy <- SubJunk$FirstYear[sapply(Stns, function(x) max(grep(x, SubJunk$Id)))]  ##  Grabbing first year of PRCP (always listed after TMAX and TMIN)
	ly <- SubJunk$LastYear[sapply(Stns, function(x) max(grep(x, SubJunk$Id)))]  ##  Grabbing last year of PRCP 
	pnas <- as.vector(sapply(GHCN, function(x) length(which(x$PRCP== -9999))/nrow(x)))
	StationInfo = data.frame(ID=Stns, Lat=Lats, Lon=Lons, Elev=ghcnElev,Start_PRCP=sy, End_PRCP=ly, PercNAs_PRCP=pnas)
	
	#GHCN[[b]]$Lat <- SubJunk$Lat[min(which(SubJunk$Id == b))]    ##  Add a column reporting decimal latitude
		#GHCN[[b]]$Lon <- SubJunk$Lon[min(which(SubJunk$Id == b))]	 ##  Add a column reporting decimal longitude

		#GHCN[[b]]$Elev <- as.numeric(ghcnElev)          ##  Add a column reporting decimal longitude

	GHCN[['StationInfo']] = StationInfo
	return(GHCN)	
	}

}



