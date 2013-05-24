##  Make an STI 
##  Spring 2013
##  Contributors:  Brian Buchanan (bb386), Jo Archibald (jaa78)

## Includes some pre-processing of files, and the function that will make the STI.  Examples at end..


#library(rgdal)
library(RSAGA)
library(raster)

rsaga.env()
rsaga.env(workspace="G:/SaturatedAreasModeling/TownBrook/Spatial")
envmyenv <- rsaga.env(workspace="G:/SaturatedAreasModeling/TownBrook/Spatial")
setwd("G:/SaturatedAreasModeling/TownBrook/Spatial")

##  Read in ASC file and convert it to a sgrd file:
rsaga.esri.to.sgrd(in.grids="fill_dem_wsh.asc", out.sgrds = "DEM2", in.path="G:/SaturatedAreasModeling/TownBrook/Spatial")
rsaga.esri.to.sgrd(in.grids="Trans2.asc", out.sgrds = "TRANS", in.path="G:/SaturatedAreasModeling/TownBrook/Spatial")


# 	This Function will make a filtered and unfiltered STI
#  ***Note, both TRANS and DEM grids must be in sgrd format, and in the StartFolder.  All new files will be placed in a new folder indexed with your start date and time.
# 	Note:  User might want to play with the settings.  
#   Dinf method used for flow accumulation


#	SlopeOpt = "Arc", 2,5,10 (or any number)
#	 Available Flow Directions Options:
	# [0] Deterministic 8
	# [1] Rho 8
	# [2] Braunschweiger Reliefmodell
	# [3] Deterministic Infinity
	# [4] Multiple Flow Direction
	# [5] Multiple Triangular Flow Directon
	
	# "+proj=utm +zone=18 ellps=WGS84"
MakeSTI <- function(StartFolder="G:/SaturatedAreasModeling/TownBrook/Spatial", DEM = "DEM2.sgrd", TRANS = "TRANS.sgrd", DEMfrom = "USGS, 2013", TRANSfrom = "USDA, S&W shared folder", SlopeOpt = 2, FlowDirOpt = 4, user="Josephine Archibald", MapProjection = "+proj=utm +zone=18 +datum=NAD28 +units=meters" ){  # 
	
	setwd(StartFolder) 
	
##  Makes a new folder to hold files
	EndFolder <- paste("Layers", strftime(Sys.time(), format='%Y%m%d%H%M'), sep="")
	Date <- as.Date(Sys.time())
	dir.create(EndFolder)	
	
	# Fill DEM
	rsaga.geoprocessor("ta_preprocessor", module=3, param=list(DEM=DEM, MINSLOPE="0.01", RESULT=paste(EndFolder,"/FILLDEM.sgrd", sep="")))  
	
	
	setwd(EndFolder)
	# Calculate Slope (using Arc option) We are using 2 options here Arc = METHOD 2, DS=2,5,10
	#  Look at options using this function: rsaga.get.usage("ta_morphometry", 0)
	if (SlopeOpt=="Arc") {
		rsaga.geoprocessor("ta_morphometry", module=0, param=list(ELEVATION="FILLDEM.sgrd", SLOPE="SLOPE.sgrd", ASPECT="ASPECT.sgrd",METHOD="2")) #	Calculate Slope
		} else rsaga.geoprocessor("ta_morphometry", module=9, param=list(DEM="FILLDEM.sgrd", DISTANCE=as.character(SlopeOpt), OUTPUT="2", GRADIENT="SLOPE.sgrd"))
	

	#  Calculate Catchment Area (using Dinf option)   Options:  D8(0),FD8, Rho8, MFD, D-inf, MTFD
	    # [0] Deterministic 8
        # [1] Rho 8
        # [2] Braunschweiger Reliefmodell
        # [3] Deterministic Infinity
        # [4] Multiple Flow Direction
        # [5] Multiple Triangular Flow Directon
	rsaga.geoprocessor("ta_hydrology", module=0, param=list(ELEVATION="FILLDEM.sgrd", Method=3, CAREA="AREA.sgrd"))
		
	#  STI
	rsaga.geoprocessor("ta_hydrology", module=20, param=list(SLOPE="SLOPE.sgrd", AREA="AREA.sgrd", CONV="1", METHOD="Standard", TRANS=paste("../", TRANS,sep=""),TWI="STI.sgrd"))
			
	# Filter previous
	rsaga.geoprocessor("grid_filter", module=0, param=list(INPUT="STI.sgrd", RESULT="STI_F.sgrd", MODE="0", METHOD="Smooth", RADIUS=1))
	
	file.create("DataDescription.txt")
	sink("DataDescription.txt", split=TRUE)
	cat(paste("Spatial files created by", user, "on", Date, "\n"))
	cat(paste("DEM from", DEMfrom, ", and transmissivity created from", TRANSfrom, "\n"))
	cat(paste("STI slope option used was", SlopeOpt, "\n"))
	FDlist=c("Deterministic 8", "Rho8", "Braunschweiger Reliefmodell", "Deterministic Infinity", "Multiple Flow Direction", "Multiple Triangular Flow Directon")
	cat(paste("Flow Direction option used was", FDlist[FlowDirOpt+1], "\n"))
	cat(paste("Projection:", MapProjection, "\n"))
	
	sink()
	
}

#

##  Read in ASC file and convert it to a sgrd file:  (NOTE, puts files in wd)
rsaga.esri.to.sgrd(in.grids="dem1.asc", out.sgrds = "DEM", in.path="E:/SaturatedAreasModeling/TownBrook/Spatial/NAD83_SAGA")
rsaga.esri.to.sgrd(in.grids="trans_cl.asc", out.sgrds = "TRANS", in.path="E:/SaturatedAreasModeling/TownBrook/Spatial/NAD83_SAGA")
MakeSTI(StartFolder="E:/SaturatedAreasModeling/TownBrook/Spatial/NAD83_SAGA", DEM = "DEM.sgrd", TRANS = "TRANS.sgrd", )



# setwd("G:/SaturatedAreasModeling/TownBrook/Spatial")
# LocData=read.csv("../../../Steves/StevesProbesNAD27.csv",sep=",", header=TRUE)
 LocData=read.csv("../StevesWells83.csv",sep=",", header=TRUE)

xy <- cbind(LocData$e83, LocData$n83)

	
STIgrid <- read.sgrd("STI.sgrd")
STI_F <- read.sgrd("STI_F.sgrd")

#  
xyz = grid.to.xyz(STIgrid,varname="z")
STIgrid <- rasterFromXYZ(xyz, res=c(NA,NA), crs="+proj=utm +zone=18 +datum=NAD27 +units=meters", digits=5)
raster(STIgrid)
Quants <- as.vector(quantile(xyz$z, na.rm=TRUE, probs = seq(0,1, by=0.1)))
Lims <- as.matrix(data.frame(From=Quants[1:10], To=Quants[2:11], Becomes = seq(10,1,by=-1)))
wet_class <- reclassify(STIgrid, Lims)  ##  depending on what version of raster, I think this function changed names...  reclass --> reclassify

xyz_F = grid.to.xyz(STI_F,varname="z")
STI_F <- rasterFromXYZ(xyz_F, res=c(NA,NA), crs="+proj=utm +zone=18 +datum=NAD27 +units=meters", digits=5)
raster(STI_F)
Quants_F <- as.vector(quantile(xyz_F$z, na.rm=TRUE, probs = seq(0,1, by=0.1)))
Lims_F <- as.matrix(data.frame(From=Quants_F[1:10], To=Quants_F[2:11], Becomes = seq(10,1,by=-1)))
wet_class_F <- reclassify(STI_F, Lims_F)  ## use to be reclass(), function changed??


LocData["STI"] <- extract(STIgrid,xy, method='simple')
LocData["STI_F"] <- extract(STI_F,xy, method='simple')
LocData["Class"] <- extract(wet_class,xy, method='simple')
LocData["Class_F"] <- extract(wet_class_F,xy, method='simple')
summary(LocData)
WellIDs <- read.csv("../../../StevesWells.csv") 

LocData["Rank"] <- WellIDs$Rank


# STI_F <- STIgrid
plot(STIgrid, col=rainbow(10, end=.7), breaks=quantile(STIgrid, probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)))
plot(STIgrid, col=rainbow(10, end=.9))
#points(LocData$Easting, LocData$Northing)
points(LocData$e83, LocData$n83)

plot(STIgrid, col=rainbow(10, end=.7), breaks=round(quantile(STIgrid, probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)), 2), xlim=c(533500,534000), ylim=c(4688500,4689000))

plot(STI_F, col=rainbow(10, end=.7), breaks=round(quantile(STI_F, probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)), 2), xlim=c(533600,534000), ylim=c(4688500,4689000))
points(LocData$e83, LocData$n83, pch=17, col="white")

plot(wet_class, col=rev(rainbow(10, start=0.2, end=.7)), xlim=c(533600,534000), ylim=c(4688500,4689000))
plot(wet_class_F, col=rev(rainbow(10, end=.7)))

head(LocData)


##  Figuring out how to use R SAGA : 
rsaga.get.usage("ta_morphometry", 0)
rsaga.get.usage("ta_morphometry", 9)
"ta_morphometry", module=0







