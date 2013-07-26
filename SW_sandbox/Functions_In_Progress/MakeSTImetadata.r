##  Make an STI 
##  Spring 2013
##  Contributors:  Brian Buchanan (bb386), Jo Archibald (jaa78)

# 	This Function will make a filtered and unfiltered STI
#  ***Note, both TRANS and DEM grids must be in sgrd format, and in the StartFolder.  All new files will be placed in a new folder indexed with your start date and time.
# 	Note:  User might want to play with the settings.  
#   Dinf method used for flow accumulation

#	SlopeOpt = "Arc", "Tarboton", 2,5,10 (or any number)
#	 Available Flow Directions Options [] :
	# [0] Deterministic 8
	# [1] Rho 8
	# [2] Braunschweiger Reliefmodell
	# [3] Deterministic Infinity
	# [4] Multiple Flow Direction
	# [5] Multiple Triangular Flow Directon

require(RSAGA)
require(raster)

##  Read in ASC file and convert it to a sgrd file:
#  e.g. :
# rsaga.esri.to.sgrd(in.grids="fill_dem_wsh.asc", out.sgrds = "DEM2", in.path="G:/SaturatedAreasModeling/TownBrook/Spatial")
# rsaga.esri.to.sgrd(in.grids="Trans2.asc", out.sgrds = "TRANS", in.path="G:/SaturatedAreasModeling/TownBrook/Spatial")



MakeSTI <- function(StartFolder="G:/SaturatedAreasModeling/TownBrook/Spatial", DEM = "DEM2.sgrd", TRANS = "TRANS.sgrd", DEMfrom = "USGS, 2013", TRANSfrom = "USDA, S&W shared folder", SlopeOpt = 2, FlowDirOpt = 3, user="Josephine Archibald", MapProjection = "+proj=utm +zone=18 +datum=NAD28 +units=meters" ){  ## "+proj=utm +zone=18 ellps=WGS84"
	
	setwd(StartFolder) 
	
##  Makes a new folder to hold files
	EndFolder <- paste("Layers", strftime(Sys.time(), format='%Y%m%d%H%M'), sep="")
	Date <- as.Date(Sys.time())
	dir.create(EndFolder)	
	
	# Fill DEM
	rsaga.geoprocessor("ta_preprocessor", module=3, param=list(DEM=DEM, MINSLOPE="0.01", RESULT=paste(EndFolder,"/FILLDEM.sgrd", sep="")))  
	
	setwd(EndFolder)
	# Calculate Slope (using Arc option) We are using 3 options here Arc = METHOD 2 or Tarboton ( Maximum triangular slope) = METHOD 1, DS=2,5,10
	#  Look at options using this function: rsaga.get.usage("ta_morphometry", 0)
	if (SlopeOpt=="Arc") {
		rsaga.geoprocessor("ta_morphometry", module=0, param=list(ELEVATION="FILLDEM.sgrd", SLOPE="SLOPE.sgrd", ASPECT="ASPECT.sgrd",METHOD="2")) #	Calculate Slope
		} else if (SlopeOpt=="Tarboton"){
		rsaga.geoprocessor("ta_morphometry", module=0, param=list(ELEVATION="FILLDEM.sgrd", SLOPE="SLOPE.sgrd", ASPECT="ASPECT.sgrd",METHOD="1"))
		}else rsaga.geoprocessor("ta_morphometry", module=9, param=list(DEM="FILLDEM.sgrd", DISTANCE=as.character(SlopeOpt), OUTPUT="2", GRADIENT="SLOPE.sgrd"))
	
	#  Calculate Catchment Area   Options:  D8(0),FD8, Rho8, MFD, D-inf, MTFD
	rsaga.geoprocessor("ta_hydrology", module=0, param=list(ELEVATION="FILLDEM.sgrd", Method=FlowDirOpt, CAREA="AREA.sgrd"))
		
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
	print(paste("flow option used was", FDlist[FlowDirOpt+1]))
	print(paste("slope calculation option was", SlopeOpt, "(number corresponds to downslope calculation)"))
}


print("Options for FlowDirOpt:")
cat("		[0] Deterministic 8 
        # [1] Rho 8 
        # [2] Braunschweiger Reliefmodell  
        # [3] Deterministic Infinity 
        # [4] Multiple Flow Direction 
        # [5] Multiple Triangular Flow Directon \n" )

print("Options for SlopeOpt:")
cat(	"Tarboton\nArc\nA number corresponding to distance in meters for downslope calculation\n") 




