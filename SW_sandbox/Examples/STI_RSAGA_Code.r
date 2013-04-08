##  Developed by Brian Buchanan (bb386@cornell.edu)
#   Read in DEM, process, make slope, aspect, create TI/STI


#library(rgdal)
library(RSAGA)
library(raster)

rsaga.env()
rsaga.env(workspace="G:/SaturatedAreasModeling/TownBrook/Spatial")
envmyenv <- rsaga.env(workspace="G:/SaturatedAreasModeling/TownBrook/Spatial")
setwd("G:/SaturatedAreasModeling/TownBrook/Spatial")

##  Read in ASC file and convert it to a :
rsaga.esri.to.sgrd(in.grids="dem_fill.asc", out.sgrds = "dem_fill", in.path="G:/SaturatedAreasModeling/TownBrook/Spatial")
?rsaga.esri.to.sgrd


####	Cornell Area
###		Read in 3m DEM
rsaga.esri.to.sgrd(in.grids="cor_dem_3m.asc", out.sgrds = "cor_dem_L_3m", in.path="C:/Thresholds/GIS_Data/DEMs/LIDAR_DEMs/Cornell")
###		Read in 10m LIDAR DEM
rsaga.esri.to.sgrd(in.grids="cor_dem_10mt.asc", out.sgrds = "cor_dem_L_10m", in.path="C:/Thresholds/GIS_Data/DEMs/LIDAR_DEMs/Cornell")
###		Read in 10m Transmissivity Layer
rsaga.esri.to.sgrd(in.grids="cor_tran_10m_cl.asc", out.sgrds = "cor_tran_10m", in.path="C:/Thresholds/GIS_Data/Soils/Cornell")
###     Read 10m USGS for Cornell Area
rsaga.esri.to.sgrd(in.grids="cor_udem_10m.asc", out.sgrds = "cor_dem_U_10m", in.path="C:/Thresholds/GIS_Data/DEMs/USGS/Cornell/n43w077")
####	Rusty's Farm
###		Read in 3m DEM
rsaga.esri.to.sgrd(in.grids="rust_dem_3ms.asc", out.sgrds = "rust_dem_L_3m", in.path="C:/Thresholds/GIS_Data/DEMs/LIDAR_DEMs/Rusty")
###		Read in 10m LIDAR DEM
rsaga.esri.to.sgrd(in.grids="rust_dem_10ms.asc", out.sgrds = "rust_dem_L_10m", in.path="C:/Thresholds/GIS_Data/DEMs/LIDAR_DEMs/Rusty")
###		Read in 3m Transmissivity Layer
rsaga.esri.to.sgrd(in.grids="cor_tran_10m_cl.asc", out.sgrds = "rust_tran_10m", in.path="C:/Thresholds/GIS_Data/DEMs/LIDAR_DEMs/Rusty")
###		Read in 10m Transmissivity Layer
rsaga.esri.to.sgrd(in.grids="cor_tran_10m_cl.asc", out.sgrds = "rust_tran_10m", in.path="C:/Thresholds/GIS_Data/DEMs/LIDAR_DEMs/Rusty")
###     Read 10m USGS for Cornell Area
rsaga.esri.to.sgrd(in.grids="cor_udem_10m.asc", out.sgrds = "rust_dem_U_10m", in.path="C:/Thresholds/GIS_Data/DEMs/LIDAR_DEMs/Rusty")

#####  Looping Items
# Site = Location:					Cornell (cor), Harford (Har), etc...
# Type = Calculation Type:			DEM (dem), Filled (fill), Slope (slp), Flow Accum (area), Transmissivity (trans), TI (TI), STI (STI)
# i1 = #DEM Vertical Resolution: 	LiDAR (L), USGS (U)
# i2 = #DEM Cell Size: 				3 (3m), 5 (5m), 10 (10m)
# i3 = #Slope Calculation: 			Arc (0), DwnSlp (2,5,10)
# i4 = #Flow Directions: 			D8, FD8, Rho8, MFD, D-inf, MTFD
# i5 = #TI Formulations: 			Beven (TI), STI (STI), SIDE (SIDE)
#Filtered: 					Filtered (F), Unfiltered ( )

#####	 Nameing Convention
#Site_Type_VerticalRes_CellSize_TI Formulation_Flow Direction_Slope
#Example: cor_dem_L_3m_Arc_D8_TI_F
#Example: cor_TI_L_3m_Arc_D8_F

#DEM Vertical Res
v <- "U"
v <- "L"
#cell size
c <- 3
c <- 10

##	Fill DEM
rsaga.geoprocessor("ta_preprocessor", module=3, param=list(DEM=paste("cor_dem_",v, "_", c, "m", ".sgrd", sep=""), MINSLOPE="0.01", RESULT=paste("cor_fill_",v,"_",c,"m",".sgrd", sep="")))
##	Calculate Slope
rsaga.geoprocessor("ta_morphometry", module=0, param=list(ELEVATION=paste("cor_fill_",v,"_",c,"m",".sgrd", sep=""), SLOPE=paste("cor_slp_",v,"_", c, "m", ".sgrd", sep=""), ASPECT=paste("cor_asp_",v,"_", c, "m", ".sgrd", sep=""),METHOD="2"))

##	Calculate Catchment Area
FDlist=c("D8", "Rho8", "BR", "Dinf", "MFD", "MTFD")
for(j in 0:5){
rsaga.geoprocessor("ta_hydrology", module=0, param=list(ELEVATION=paste("cor_fill_",v,"_",c,"m",".sgrd", sep=""), Method=j, CAREA=paste("cor_area_",v,"_",c,"m_", FDlist[j+1], ".sgrd", sep="")))
}

##	Calculate TI
start.time <- Sys.time()
for(a in 0:length(FDlist)){
rsaga.geoprocessor("ta_hydrology", module=20, param=list(SLOPE=paste("cor_slp_", v,"_",c, "m", ".sgrd", sep=""), AREA=paste("cor_area_",v,"_",c,"m",FDlist[a],".sgrd",sep=""), CONV="1", METHOD="Standard", TWI=paste("cor_TI_",v,"_",c,"m_",FDlist[a],".sgrd",sep="")))
}
end.time <- Sys.time(); start.time-end.time

start.time <- Sys.time()
##	Fill DEM
#rsaga.fill.sinks("cor_dem_3m.sgrd", "Filled_DEM.sgrd", method = "planchon.darboux.2001")
rsaga.geoprocessor("ta_preprocessor", module=3, param=list(DEM=paste("cor_dem_",v, "_", c, "m", ".sgrd", sep=""), MINSLOPE="0.01", RESULT=paste("cor_fill_",v,"_",c,"m",".sgrd", sep="")))
##	Calculate Slope
rsaga.geoprocessor("ta_morphometry", module=0, param=list(ELEVATION=paste("cor_fill_",v,"_",c,"m",".sgrd", sep=""), SLOPE=paste("cor_slp_",v,"_", c, "m", ".sgrd", sep=""), ASPECT=paste("cor_asp_",v,"_", c, "m", ".sgrd", sep=""),METHOD="2"))
##	Calculate Catchment Area
rsaga.geoprocessor("ta_hydrology", module=0, param=list(ELEVATION=paste("cor_fill_",v,"_",c, "m", ".sgrd", sep=""), Method="5", CAREA=paste("cor_area_",v,"_",c, "m", ".sgrd", sep="")))
##	Calculate TI
rsaga.geoprocessor("ta_hydrology", module=20, param=list(SLOPE=paste("cor_slp_", v,"_",c, "m", ".sgrd", sep=""), AREA=paste("cor_area_", v,"_",c, "m", ".sgrd", sep=""), CONV="1", METHOD="Standard", TWI=paste("cor_TI_",v,"_", c, "m", ".sgrd", sep="")))
##	Filter TI
rsaga.geoprocessor("grid_filter", module=0, param=list(INPUT=paste("cor_TI_",v,"_", c, "m", ".sgrd", sep=""), RESULT=paste("cor_TI_",v,"_", c, "m","_F", ".sgrd", sep=""), MODE="0", METHOD="Smooth", RADIUS=1))
##	Calculate STI
rsaga.geoprocessor("ta_hydrology", module=20, param=list(SLOPE=paste("cor_slp_",v,"_", c, "m", ".sgrd", sep=""), AREA=paste("cor_area_",v,"_", c, "m", ".sgrd", sep=""), CONV="1", METHOD="Standard", TRANS=paste("cor_tran_",v,"_", c, "m", ".sgrd", sep=""), TWI=paste("cor_STI_",v,"_", c, "m", ".sgrd", sep="")))
##	Filter STI
rsaga.geoprocessor("grid_filter", module=0, param=list(INPUT=paste("cor_STI_",v,"_", c, "m", ".sgrd", sep=""), RESULT=paste("cor_STI_",v,"_", c, "m","_F", ".sgrd", sep=""), MODE="0", METHOD="Smooth", RADIUS=1))
##	Calculate Downslope Distance Gradient
rsaga.geoprocessor("ta_morphometry", module=9, param=list(DEM=paste("cor_fill_",v,"_", c, "m", ".sgrd", sep=""), DISTANCE="2", OUTPUT="2", GRADIENT=paste("cor_gradient_",v,"_", c, "m", ".sgrd", sep="")))
##	Calculate Downslope TI
rsaga.geoprocessor("ta_hydrology", module=20, param=list(SLOPE=paste("cor_gradient_",v,"_", c, "m", ".sgrd", sep=""), AREA=paste("cor_area_",v,"_", c, "m", ".sgrd", sep=""), CONV="1", METHOD="Standard", TWI=paste("cor_TI_DwnSlp_",v,"_", c, "m", ".sgrd", sep="")))
##	Filter Downslope TI
rsaga.geoprocessor("grid_filter", module=0, param=list(INPUT=paste("cor_TI_DwnSlp_",v,"_", c, "m", ".sgrd", sep=""), RESULT=paste("cor_TI_DwnSlp_",v,"_", c, "m","_F",".sgrd", sep=""), MODE="0", METHOD="Smooth", RADIUS=1))
##	Calculate Downslope STI
rsaga.geoprocessor("ta_hydrology", module=20, param=list(SLOPE=paste("cor_gradient_", v,"_",c, "m", ".sgrd", sep=""), AREA=paste("cor_area_", v,"_",c, "m", ".sgrd", sep=""), CONV="1", METHOD="Standard", TRANS=paste("cor_tran_",v,"_", c, "m", ".sgrd", sep=""), TWI=paste("cor_STI_DwnSlp_",v,"_", c, "m", ".sgrd", sep="")))
##	Filter Downslope STI
rsaga.geoprocessor("grid_filter", module=0, param=list(INPUT=paste("cor_STI_DwnSlp_",v,"_", c, "m", ".sgrd", sep=""), RESULT=paste("cor_STI_DwnSlp_",v,"_", c, "m","_F",".sgrd", sep=""), MODE="0", METHOD="Smooth", RADIUS=1))
end.time <- Sys.time()
start.time-end.time

####		LOOPED Flow Direction Calculation
rsaga.get.usage("ta_hydrology", 0)
FDlist=c("D8", "Rho8", "BR", "Dinf", "MFD", "MTFD")

for(j in 0:5){
##	Calculate Catchment Area
rsaga.geoprocessor("ta_hydrology", module=0, param=list(ELEVATION=paste("cor_fill_",v,"_",c,"m",".sgrd", sep=""), Method=j, CAREA=paste("cor_area_",v,"_",c,"m", FDlist[j+1], ".sgrd", sep="")))
}

#########		Extract TI VALUES from Points
#### CRC
setwd("C:/Thresholds/Field_Data/CRC/CSVs")
smdata=read.csv("CRC_Master.csv",sep=",", header=TRUE)
xy <- cbind(smdata$x_proj, smdata$y_proj)
setwd("C:/Thresholds/SAGA")

#Pick.from.saga.grid
colnames(turd)
head(turd)
is.data.frame(turd)
turd <- as.data.frame(xy, col.names=c("X.names", "Y.names"))
names(turd) <- c("X.name", "Y.name")
pick.from.saga.grid(turd,"cor_TI_U_10m.sgrd",path="C:/Thresholds/Field_Data/CRC/CSVs", varname="interp", prec = 7, method="nearest.neighbour")
show.output.on.console = TRUE)

TIgrid <- read.sgrd("cor_TI_L_3m.sgrd") 
TIgrid <- read.sgrd("cor_TI_U_10m.sgrd") 
TIgrid <- read.sgrd("cor_TI_L_10m.sgrd") 
xyz = grid.to.xyz(TIgrid,varname="z");  str(xyz)
TIgrid <- rasterFromXYZ(xyz, res=c(NA,NA), crs="+proj=utm +zone=18 ellps=WGS84", digits=5)
raster(TIgrid)

STIsF_tau_simp <- extract(TIgrid,xy, method='simple')
STIs_tau_simp <- extract(TIgrid,xy, method='bilinear')

plot(STIsF_tau_simp, smdata$Average,tck=0.02, mgp=c(3,.05,0));legend("top", bty="n", cex=1)
	lines(supsmu(STIsF_tau_simp,smdata$Average),col=2)
	Linear <- lm(smdata$Average~STIsF_tau_simp)
	abline(Linear, lwd=1.8)
	summary(Linear)
	text(.6*max(STIsF_tau_simp,na.rm=TRUE),.75*max(smdata$Average,na.rm=TRUE),paste(round(summary(Linear)$adj.r.squared, digits=2), round(coef(summary(Linear))[8], digits=4),sep=" , "),col=4)

#	Lattice Plot by Date
library(lattice)
par(mar=c(1.5,1.2,0,0), oma=c(5,3,2,5))
xyplot(smdata$Average~STIs_tau_simp | smdata$Date, span=1, aspect="xy",  panel=function(x,y,subscripts){panel.xyplot(x,y);panel.lmline(x,y);panel.text(10,10,summary(lm(x~y))$r.squared)})

xyplot(smdata$Average~STIsF_tau_simp | smdata$Date, span=1, aspect="xy",  panel=function(x,y,subscripts){panel.xyplot(x,y);panel.lmline(x,y);panel.text(10,10,summary(lm(x~y))$r.squared)})

### Correlations
Spearman <- cor(STIs_tau_simp, smdata$Average, method="spearman",use="complete.obs")
Spearman <- cor(STIsF_tau_simp, smdata$Average, method="spearman",use="complete.obs")
cor(Combined, smdata$Average, method="spearman")
Spearman <- cor(Combined, smdata$Average, method="spearman")
Highest_corr <- rownames(Spearman)
Highest_corr[[which(Spearman==max(Spearman))]]

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, method="pearson"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- .8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(Combined, lower.panel=panel.smooth, upper.panel=panel.cor, main="Pearson Correlation Matrix: 10m DEM")
pairs(x, lower.panel=panel.smooth, upper.panel=panel.cor, main="Pearson Correlation Matrix: 10m DEM")

# Correlations with significance levels
library(Hmisc)
x <- cbind(smdata$Average,STIs_tau_simp)
rcorr(x, type="spearman") # type can be pearson or spearman
rcorr(STIs_tau_simp, smdata$Average, type="pearson") # type can be pearson or spearman
cor(STIs_tau_simp, smdata$Average, method="spearman",use="complete.obs")

library(Hmisc)
x <- cbind(smdata$average,Combined)
rcorr(x, type="pearson") # type can be pearson or spearman
rcorr(Combined, smdata$Average, type="pearson") # type can be pearson or spearman
cor(x, method="spearman")

cor.test(STIs_tau_simp, smdata$Average, method="spearman")

x <- c(-2, -1, 0, 1, 2)
y <- c(4,   1, 0, 1, 4)
z <- c(1,   2, 3, 4, NA)
v <- c(1,   2, 3, 4, 5)
rcorr(cbind(x,y,z,v))

####		LOOPED Flow Direction Calculation
rsaga.get.usage("ta_hydrology", 0)
FDlist=c("D8", "Rho8", "BR", "Dinf", "MFD", "MTFD")

for(j in 0:5){
##	Calculate Catchment Area
rsaga.geoprocessor("ta_hydrology", module=0, param=list(ELEVATION=paste("cor_fill_", c, "m", ".sgrd", sep=""), Method=j, CAREA=paste("cor_area_", c, "m", FDlist[j+1], ".sgrd", sep="")))
}

#############		SAGA Reference
# make sure that ’rsaga.env’ can find ’saga_cmd.exe’
rsaga.env()
# before running this:
rsaga.get.libraries()
# list all modules in my favorite libraries:
rsaga.get.modules(c("io_grid", "grid_tools", "ta_preprocessor","ta_morphometry"))
rsaga.get.modules(c("ta_preprocessor","ta_morphometry","ta_hydrology" ))
rsaga.get.modules(c("grid_filter" ))
# list *all* modules (quite a few!):
# Find out module parameters
rsaga.get.usage("grid_filter", 0)
rsaga.get.usage("ta_morphometry", 0)
rsaga.get.usage("ta_morphometry", 9)
rsaga.get.usage("ta_hydrology", 0)
rsaga.get.usage("ta_hydrology", 15)
rsaga.get.usage("ta_hydrology", 20)
rsaga.get.usage("grid_gridding", 4)
rsaga.get.usage("ta_preprocessor", 3)
# rsaga.get.modules(interactive=TRUE)
# find modules that remove sink from DEMs:
rsaga.search.modules("sink")
# find modules that close gaps (no-data areas) in grids:
rsaga.search.modules("gap")
## End(Not run)	
rsaga.get.modules("grid_spline")

##############		EXTRA

######		RASTER PACKAGE - READING IN TI VALUES
DEM <- read.sgrd("cor_dem_3m.sgrd",env=myenv)
TIgrid <- readGDAL("TI_Std_D8.sgrd") 
TIgrid <- read.sgrd("cor_TI_DwnSlp_3m.sgrd") 

TIgrid <- rsaga.sgrd.to.esri("cor_TI_DwnSlp_10m.sgrd", ) 

pick.from.saga.grid(data, filename, path, varname, prec = 7, show.output.on.console = FALSE, env = rsaga.env(), ...)

### Calculate SAGA TI
##	Calculate SAGA TI - t=10
rsaga.geoprocessor("ta_hydrology", module=15, param=list(DEM=paste("cor_fill_", c, "m", ".sgrd", sep=""), C=paste("cor_SAGA_area_", c, "m", ".sgrd", sep=""), GN=paste("cor_SAGA_slp_", c, "m", ".sgrd", sep=""),CS=paste("cor_SAGA_modarea_", c, "m", ".sgrd", sep=""), SB=paste("cor_SAGA_TI_t=10_", c, "m", ".sgrd", sep=""), T="10"))
##	Calculate SAGA TI - t=2
rsaga.geoprocessor("ta_hydrology", module=15, param=list(DEM=paste("cor_fill_", c, "m", ".sgrd", sep=""), C=paste("cor_SAGA_area_", c, "m", ".sgrd", sep=""), GN=paste("cor_SAGA_slp_", c, "m", ".sgrd", sep=""),CS=paste("cor_SAGA_modarea_", c, "m", ".sgrd", sep=""), SB=paste("cor_SAGA_TI_t=2_", c, "m", ".sgrd", sep=""), T="2"))
##	Calculate SAGA TI - t=30
rsaga.geoprocessor("ta_hydrology", module=15, param=list(DEM=paste("cor_fill_", c, "m", ".sgrd", sep=""), C=paste("cor_SAGA_area_", c, "m", ".sgrd", sep=""), GN=paste("cor_SAGA_slp_", c, "m", ".sgrd", sep=""),CS=paste("cor_SAGA_modarea_", c, "m", ".sgrd", sep=""), SB=paste("cor_SAGA_TI_t=30_", c, "m", ".sgrd", sep=""), T="30"))

###	Example SAGA Code
N.sim <- 10
DEM.sim <- predict.gstat(vt.gt, bargrid, nsim=N.sim, debug.level=-1)
fullgrid(DEM.sim) <- TRUE
stream.list <- list(rep(NA, N.sim))
# derive stream networks in SAGA GIS 2.0.8:
for (i in 1:N.sim) {
  writeGDAL(DEM.sim[i], paste("DEM", i, ".sdat", sep=""), "SAGA", mvFlag = -99999)
  # filter the spurious sinks:
  rsaga.fill.sinks(in.dem=paste("DEM", i, ".sgrd", sep=""), out.dem="DEMflt.sgrd")
  # extract the channel network SAGA GIS 2.0.6:
  rsaga.geoprocessor(lib="ta_channels", module=0, param=list(ELEVATION="DEMflt.sgrd", 
    CHNLNTWRK=paste("channels", i, ".sgrd", sep=""), CHNLROUTE="channel_route.sgrd", 
    SHAPES="channels.shp", INIT_GRID="DEMflt.sgrd", DIV_CELLS=3, MINLEN=40), 
    show.output.on.console=FALSE)
  stream.list[[i]] <- readOGR("channels.shp", "channels", verbose=FALSE)
  proj4string(stream.list[[i]]) <- barxyz@proj4string
}


