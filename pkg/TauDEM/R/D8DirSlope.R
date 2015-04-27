D8DirSlope = function (DEM, degree=1)
{
  stopifnot( class(DEM) %in% c("SpatialGridDataFrame", "RasterLayer") )
  if (class(DEM)=="SpatialGridDataFrame") {
    senddata=DEM@data$band1
    xdim = DEM@grid@cells.dim[1]
    ydim = DEM@grid@cells.dim[2]
    res=DEM@grid@cellsize[1]
  } else {
    senddata=values(DEM)
    xdim = DEM@ncols
    ydim = DEM@nrows
    res=res(DEM)[1] #will fail for a raster w/different x&y resolution...probably a good thing      
  }
  senddata[is.na(senddata)]=-9999
  result <- .C("setdird8", PACKAGE = "TauDEM", as.double(senddata),resultd8 = double(xdim * ydim),resultslope = double(xdim * ydim), as.integer(xdim), as.integer(ydim),as.double(res), as.double(degree))

  result$resultd8[result$resultd8< 0] <- NA
  result$resultslope[result$resultslope< 0] <- NA
  if (class(DEM)=="SpatialGridDataFrame") {DEM@data$band1=result$resultd8} else {values(DEM)=result$resultd8}
  resultd8=DEM  
  if (class(DEM)=="SpatialGridDataFrame") {DEM@data$band1=result$resultslope} else {values(DEM)=result$resultslope}
  resultslope=DEM  
  DEMs=list(resultd8=resultd8,resultslope=resultslope)
  return(DEMs)
}


