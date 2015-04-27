#the value expected by the .C call that was previously called "nrow" is actually the x-dim/ncols of either SpatialGrid or raster objects
#changed to "x-dim/y-dim" with same .C call ordering...

flood = function (DEM, degree=1)
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
  result <- .C("flood", PACKAGE = "TauDEM", as.double(senddata),
               result = double(xdim * ydim), as.integer(xdim), as.integer(ydim),
               as.double(res), as.double(degree))$result
  result[result < 0] <- NA
  if (class(DEM)=="SpatialGridDataFrame") {DEM@data$band1=result} else {values(DEM)=result}
  return(DEM)
}


