Source = function (ad8,d8sl, ipar=1,p=c(1000,0,0,0),xs=c(1),ys=c(1))
{
  stopifnot( class(ad8) %in% c("SpatialGridDataFrame", "RasterLayer") )
  if (class(ad8)=="SpatialGridDataFrame") {
    areafile=ad8@data$band1
    dirfile=d8sl@data$band1
    xdim = ad8@grid@cells.dim[1]
    ydim = ad8@grid@cells.dim[2]
    res=ad8@grid@cellsize[1]
  } else {
    areafile=values(ad8)
    dirfile=values(d8sl)
    xdim = ad8@ncols
    ydim = ad8@nrows
    res=res(ad8)[1] #will fail for a raster w/different x&y resolution...probably a good thing      
  }
  areafile[is.na(areafile)]=-9999
  dirfile[is.na(dirfile)]=-9999
  print(paste(xdim,ydim,xdim*ydim,ipar,p[1],length(areafile),length(dirfile)))
  result <- .C("source", PACKAGE = "TauDEM", 
     as.double(areafile),
     as.double(dirfile),
     source = double(xdim * ydim), 
     as.integer(ipar),
     as.double(p),
     as.integer(length(xs)),
     as.double(xs),as.double(ys),
     as.integer(1), # contcheck
     as.integer(1), # droppan
     as.integer(1), # masksca
     as.integer(xdim), 
     as.integer(ydim),
     as.double(res))
#int aread8(double *input, double *outputarea, double *x, double *y,long nxy ,int doall, int *nrow, int *ncol, double *cellsize) {

  result$source[result$source< 0] <- NA
  if (class(ad8)=="SpatialGridDataFrame") {ad8@data$band1=result$source} else {values(ad8)=result$source}
  resultsource=ad8
  source=list(resultsource=resultsource)
  return(source)
}


