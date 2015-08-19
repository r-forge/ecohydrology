AreaD8 = function (d8, xs=c(1),ys=c(1))
{
  stopifnot( class(d8) %in% c("SpatialGridDataFrame", "RasterLayer") )
  if (class(d8)=="SpatialGridDataFrame") {
    senddata=d8@data$band1
    xdim = d8@grid@cells.dim[1]
    ydim = d8@grid@cells.dim[2]
    res=d8@grid@cellsize[1]
  } else {
    senddata=values(d8)
    xdim = d8@ncols
    ydim = d8@nrows
    res=res(d8)[1] #will fail for a raster w/different x&y resolution...probably a good thing      
  }
  senddata[is.na(senddata)]=-9999
  result <- .C("aread8", PACKAGE = "TauDEM", 
     as.double(senddata),
     ad8 = double(xdim * ydim), 
     as.double(xs),as.double(ys),
     as.integer(length(xs)),
     as.integer(1),
     as.integer(xdim), 
     as.integer(ydim),
     as.double(res))
#int aread8(double *input, double *outputarea, double *x, double *y,long nxy ,int doall, int *nrow, int *ncol, double *cellsize) {

  result$ad8[result$ad8< 0] <- NA
  if (class(d8)=="SpatialGridDataFrame") {d8@data$band1=result$ad8} else {values(d8)=result$ad8}
  resultaread8=d8
  d8=list(resultaread8=resultaread8)
  return(d8)
}


