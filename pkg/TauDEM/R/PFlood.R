PFlood=function(futm){
  corecols=detectCores()
  startcols=floor(pmax(1,head(seq(-10,futm@ncols,futm@ncols/corecols),-1)))
  endcols=floor(tail(c(seq(10,futm@ncols,futm@ncols/corecols),futm@ncols),-1))

  futmslist=list()
  for(i in 1:corecols){
     cells=c(cellFromRowCol(futm,1,startcols[i]),cellFromRowCol(futm,futm@nrows,endcols[i]))
     futm1=rasterFromCells(futm,cells)
     futm1=futm1*0+futm
     rnm=paste0("r",i)
     futmslist[rnm]=futm1
  }

  t1<-mclapply(futmslist,function(r) Flood(r)$resultfel,mc.cores=detectCores())
  futmfelpre=eval(parse(text=paste0("mosaic(",paste0("t1$",ls(t1),collapse=","),",fun=mean)")))
  futmfel=Flood(futmfelpre)
  return(futmfel)
}
