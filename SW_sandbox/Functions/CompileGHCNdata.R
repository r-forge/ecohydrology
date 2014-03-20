CompileGHCN <- function(GHCNdata, Start = "2000-01-01", End = "2013-12-31", wgt=rep(1,length(GHCNdata))) {  
  DATE <- seq(as.Date(Start), as.Date(End), by=1)
  
  #  Create a data frame containing all the precip and temp data in the GHCN record, 
  #   and converting units to mm and C
  NOAA_gages <- as.data.frame(matrix(nrow=length(DATE), ncol=1+3*length(GHCNdata)))
  names(NOAA_gages) <- c("DATE", paste("P", seq(1,length(GHCNdata)), sep=""), paste("Tn", seq(1,length(GHCNdata)), sep=""), paste("Tx", seq(1,length(GHCNdata)), sep=""))
  NOAA_gages$DATE <- DATE
  for (i in 1:length(DATE)){
    for (j in 1:length(GHCNdata)){
      NOAA_gages[,(j+1)][i] <- if(length(which(GHCNdata[[j]]$date==DATE[i]))==1 & length(GHCNdata[[j]]$PRCP)>0) GHCNdata[[j]]$PRCP[which(GHCNdata[[j]]$date==DATE[i])]/10 else NA
      NOAA_gages[,(j+1+length(GHCNdata))][i] <- if(length(which(GHCNdata[[j]]$date==DATE[i]))==1 & length(GHCNdata[[j]]$TMIN)>0) GHCNdata[[j]]$TMIN[which(GHCNdata[[j]]$date==DATE[i])]/10 else NA
      NOAA_gages[,(j+1+2*length(GHCNdata))][i] <- if(length(which(GHCNdata[[j]]$date==DATE[i]))==1 & length(GHCNdata[[j]]$TMAX)>0) GHCNdata[[j]]$TMAX[which(GHCNdata[[j]]$date==DATE[i])]/10 else NA
    }
  }
  
  #  Replace -999.9 with a value of NA
  ReplaceNA <- function(x, NAval=-999.9){
    x[which(x==NAval)] <- NA
    return(x)
  }
  NOAA_gages[,2:ncol(NOAA_gages)]<-sapply(NOAA_gages[,2:ncol(NOAA_gages)], ReplaceNA)
  
  ##  Allows us to weight the different gages (must be non-negative integers. Zero values removes that data-set)
  wtmean <- function(x,y){
    return(sum(na.omit(x*y))/sum(na.omit((x+y-x))))
  }
  
  if (mean(wgt)==1 & sd(wgt)==0){
    NOAA_gages$P <- sapply(apply(NOAA_gages[,2:(1+length(GHCNdata))],MARGIN=1,FUN=na.omit),MARGIN=1,FUN=mean)
    NOAA_gages$Tn <- sapply(apply(NOAA_gages[,(2+length(GHCNdata)):(1+2*length(GHCNdata))],MARGIN=1,FUN=na.omit),MARGIN=1,FUN=mean)
    NOAA_gages$Tx <- sapply(apply(NOAA_gages[,(2+2*length(GHCNdata)):(1+3*length(GHCNdata))],MARGIN=1,FUN=na.omit),MARGIN=1,FUN=mean)
  } else{  ##  If we want to weight the gages differently, it takes much longer
    NOAA_gages$P <- apply(NOAA_gages[,2:(1+length(GHCNdata))],MARGIN=1,FUN=wtmean, y=wgt)
    NOAA_gages$Tn <- apply(NOAA_gages[,(2+length(GHCNdata)):(1+2*length(GHCNdata))],MARGIN=1,FUN=wtmean, y=wgt)
    NOAA_gages$Tx <- apply(NOAA_gages[,(2+2*length(GHCNdata)):(1+3*length(GHCNdata))],MARGIN=1,FUN=wtmean, y=wgt)
  }
  print(summary(NOAA_gages))
  return(NOAA_gages)
}
