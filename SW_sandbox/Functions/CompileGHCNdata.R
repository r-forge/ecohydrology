Comp2GHCN#  Function created 3/21/2014 by Jo Archibald (jaa78@cornell.edu)

##  TWO functions here:  
## CompileGHCN :  reads in output from getGHCN_shape or getGHCN and combines the P, Tn and Tx from all stations to give an average P (mm), Tn (mm) and Tx (mm).
## Comp2GHCN : Calls the CompileGHCN function for 2 separate lists of GHCN datasets.
#  It defaults to the vales from the first object, only uses the second to fill in missing values.  It returns a column called P_filled = TRUE when P had to be filled from the second GHCN object.


CompileGHCN <- function(GHCNdata, Start = "2000-01-01", End = "2013-12-31", wgt=rep(1,length(GHCNdata)), silent=FALSE) {  
  DATE <- seq(as.Date(Start), as.Date(End), by=1)
  GHCNdata[[length(GHCNdata)]] <- NULL  #  Remove the last element of GHCNdata, StationInfo
  
  #  Create a data frame containing all the precip and temp data in the GHCN record, 
  #   and converting units to mm and C
  NOAA_gages <- as.data.frame(matrix(nrow=length(DATE), ncol=1+3*length(GHCNdata)))
  names(NOAA_gages) <- c("DATE", paste("P", seq(1,length(GHCNdata)), sep=""), paste("Tx", seq(1,length(GHCNdata)), sep=""), paste("Tn", seq(1,length(GHCNdata)), sep=""))
  NOAA_gages$DATE <- DATE
  
  for (j in 1:length(GHCNdata)){ 
      dat <- GHCNdata[[j]][which(GHCNdata[[j]]$date>=DATE[1] & GHCNdata[[j]]$date<=max(DATE)),]
	  
	  notMissing <- data.frame(dat, as.data.frame(matrix(nrow=nrow(dat), ncol=(4-ncol(dat)))))   ## This assumes that we never have Tn or Tx without PRCP first..  Otherwise the Tn and Tx will be put into precip column..
	  Missing <- data.frame(DATE = DATE[which(!DATE %in% dat$date)], as.data.frame(matrix(nrow=length(which(!DATE %in% dat$date)), ncol=3)))
	  names(Missing) = names(notMissing)  ## assign the same names to the Missing data frame so that we can use rbind()
	  New <- rbind(Missing, notMissing)
	  NOAA_gages[,c((j+1),(j+1+length(GHCNdata)),(j+1+2*length(GHCNdata)))] <- 
	  New[order(New$date),2:4] / 10  ##  CHANGE HERE if the getGHCN function changes

  }
  
  #  Replace -999.9 with a value of NA
  ReplaceNA <- function(x, NAval=-999.9){
    x[which(x==NAval)] <- NA
    return(x)
  }
  NOAA_gages[,2:ncol(NOAA_gages)]<-sapply(NOAA_gages[,2:ncol(NOAA_gages)], ReplaceNA)
  
	NOAA_gages$P <- sapply(apply(NOAA_gages[,2:(1+length(GHCNdata))],MARGIN=1,FUN=na.omit),MARGIN=1,FUN=mean)
	if (length(sapply(apply(NOAA_gages[,(2+length(GHCNdata)):(1+2*length(GHCNdata))],MARGIN=1,FUN=na.omit),MARGIN=1,FUN=mean)) == nrow(NOAA_gages)){  #  If no temp data is available, there will be no Tn or Tx column returned
	NOAA_gages$Tx <- sapply(apply(NOAA_gages[,(2+length(GHCNdata)):(1+2*length(GHCNdata))],MARGIN=1,FUN=na.omit),MARGIN=1,FUN=mean)
	NOAA_gages$Tn <- sapply(apply(NOAA_gages[,(2+2*length(GHCNdata)):(1+3*length(GHCNdata))],MARGIN=1,FUN=na.omit),MARGIN=1,FUN=mean)
	}
	
  if (!silent) print(summary(NOAA_gages))
  return(NOAA_gages)
}


##  This function tries to create a continuous daily record of P and max/min temps for GHCN gage information from output from the getGHCN and/or getGHCN_shape functions
#  Arguments: GHCNopt:  These are the optimal GHCN stations, for example, gages within the watershed.
#  GHCN2:  GHCN gages that can be used to fill in the missing values from the GHCNopt records

Comp2GHCN <- function(GHCNopt, GHCN2, Start = "2000-01-01", End = "2013-12-31"){
	g1 <- CompileGHCN(GHCNopt, Start = "2000-01-01", End = "2013-12-31", silent=TRUE)
	g2 <- CompileGHCN(GHCN2, Start = "2000-01-01", End = "2013-12-31", silent=TRUE)
	
	if ("P" %in% names(g1)){
	P <- g1$P
	P_filled <- rep(FALSE, length(P))
	P_filled[which(is.na(P))] <- TRUE
	P[which(is.na(P))] <- g2$P[which(is.na(P))]
	} else P <- g2$P
	if ("Tn" %in% names(g1)){
	Tn <- g1$Tn
	Tn[which(is.na(Tn))] <- g2$Tn[which(is.na(Tn))]
	} else Tn <- g2$Tn
	if ("Tx" %in% names(g1)){
	Tx <- g1$Tx
	Tx[which(is.na(Tx))] <- g2$Tx[which(is.na(Tx))]
	} else Tx <- g2$Tx
	return(data.frame(DATE=g1$DATE, P, Tn, Tx, P_filled))	
}





