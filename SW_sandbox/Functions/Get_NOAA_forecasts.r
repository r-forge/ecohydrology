##################################################################################################################################  
###################  Generating predicted values of Mx/Mn T (C) and Precip (mm) for a given NOAA station
##################   		Ithaca 			
#  Developed by Jo Archibald, jaa78@cornell.edu
########################################################
##  Locations near Ithaca:  
##  Ithaca:  	KITH
##	Syracuse: 	KSYR
##	Elmira:		KELM
##  Penn Yan:	KPEO
##	Binghamton:	KBGM
##	Fulton:		KFZY
##  Can help find stations: http://weather.uwyo.edu/models/mos/mos.html

forecastsNOAA <- function(stn = c("KITH", "KSYR") , probCut = 30, numbDays = 5, PrintDataTables = FALSE) {
		
	##  This allows us to convert from the numbers in the prediction table, to an actual depth of precip (mm)
	##	Since we want the tool to be conservative, I am using the max precip depth for that code, 
	##	Max code (6) for any prediction above 2 inches is set to 2.5 inches.
	MAVcodes<-rbind(c(0,1,2,3,4,5,6), c(0,0.09, 0.24, 0.49, 0.99, 1.99, 2.5)*25.4) 
	
	MxT <- matrix(nrow=numbDays, ncol=length(stn), dimnames=list(seq(1:numbDays),stn))  # Max temp predictions for all stations
	MnT <- matrix(nrow=numbDays, ncol=length(stn), dimnames=list(seq(1:numbDays),stn))  # Min temp predictions for all stations
	Precip <- matrix(nrow=numbDays, ncol=length(stn), dimnames=list(seq(1:numbDays),stn))  # Precip predictions for all stations
	Probs <-  matrix(nrow=numbDays, ncol=length(stn), dimnames=list(seq(1:numbDays),stn))  # Probability of precip for all stations
	StationInfo <- vector(length=length(stn)*2)
	
##  Gather forecasts and recent data for each station: 	
	for (i in 1:length(stn) ){
		PredTime <- vector(length = numbDays)
		
	#  Read in forecasts from the short-term forecast (MOS and MAV), and format it (final table called ForecastTable):
		forecasts <- read.fwf(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmav.pl?sta=", stn[i], sep=""), fill=TRUE, skip=7, row.names=NULL, widths=c(5,rep(3,21)),colClasses="character")[1:20,]
		StationInfo[2*i-1] <- readLines(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmav.pl?sta=", stn[i], sep=""))[6]
		f1 <- apply(forecasts,1,as.character) 		# changes everything to character, and transposes table
		tis <- gsub(" ","",f1[1,])  												# Row names in web table, column names now
		tis[which(tis=="X/N" | tis=="N/X")] <- "MxMn"				# Rename max/min temp column
		f2 <- f1[-1,]
		colnames(f2) <- tis
		ForecastTable <- suppressWarnings(data.frame(apply(f2,2,as.numeric)))    # Convert to numeric. Final data frame. 
		TimeDif <- Sys.time() - as.POSIXlt(strptime(substr(StationInfo[2*i-1],40,47),format="%H%M", tz="UTC"))
		PredTime[1] <- as.character(Sys.time() - TimeDif + 30*3600)    ##  Puts it into Eastern Standard Time insted of UTM
		
	#  If we want forecasts for more than 3 days, we need to get the extended forecast : MOS and MEX 
		forecasts <- read.fwf(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmex.pl?sta=", stn[i], sep=""), fill=TRUE, skip=6, row.names=NULL, widths=c(5,rep(4,15)),colClasses="character")[1:16,1:14]
		StationInfo[2*i] <- readLines(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmex.pl?sta=", stn[i], sep=""))[6]
		f1 <- apply(forecasts,1,function(x) gsub("|","",x, fixed=TRUE)) 		# removes "|", and transposes table
		tis <- gsub(" ","",f1[1,]) 												# Row names in web table, column names now
		tis[which(tis=="X/N" | tis=="N/X")] <- "MxMn"				# Rename max/min temp column
		f2 <- f1[-1,]
		colnames(f2) <- tis
		ForecastTableExtend <- suppressWarnings(data.frame(apply(f2,2,as.numeric)))    # Convert to numeric. Final data frame
	
	#  First day's forecast is from the first 9 prediction rows from MAV 
	# (a reading at 0600 UTM will give a 24-hr prediction from 7 am - 7 am EST or 8 am - 8 am during daylight savings time.)
		MxT[1,i] <- (max(ForecastTable$MxMn[1:9], na.rm=TRUE)-32)*5/9 # Max temp predictions 
		MnT[1,i] <- (min(ForecastTable$MxMn[1:9], na.rm=TRUE)-32)*5/9 # Min temp predictions 
		Precip[1,i] <- sum(MAVcodes[2, match(ForecastTable$Q06[which(ForecastTable$P06[1:9] > probCut)], MAVcodes[1,] )])  
		Probs[1,i] <- max(ForecastTable$P06[1:8], na.rm=TRUE)
		#  Find the times associated with the predictions:
		PredTimeList <- GetPredictionTime(chartExt=ForecastTableExtend, stnHead = StationInfo[2*i])
		
	#  Now we will pull out Max and Min temps, and predicted precipitation for the rest of the prediction from the long-term forecast
		for (j in 2:numbDays){
			if (TRUE %in% is.na(ForecastTableExtend$MxMn[(2*j-1):(2*j)])){  # If 24 hr Max/Min Problem, probably won't use
				print("Using 12-hr TEMP instead of 24-hr MxMn")
				MxT[j,i] <- (max(ForecastTableExtend$TMP[(2*j-1):(2*j)])-32)*5/9 # Not as accurate max/min result
				MnT[j,i] <- (min(ForecastTableExtend$TMP[(2*j-1):(2*j)])-32)*5/9 # Not as accurate max/min result
			} else{		# This is default
			MxT[j,i] <- (max(ForecastTableExtend$MxMn[(2*j-1):(2*j)])-32)*5/9 # Max temp predictions 
			MnT[j,i] <- (min(ForecastTableExtend$MxMn[(2*j-1):(2*j)])-32)*5/9 # Min temp predictions 
			}
			PredTime[j] <- as.character(PredTimeList[2*j])  # Prediction is for 24 hours up to this time.
		#  If either 12-hour probability of precip is greater than our cutoff, we will have the greater of:
			# 2 mm of rain or the sum of the 12-hour predicted rain amounts
			Probs[j,i] <- max( ForecastTableExtend$P12[(2*j-1):(2*j)] )
			Precip[j,i] <- ifelse(Probs[j,i] > probCut, max(MAVcodes[2,2],
				sum(MAVcodes[2, match(ForecastTableExtend$Q12[(2*j-1:0)][which(ForecastTableExtend$P12[(2*j-1:0)] > probCut)], MAVcodes[1,] )],0) ) , 0 )
		}

	if (PrintDataTables == TRUE) {
		DateTime <- strftime(Sys.time(), format='%Y%m%d%H%M')
		Year <- 1900 + as.POSIXlt(as.Date(Sys.time()))$year
		if(! "DataTables" %in% list.files()) dir.create("DataTables")
		if(! Year %in% list.files("DataTables"))	dir.create(paste("DataTables/", Year, sep=""))
		if (i == 1) 	sink(paste("DataTables/", Year, "/", DateTime, "_FORECASTS.txt", sep=""))
			cat("NOAA Forecasts \n")
				cat(paste(stn[i], "\n"))
				cat("ForecastTableExtend \n")
				print(ForecastTableExtend)
				cat("ForecastTable \n")
				print(ForecastTable)
		if (i == length(stn)) 	sink()   # Stops printing to the output file at last station
	}
	
	#  Print the predictions for each station:
	if (b == "OwascoLake"){
		print(StationInfo[i*2])
		print(data.frame(PredTime = as.POSIXct(PredTime, origin = "1970-01-01"),ProbPrc = Probs[,i], P = Precip[,i], Tx = MxT[,i], Tn = MnT[,i]))
	}
	}
	return(list(StationInf = StationInfo, MxT = MxT, MnT = MnT, Pmm = Precip, PrbPrecip = Probs))
}


GetPredictionTime <- function (chartExt=ForecastTableExtend, stnHead = StationInfo[2*i]){
	now <- Sys.time()
	PredTime <- substr(stnHead,40,47)
	TimeDif <- now-as.POSIXlt(strptime(PredTime,format="%H%M", tz="UTC"))
	return(now - TimeDif + chartExt$FHR*3600)  ##  This is the time that corresponds to each prediction
}  ##  I can't figure out how to convert from UTM to EST, so this seems to be the best way to do it...





