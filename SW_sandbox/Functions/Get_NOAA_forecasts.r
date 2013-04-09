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

forecastsNOAA <- function(stn = c("KITH", "KSYR") , probCut = 30, numbDays = 3, PrintDataTables = TRUE) {
		
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
		PrbPrecip <- vector(length = numbDays)
		
	#  We first need to get the most recent 6 hours of recorded data from this station: 
		library(XML)
		# temp <- readHTMLTable(paste("http://www.nws.noaa.gov/data/obhistory/", stn[i], ".html", sep=""), skip.rows=c(1,2,3,70,71,72,73,74,75,76,77,78), header=FALSE)[[4]]   ##  This one seems slower to update? 
		Cur6hr <- readHTMLTable(paste("http://w1.weather.gov/data/obhistory/", stn[i], ".html", sep=""), skip.rows=c(1,2,3,70,71,72,73,74,75,76,77,78), header=FALSE, stringsAsFactors=FALSE)[[4]]
		colnames(Cur6hr) <- c("day", "time", "wind_mph", "vis_mi", "weath", "Sky", "AirTempF", "DewPt_F", "Temp6hrMax", "Temp6hrMin", "RH", "WindChill_F", "HeatIndex_F", "Press_alt_in", "Press_SL_mb", "Prec_1hr_in", "Prec_3hr_in","Prec_6hr_in")
		MaxTemp6 <- max(na.omit(as.numeric(c(Cur6hr[1:6,9],Cur6hr[1:6,7]))))  		# F (will be converted below)
		MinTemp6 <- min(na.omit(as.numeric(c(Cur6hr[1:6,10],Cur6hr[1:6,7]))))	
		precip6 <- suppressWarnings(sum(na.omit(as.numeric(Cur6hr$Prec_1hr_in[1:6])))) * 25.4		# mm
		
	#  Read in forecasts from the short-term forecast (MOS and MAV), and format it (final table called ForecastTable):
		forecasts <- read.fwf(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmav.pl?sta=", stn[i], sep=""), fill=TRUE, skip=7, row.names=NULL, widths=c(5,rep(3,21)),colClasses="character")[1:20,]
		StationInfo[2*i-1] <- readLines(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmav.pl?sta=", stn[i], sep=""))[6]
		f1 <- apply(forecasts,1,as.character) 		# changes everything to character, and transposes table
		tis <- gsub(" ","",f1[1,])  												# Row names in web table, column names now
		tis[which(tis=="X/N" | tis=="N/X")] <- "MxMn"				# Rename max/min temp column
		f2 <- f1[-1,]
		colnames(f2) <- tis
		ForecastTable <- suppressWarnings(data.frame(apply(f2,2,as.numeric)))    # Convert to numeric. Final data frame. 
		
	#  If we want forecasts for more than 3 days, we need to get the extended forecast : MOS and MEX 
		forecasts <- read.fwf(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmex.pl?sta=", stn[i], sep=""), fill=TRUE, skip=6, row.names=NULL, widths=c(5,rep(4,15)),colClasses="character")[1:16,1:14]
		StationInfo[2*i] <- readLines(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmex.pl?sta=", stn[i], sep=""))[6]
		f1 <- apply(forecasts,1,function(x) gsub("|","",x, fixed=TRUE)) 		# removes "|", and transposes table
		tis <- gsub(" ","",f1[1,]) 												# Row names in web table, column names now
		tis[which(tis=="X/N" | tis=="N/X")] <- "MxMn"				# Rename max/min temp column
		f2 <- f1[-1,]
		colnames(f2) <- tis
		ForecastTableExtend <- suppressWarnings(data.frame(apply(f2,2,as.numeric)))    # Convert to numeric. Final data frame
	
	#  First day's forecast is patched together from 6 hours earlier, and the first 3 6-hr predictions from MAV
		MxT[1,i] <- (max(ForecastTable$TMP[1:7],MaxTemp6)-32)*5/9 # Max temp predictions 
		MnT[1,i] <- (min(ForecastTable$TMP[1:7],MinTemp6)-32)*5/9 # Min temp predictions 
		Precip[1,i] <- sum(MAVcodes[2, match(ForecastTable$Q06[which(ForecastTable$P06[1:8] > probCut)], MAVcodes[1,] )],precip6)  
		Probs[1,i] <- max(ForecastTable$P06[1:8], na.rm=TRUE)
		
	#  Now we will pull out Max and Min temps, and predicted precipitation for the rest of the prediction from the long-term forecast
		for (j in 2:numbDays){
			if (TRUE %in% is.na(ForecastTableExtend$MxMn[(2*j-3):(2*j-2)])){  # If 24 hr Max/Min Problem, probably won't use
				print("Using 12-hr TEMP instead of 24-hr MxMn")
				MxT[j,i] <- (max(ForecastTableExtend$TMP[(2*j-3):(2*j-2)])-32)*5/9 # Not as accurate max/min result
				MnT[j,i] <- (min(ForecastTableExtend$TMP[(2*j-3):(2*j-2)])-32)*5/9 # Not as accurate max/min result
			} else{		# This is default
			MxT[j,i] <- (max(ForecastTableExtend$MxMn[(2*j-3):(2*j-2)])-32)*5/9 # Max temp predictions 
			MnT[j,i] <- (min(ForecastTableExtend$MxMn[(2*j-3):(2*j-2)])-32)*5/9 # Min temp predictions 
			}
			if (is.na(ForecastTableExtend$P24[j-1]) | is.na(ForecastTableExtend$Q24[j-1])){
				print("Using Q12 instead of Q24")
				Precip[j,i] <- sum(MAVcodes[2, match(ForecastTableExtend$Q12[which(ForecastTableExtend$P12[(2*j-3):(2*j-2)] > probCut)], MAVcodes[1,] )],0)
				Probs[j,i] <- max( ForecastTableExtend$P12[(2*j-3):(2*j-2)] )
			} else {
				Precip[j,i] <- ifelse(ForecastTableExtend$P24[j-1] > probCut, MAVcodes[2, which(MAVcodes[1,] == ForecastTableExtend$Q24[j-1])], 0)  
				Probs[j,i] <- ForecastTableExtend$P24[j-1]
			}
			
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
	}
	##  Just for checking that this is working initially: (can delete these lines soon)
	cat("Daily FORECASTS:\n")
	print(StationInfo)
	cat("Max temp\n")
	print(MxT)
	cat("Min temp\n")
	print(MnT)
	cat("Precip mm\n")
	print(Precip)
	##  DELETE TO HERE
	return(list(StationInf = StationInfo, MxT = MxT, MnT = MnT, Pmm = Precip, PrbPrecip = Probs))
}



