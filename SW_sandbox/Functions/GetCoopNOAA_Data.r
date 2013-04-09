## 	Funtion:  Past24hrData  
##	This function will give Precip and Max/Min temp data for the past 24 hours from :
##		- user defined CoCoRaHS (coop, volunteer) data locations (precip only - recorded between 4 am - 8 am once daily), 
##		- and for user defined NOAA stations (temp and precip data - updated every hour, but limited spatial extent)
## 	Converted to a function 3/14/2013
## 	Josephine Archibald (older non-function version called PrecipTempData3.r - check here for ideas if anything breaks)

###############################################################
library(XML)

####			 INPUT EXAMPLES and NOTES:
#	state = "NY"  ##  If multiple states, store them in one character string, separated by |, eg: "NY | PA"
#	CoopStns = c("Auburn", "Freeville", "Locke", "Moravia", "Lansing 6.6 NW")	  
	#	Note, I think there are multple Lansings so that's why I specify that location here.  However, if I keep it more general, I might get multiple readings for one town, and then if one location is not reporting, I could pick up the other.   But be careful to make sure there are not multiple towns with the same name within your state/s of interest.  (could be worthwhile to check periodically as locations get added/changed over time.)
#	CoopBkup = c("Syracuse", "Cortland","Ithaca", "Aurora")
#	NOAAstn = "KITH"  #  Can also input a vector of multiple NOAA station IDs
#	UseCoopPrecip = TRUE  
			# 	Advantages to Co-op data:  More locations, so might find some within a watershed
			#  	Advantage to NOAA :  Updated hourly, good for use before 10 am (otherwise data can be delayed ~24 hours)
#	PrintDataTables=TRUE
	#	This will keep a record of all the co-op records from the state

##  To find Coop Stations near/in watershed, here is a useful map: http://data.cocorahs.org/cocorahs/maps/?country=USA&state=NY&lat=42.35204&lon=-76.31838&date=3/14/2013 
##  Although NOTE - on 3/14/13,  "Auburn 8.3 SSE is not in the text site"... (so the map doesn't seem perfectly consistent).

##  To find NOAA station nearby the watershed: http://weather.uwyo.edu/models/mos/mos.html

################################################################
Past24hrData <- function(state="NY", CoopStns = NULL, CoopBkup = c("Syracuse", "Cortland","Ithaca", "Aurora"), 	NOAAstn = c("KITH", "KSYR"), UseCoopPrecip = TRUE, PrintDataTables = TRUE) {

	todaysDate <- substr(as.character(Sys.time()),1,10)  # For recording input data

	
##  First, the co-op volunteer precipitation readings: (usually available after approx 9:30 am)
	if (UseCoopPrecip == TRUE & !is.null(CoopStns) ){
		CoopP <- data.frame(Names = c(CoopStns,CoopBkup), Type = c(rep("WSH", length(CoopStns)),  rep("BKU", length(CoopBkup))), P_in=rep(NA,length(CoopStns)+length(CoopBkup)),stringsAsFactors=FALSE) 	#  Table that will hold the Precip values from the co-op stations

		PrecipURL <- "http://www.nws.noaa.gov/data/TAR/HYDTAR" 		# This is volunteer data from CoCoRaHS

		t2 <- read.fwf(PrecipURL, fill=TRUE, skip=12, row.names=NULL, widths=c(1,5,7,10,15,5, 30), col.names=c("A", "B", "CODE", "DATE", "C", "PREC_in", "LOC"),colClasses="character")
		##  Flag (3/14/2013) - this depends on the widths being consistant over time - check to make sure that is the case, otherwise go back to PrecipTempData to understand how this data frame was patched together

		todaysTable <- t2[grep(state,t2$LOC),]  ## Make sure to get stations in this state in case of duplicate names in another state.
		todaysTable$PREC_in <- as.numeric(todaysTable$PREC_in)  #  Make Precip column numeric 
		TableInfo<-readLines(PrecipURL, n=7)[7]

		## Append precipitation in inches in CoopP table
		for (i in 1:nrow(CoopP)){
			CoopP$P_in[i] <- mean(todaysTable$PREC_in[grep(paste(CoopP$Names[i], "|", toupper(CoopP$Names[i]), sep=""), todaysTable$LOC)])
		}

		##  Average all the best stations for P depth (mm).  If these aren't available, use backup stations.
		P_mm_coop <- mean(CoopP$P_in[which(CoopP$Type == "WSH")], na.rm=TRUE) * 25.4
		CoStnsNames <- paste(CoopP$Names[which(!is.na(CoopP$P_in) &CoopP$Type == "WSH")], collapse = ", ")
		if (is.na(P_mm_coop)){  # Use backup stations if best stations are no reporting
			P_mm_coop <- mean(CoopP$P_in[which(CoopP$Type == "BKU")], na.rm=TRUE) * 25.4
			CoStnsNames <- paste(CoopP$Names[which(!is.na(CoopP$P_in) &CoopP$Type == "BKU")], collapse = ", ")
		}
	} else {
		P_mm_coop <- NA
		CoStnsNames <- NA
	}

# Here we have an option to save the input files from NOAA and co-op for future reference. 
	if (PrintDataTables == TRUE) {
		DateTime <- strftime(Sys.time(), format='%Y%m%d%H%M')
		Year <- 1900 + as.POSIXlt(as.Date(Sys.time()))$year
		if(! "DataTables" %in% list.files()) dir.create("DataTables")
		if(! Year %in% list.files("DataTables"))	dir.create(paste("DataTables/", Year, sep=""))
		sink(paste("DataTables/", Year, "/", DateTime, ".txt", sep=""))
			cat("Co-op Data \n")
			print(TableInfo)
			print(todaysTable)
	} 
	
	
#  NOAA station Precip and temp data	
	MaxTemp24 <- vector(length=length(NOAAstn)) 	# F (will be converted below)
	MinTemp24 <- vector(length=length(NOAAstn)) 	
	precip24 <- vector(length=length(NOAAstn)) 		# mm
	for (i in 1:length(NOAAstn)){
		CurNOAA <- readHTMLTable(paste("http://w1.weather.gov/data/obhistory/", NOAAstn[i], ".html", sep=""), skip.rows=c(1,2,3,70,71,72,73,74,75,76,77,78), header=FALSE, stringsAsFactors=FALSE)[[4]]  
		colnames(CurNOAA) <- c("day", "time", "wind_mph", "vis_mi", "weath", "Sky", "AirTempF", "DewPt_F", "Temp6hrMax", "Temp6hrMin", "RH", "WindChill_F", "HeatIndex_F", "Press_alt_in", "Press_SL_mb", "Prec_1hr_in", "Prec_3hr_in","Prec_6hr_in")
		MaxTemp24[i] <- max(na.omit(as.numeric(c(CurNOAA[1:24,9],CurNOAA[1:24,7]))))  		# F (will be converted below)
		MinTemp24[i] <- min(na.omit(as.numeric(c(CurNOAA[1:24,10],CurNOAA[1:24,7]))))	
		precip24[i] <- suppressWarnings(sum(na.omit(as.numeric(CurNOAA$Prec_1hr_in[1:24])))) * 25.4		# mm
		if (PrintDataTables == TRUE) {
			cat(NOAAstn[i])		# This will be sent to out DataTable file 
			print(CurNOAA)
		}
	}
	Tx <- (mean(MaxTemp24) - 32) * 5/9   # C
	Tn <- (mean(MinTemp24) - 32) * 5/9   # C
	P_mm_NOAA <- mean(precip24)

	if (PrintDataTables == TRUE) sink()
	
## Return the P and temp values in a data frame: 	
	P_mm <- ifelse (UseCoopPrecip == TRUE & !is.na(P_mm_coop), P_mm_coop, P_mm_NOAA)   # Return 
	return(data.frame(P_mm = P_mm, Tx=Tx, Tn=Tn, P_mm_NOAA=P_mm_NOAA, P_mm_coop=P_mm_coop, CoopStnsReporting = CoStnsNames))
	
}
	
	





#	Past24hrData(CoopStns = c("Auburn", "Freeville", "Locke", "Moravia", "Lansing 6.6 NW"))



## Co-op data (collected by citizen scientists) 
## ONLY Needs to be run ONCE - once the file is set up with these headings, 
## then we want to add the daily data everyday (See below).
# titles<-c("Co-op Data Date Time", "Aburn P (in)", "Cortland Precip (in)", "Ithaca Precip (in)", 
# 	"Freeville P (in)", "Aurora", "Locke", "Syracuse", "Moravia Precip (in)")
# write(titles, file="CoopData3.csv", ncolumns= 9, append=FALSE, sep=",")
# setwd("E:\\Soil and Water Lab\\Modeling\\Owasco\\Precip Data")
## This needs to be run EVERY DAY at approximately 10:30 am
	# TodaysCoopData <- c(TableInfo,AuburnPrecip,CortlandPrecip, IthPrecip, 
				# FreevillePrecip, AuroraPrecip, LockePrecip, SyracusePrecip, MoraviaPrecip)
	# write(TodaysCoopData, file="CoopData3.csv", ncolumns= 9, append=TRUE, sep=",")

## NOAA Data
## This first part only needs to be run once, to set up the file...
# NOAA_NRCCtitles<-c("Time of Download (Data is from 24 hours before this)", "Syr (in)", "Ith (in)", "Ith Game Farm (in)", 
# 	"SYR max temp (F)",	"SYR min temp (F)", "Ith max temp (F)", "Ith min temp (F)")
# write(NOAA_NRCCtitles, file="NOAA_NRCCData.csv", ncolumns= 8, append=FALSE, sep=",")

## This needs to be run EVERY DAY at approximately 10:30 am
	# TodaysNOAAData <- c(as.character(DataDownloadTime),PrecipSyrNOAA,precipIth,
				# IthPrecipGameFarm,MaxTempSyr,MinTempSyr,MaxTempIth,MinTempIth)
	# write(TodaysNOAAData, file="NOAA_NRCCData.csv", ncolumns= 8, append=TRUE, sep=",")






