##  Getting data from NRCC
#  This should work for Dec 2000 forward (data is formatted differently 1995-2000)
#  (But probably can get it to work earlier)
#	questions:  contact Jo Archibald, jaa78@cornell.edu

######################################################################################################################
###		Online precip and temp data from nrcc
######################################################################################################################
library(XML)

GetIthacaData <- function(startMonth=12, startYear=2001, EndDate=strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")){
	
	EndDate <- as.POSIXlt(as.Date(EndDate))
	noMonths<-12*(1900+EndDate$year-startYear) + EndDate$mon+1 - startMonth		## Gets the number of months since the data set was available until present (or end date)
	month<-c()
	year<-c()
	days<-c()
	precip<-vector(length=0)
	snow<-vector(length=0)
	TempMX<-vector(length=0)
	TempMN<-vector(length=0)
	SnowDepth<-c()
	dates<-c()
	for(i in 1:(noMonths+1)){
		CurMonth 	<- i+startMonth - 12*floor((i+startMonth-2)/12)-1	## gives what month it is
		CurYear 	<- startYear+floor((i+startMonth-2)/12)
		
		if(CurMonth < 10) zM <- "0" else zM <- ""
		if((CurYear-2000)<10 & CurYear >= 2000) zY<-"0" else zY <- ""
		if (CurYear >= 2000){
		CurURL <- paste("http://www.nrcc.cornell.edu/climate/ithaca/moncrt_", zM, CurMonth,"-",zY,(CurYear-2000), ".html", sep="")
		} else CurURL <- paste("http://www.nrcc.cornell.edu/climate/ithaca/moncrt_", zM, CurMonth,"-",(CurYear-1900), ".html", sep="")
		if (CurMonth == strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")$mon+1 & CurYear == strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")$year+1900) CurURL<- "http://www.nrcc.cornell.edu/climate/ithaca/moncrt.html"  #Current month has a different url
		
		IthW 	<- readHTMLTable(CurURL)[[1]]
		# IthW <- read.delim(CurURL, skip=10)	 # Nov 2000 and before can't use HTMLTable command...
		day 	<- as.numeric(as.character(IthW[,1]))
		day 	<- day[-which(is.na(day))]
		monthLen <- length(day)
		month 	<- c(month,rep(CurMonth, times=monthLen))
		year 	<- c(year,rep(CurYear, times=monthLen))
		
		if (CurYear > 2001 | (CurYear == 2001 & CurMonth == 12)) {  # only 10 columns after Dec 2001
			precip 	<- suppressWarnings(append(precip,as.numeric(as.character(IthW[1:monthLen,8]))))
			snow 	<- suppressWarnings(append(snow,as.numeric(as.character(IthW[1:monthLen,9]))))
			SnowDepth 	<- suppressWarnings(append(SnowDepth,as.numeric(as.character(IthW[1:monthLen,10]))))
		} else {
			precip 	<- suppressWarnings(append(precip,as.numeric(as.character(IthW[1:monthLen,9]))))
			snow 	<- suppressWarnings(append(snow,as.numeric(as.character(IthW[1:monthLen,10]))))
			SnowDepth 	<- suppressWarnings(append(SnowDepth,as.numeric(as.character(IthW[1:monthLen,11]))))
		}
		
		TempMX 	<- append(TempMX,as.numeric(as.character(IthW[1:monthLen,2])))
		TempMN 	<- append(TempMN,as.numeric(as.character(IthW[1:monthLen,3])))
		
		zD 		<- rep("",times=monthLen)
		zD[which(day<10)] <- "0"
		dates 	<- c(dates,paste(CurYear, "-", zM, CurMonth, "-", zD,day, sep=""))
		days 	<- c(days,day)
	}
	dates<-as.Date(strptime(dates, format="%Y-%m-%d"))
	precip[which(is.na(precip))]<-0.001  ## These are trace amounts, approximating as 0.001
	snow[which(is.na(snow))]<-0.001 ## These are trace amounts, approximating as 0.001
	SnowDepth[which(is.na(SnowDepth))]<-0.001 ## These are trace amounts, approximating as 0.001
	IthWeatherData<-data.frame(dates,precip*25.4,snow*25.4,(TempMX-32)*5/9,(TempMN-32)*5/9,SnowDepth*25.4) ## convert to metric system
	colnames(IthWeatherData)<-c("Date", "P_mm", "snow_mm", "Tmax_C", "Tmin_C", "snowDepth_mm") 
	return(IthWeatherData)
}

##########################################################################################################################
# examples :  1. 	IthWeath10 <- GetIthacaData(startMonth=1, startYear=2010, EndDate=strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"))
# 2. 				IthWeath10 <- GetIthacaData(startMonth=12, startYear=2001, EndDate="2013-01-31")
# 	IthSM <- SnowMelt(Date=IthWeath10$Date, IthWeath10$P_mm, IthWeath10$Tmax_C, IthWeath10$Tmin_C, lat_deg = 42.44)
#	NSeff(IthWeath10$snowDepth_mm,IthSM$SnowDepth_m * 1000)
#	plot(IthWeath10$Date,IthWeath10$snowDepth_mm)
#	lines(IthWeath10$Date,IthSM$SnowDepth_m * 1000, col="blue")
		