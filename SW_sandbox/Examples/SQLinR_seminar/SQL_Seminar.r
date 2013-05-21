
library(sqldf)


### Loading our Data
FileList<-read.table("FileList.csv", header=T, sep=",", fill=T)
for(i in 1:nrow(FileList)) assign(as.character(FileList[i,1]),read.table(as.character(FileList[i,2]), header=T, sep=",", fill=T),envir=.GlobalEnv)


### Viewing a Few Fields from 1 Table
Locations2<-sqldf("Select Location_ID, Lat, Long from Locations")
head(Locations2)

### Viewing a All Fields from 1 Table
Locations3<-sqldf("Select * from Locations")
head(Locations3)

### Viewing a Few Fields from 1 Table with an Alias
Locations4<-sqldf("Select Location_ID AS SampleSite, Lat, Long from Locations")
head(Locations4)

### Viewing a Few Fields from 2 Tables
WaterDepth<-sqldf("Select Lat, Long, Date, WaterDepth, LandCover from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID")
head(WaterDepth)

WaterDepth2<-sqldf("Select Lat, Long, Date, WaterDepth, LandCover from Locations, FieldData Where Locations.Location_ID = FieldData.Location_ID")
head(WaterDepth2)

### Viewing a Few Fields from 2 Tables with a Condition
WaterDepthLandCover<-sqldf("Select Lat, Long, Date, WaterDepth, LandCover from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID Where Locations.LandCover='Wetland'")
head(WaterDepthLandCover)

### Viewing a Few Fields from 2 Tables with a Condition & Ordering
WaterDepthLandCoverOrder<-sqldf("Select Lat, Long, Date, WaterDepth, LandCover from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID Where Locations.LandCover='Wetland' Order By WaterDepth")
head(WaterDepthLandCoverOrder)

	#adding na.omit
	WaterDepthLandCoverOrder<-na.omit(sqldf("Select Lat, Long, Date, WaterDepth, LandCover from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID Where Locations.LandCover='Wetland' Order By WaterDepth DESC"))
	head(WaterDepthLandCoverOrder)

	#adding a 2nd condition
	WaterDepthLandCoverOrder<-na.omit(sqldf("Select Lat, Long, Date, WaterDepth, LandCover from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID Where Locations.LandCover='Wetland' and FieldData.WaterDepth>0 Order By WaterDepth"))
	head(WaterDepthLandCoverOrder)

	#adding a 2nd condition, DESCing sort
	WaterDepthLandCoverOrder2<-na.omit(sqldf("Select Lat, Long, Date, WaterDepth, LandCover from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID Where Locations.LandCover='Wetland' and FieldData.WaterDepth>0 Order By WaterDepth DESC"))
	head(WaterDepthLandCoverOrder2)

### Viewing a Few Fields from 2 Tables with a Condition & Grouping
WaterDepthLandCoverGroup<-sqldf("Select AVG(WaterDepth), LandCover from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID Group By LandCover Order By WaterDepth")
head(WaterDepthLandCoverGroup)

### Viewing a Few Fields from 2 Tables with a Condition & Grouping by 2 Fields
WaterDepthLandCoverGroup2<-sqldf("Select AVG(WaterDepth), LandCover, Date from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID Group By LandCover, Date Order By WaterDepth")
head(WaterDepthLandCoverGroup2)

WaterDepthLandCoverGroup2<-na.omit(sqldf("Select AVG(WaterDepth), LandCover, Date from Locations Inner Join FieldData on Locations.Location_ID = FieldData.Location_ID Group By LandCover, Date Order By WaterDepth"))
head(WaterDepthLandCoverGroup2)

### Viewing a Few Fields from Multiple Tables
AnionData1<-sqldf("select FieldData.Date, FieldData.Location_ID, AVG(NO3N) AS NO3N,AVG(NO2N) AS NO2N,AVG(SO4) AS SO4,AVG(Cl) AS Cl from ((Samples Inner Join FieldData on Samples.FieldData_ID=FieldData.FieldData_ID) Inner Join Results on Samples.Sample_ID=Results.Sample_ID) Group By FieldData.Date, Location_ID")
head(AnionData1)

## Multiple Tables with Condional
AnionData2<-sqldf("select FieldData.Date, FieldData.Location_ID, WaterDepth, AVG(NO3N) AS NO3N,AVG(NO2N) AS NO2N,AVG(SO4) AS SO4,AVG(Cl) AS Cl from ((Samples Inner Join FieldData on Samples.FieldData_ID=FieldData.FieldData_ID) Inner Join Results on Samples.Sample_ID=Results.Sample_ID)  Where FieldData.WaterDepth <.5 Group By FieldData.Date, Location_ID, WaterDepth Order By WaterDepth DESC")
head(AnionData2)

## "Paste" in the SQL Statement
DepthCutOff<-.5
AnionData2<-sqldf(paste("select FieldData.Date, FieldData.Location_ID, WaterDepth, AVG(NO3N) AS NO3N,AVG(NO2N) AS NO2N,AVG(SO4) AS SO4,AVG(Cl) AS Cl from ((Samples Inner Join FieldData on Samples.FieldData_ID=FieldData.FieldData_ID) Inner Join Results on Samples.Sample_ID=Results.Sample_ID)  Where FieldData.WaterDepth <",DepthCutOff," Group By FieldData.Date, Location_ID, WaterDepth Order By WaterDepth DESC",sep=""))
head(AnionData2)

##Outer Joins
FieldSamples<-sqldf("Select Location_ID, FieldData.Date, Sample_ID, WaterDepth, Type from FieldData Inner Join Samples on FieldData.FieldData_ID=Samples.FieldData_ID")
head(FieldSamples)
nrow(FieldSamples)

FieldSamples<-sqldf("Select Location_ID, FieldData.Date, Sample_ID, WaterDepth, Type from FieldData Left Join Samples on FieldData.FieldData_ID=Samples.FieldData_ID")
head(FieldSamples)
nrow(FieldSamples)

