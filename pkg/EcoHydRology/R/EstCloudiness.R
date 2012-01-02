EstCloudiness <-
function(lat,Jday,Tx,Tn){
# estimates the cloudiness of the atmosphere by scaling to atmospheric transmissivity

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]
trans<-transmissivity(lat,Jday,Tx,Tn)
cloudiness<-1-(trans-0.15)/(0.75-0.15)
cloudiness[which(cloudiness>1)]<-1
cloudiness[which(cloudiness<0)]<-0
return(cloudiness)		
}

