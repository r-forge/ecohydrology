transmissivity <-
function(lat,Jday,Tx,Tn){
# fraction of direct solar radiation passing through the atmosphere based on the Bristow-Campbell eqn

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]

# B constant based on work by Ndlvou
B<-rep(0.170*lat^-0.979, length(Jday))		#winter
B[which(Jday>80 & Jday<262)]<-0.282*lat^-0.431		#summer

#Potential solar radiation 30 days ago
PotSolar<-PotentialSolar(lat,(Jday-30))
PotSolar[which(Jday<31)]<-PotentialSolar(lat,(365-(30-Jday[which(Jday<31)])))

return(0.75*(1-exp(-B*(Tx-Tn)^2/(PotSolar/1000))))
}

