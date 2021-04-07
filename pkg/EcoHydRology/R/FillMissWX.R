FillMissWX <-
function(declat, declon,StnRadius,date_min,date_max){
stns=meteo_distance(
  station_data=ghcnd_stations(),
  lat=declat,
  long=declon,
  units = "deg",
  radius = StnRadius,
  limit = NULL
)
ustns=unique(data.frame(id=stns$id,distance=stns$distance)[c("id", "distance")])
WXStn=ustns[order(ustns$distance),]$id[1]
WXDistance=ustns[order(ustns$distance),]$distance[1]
modeldata=meteo_pull_monitors(
  monitors=WXStn,
  keep_flags = FALSE,
  date_min = date_min,
  date_max = date_max,
  var = c("TMAX","TMIN","PRCP")
)
modeldata$tmaxid=WXStn
modeldata$tminid=WXStn
modeldata$prcpid=WXStn
modeldata$tmaxDis=WXDistance
modeldata$tminDis=WXDistance
modeldata$prcpDis=WXDistance
for (i in 2:length(ustns[,1])){
  WXStn=ustns[order(ustns$distance),]$id[i]
  WXDistance=ustns[order(ustns$distance),]$distance[i]
  WXData=try(meteo_pull_monitors(
    monitors=WXStn,
    keep_flags = FALSE,
    date_min = date_min,
    date_max = date_max,
    var = c("TMAX","TMIN","PRCP")
  ))
  if (length(WXData$id)>5){
    modeldata=merge(modeldata,WXData,by.x=c("date"),by.y=c("date"),all = T)
    if("prcp.y" %in% colnames(modeldata)){
     
      modeldata$prcpid[is.na(modeldata$prcp.x)]=WXStn
      modeldata$prcpDis[is.na(modeldata$prcp.x)]=WXDistance
      modeldata$prcp.x[is.na(modeldata$prcp.x)]=modeldata$prcp.y[is.na(modeldata$prcp.x)]
      modeldata=subset(modeldata,select = -c(prcp.y))
    }
    if("tmin.y" %in% colnames(modeldata)){
     
      modeldata$tminid[is.na(modeldata$tmin.x)]=WXStn
      modeldata$tminDis[is.na(modeldata$tmin.x)]=WXDistance
      modeldata$tmin.x[is.na(modeldata$tmin.x)]=modeldata$tmin.y[is.na(modeldata$tmin.x)]
      modeldata=subset(modeldata,select = -c(tmin.y))
    }
    if("tmax.y" %in% colnames(modeldata)){
     
      modeldata$tmaxid[is.na(modeldata$tmax.x)]=WXStn
      modeldata$tmaxDis[is.na(modeldata$tmax.x)]=WXDistance
      modeldata$tmax.x[is.na(modeldata$tmax.x)]=modeldata$tmax.y[is.na(modeldata$tmax.x)]
      modeldata=subset(modeldata,select = -c(tmax.y))
    } 
    modeldata=subset(modeldata,select = -c(id.y))
    #View(modeldata3)
    colnames(modeldata)[2]="id"
    colnames(modeldata)[which(colnames(modeldata)=="prcp.x")]="prcp"
    colnames(modeldata)[which(colnames(modeldata)=="tmax.x")]="tmax"
    colnames(modeldata)[which(colnames(modeldata)=="tmin.x")]="tmin"

    #if ((sum(is.na(modeldata$prcp)))<5&(sum(is.na(modeldata$tmax)))<5){
    #  stop("Enough")
    #}
  } 
}
modeldata$MaxTemp=as.numeric(modeldata$tmax)/10# Converting to C
modeldata$MinTemp=as.numeric(modeldata$tmin)/10 # Converting to C
modeldata$P=as.numeric(modeldata$prcp)/10 # Converting to mm
return(modeldata)
}
