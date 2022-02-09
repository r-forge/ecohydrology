FillMissWX=function(declat, declon,StnRadius=30,minstns=10,date_min,date_max,targElev=1,method="IDW",alfa=2,printinto="png"){
  print(paste(date_min,date_max,StnRadius,targElev))
  station_data=ghcnd_stations()
  while(StnRadius<2000){
    print(StnRadius)
    stns=meteo_distance(
      station_data=station_data,
      lat=declat, long=declon,
      units = "deg",
      radius = StnRadius,
      limit = NULL
    )
    if(length(unique(stns$id))>minstns){break()}
    StnRadius=StnRadius*1.414
  }
  ustns=unique(data.frame(id=stns$id,distance=stns$distance,elevation=stns$elevation)[c("id", "distance","elevation")])
  ustns$idNum=substring(ustns$id, 3,3)
  #ustns$idNum=as.vector(ustns$idNum)
  ustns$source=NA
  #ustns$source=as.character(ustns$source)
  for(k in 1:nrow(ustns)){
    if(ustns$idNum[k]=="0"){
      ustns$source[k]="unspecified"
    }
    if (ustns$idNum[k]=="1"){
      ustns$source[k]="CoCoRaHS" 
    }
    if (ustns$idNum[k]=="C"){
      ustns$source[k]="COOP" 
    }
    if (ustns$idNum[k]=="E"){
      ustns$source[k]="ECA&D" 
    }
    if (ustns$idNum[k]=="M"){
      ustns$source[k]="WMOID" 
    }
    if (ustns$idNum[k]=="N"){
      ustns$source[k]="NM/HC" 
    }  
    
    if (ustns$idNum[k]=="R"){
      ustns$source[k]="RAWS" 
    }  
    if (ustns$idNum[k]=="S"){
      ustns$source[k]="SNOTEL" 
    }
    if (ustns$idNum[k]=="W"){
      ustns$source[k]="WBAN" 
    }
  }
  
  WXStn=ustns[order(ustns$distance),]$id[1]
  WXDistance=ustns[order(ustns$distance),]$distance[1]
  WXElev=ustns$elevation[1]
  WXSource=ustns$source[1]
  modeldata=meteo_pull_monitors(
    monitors=WXStn,
    keep_flags = FALSE,
    date_min = date_min,
    date_max = date_max,
    var = c("TMAX","TMIN","PRCP")
  )
  if(!("prcp" %in% colnames(modeldata))){
    modeldata$prcp=NA
    
  }
  if(!("tmax" %in% colnames(modeldata))){
    modeldata$tmax=NA
    
  } 
  if(!("tmin" %in% colnames(modeldata))){
    modeldata$tmin=NA
  }
  modeldata$tmaxid=WXStn
  modeldata$tminid=WXStn
  modeldata$prcpid=WXStn
  modeldata$tmaxDis=WXDistance
  modeldata$tminDis=WXDistance
  modeldata$prcpDis=WXDistance
  modeldata$prcpElev=WXElev
  modeldata$tmaxElev=WXElev
  modeldata$tminElev=WXElev
  modeldata$PrcSource=WXSource
  modeldata$tmaxSource=WXSource
  modeldata$tminSource=WXSource
  #####Base plot(distance vs date) func for both IDW nd Closest#
  WXDataplot=modeldata
  
  if (method == "IDW" || method == "IDEW"){
    #################################################
    if(method == "IDW"){
      PrcpAdjFac=matrix(data = 0, nrow = 1, ncol = 12)
      TempLapsRate=matrix(data = 0, nrow = 1, ncol = 12)
    }
    if(method == "IDEW") {
      PrcpAdjFac=matrix(c(0.00035,0.00035,0.00035,0.00030,0.00025,0.00020,0.0002,0.0002,0.0002,0.00025,0.00030,0.00035))# m^-1
      TempLapsRate=matrix(c(0.0044,0.0059,0.0071,0.0078,0.0081,0.0082,0.0081,0.0081,0.0077,0.0068,.00055,0.0047)) # C m^-1
    }
    modeldata$DenPrcp=0;modeldata$DenTmin=0;modeldata$DenTmax=0
    if("prcp" %in% colnames(modeldata)){
      
      modeldata$prcp=modeldata$prcp*(1/(modeldata$prcpDis^alfa))*abs(((1+(PrcpAdjFac[month(modeldata$date)]*(targElev-WXElev)))/(1-(PrcpAdjFac[month(modeldata$date)]*(targElev-WXElev)))))
      modeldata$DenPrcp[!is.na(modeldata$prcp)]=(1/(modeldata$prcpDis^alfa))
      modeldata$prcpElev[is.na(modeldata$prcp)]=NA
      modeldata$prcpElev[!is.na(modeldata$prcp)]=WXElev*(1/(modeldata$prcpDis^alfa))
      
    } else {
      modeldata$prcp=NA
    }
    if("tmax" %in% colnames(modeldata)){
      modeldata$tmax=(modeldata$tmax-(TempLapsRate[month(modeldata$date)]*(targElev-WXElev)))*(1/(modeldata$tmaxDis^alfa))
      modeldata$DenTmax[!is.na(modeldata$tmax)]=(1/(modeldata$tmaxDis^alfa))
      modeldata$tmaxElev[is.na(modeldata$tmax)]=NA
      modeldata$tmaxElev[!is.na(modeldata$tmax)]=WXElev*(1/(modeldata$tmaxDis^alfa))
      
    } else {
      modeldata$tmax=NA
    }
    
    if("tmin" %in% colnames(modeldata)){
      modeldata$tmin=(modeldata$tmin-(TempLapsRate[month(modeldata$date)]*(targElev-WXElev)))*(1/(modeldata$tminDis^alfa))
      modeldata$DenTmin[!is.na(modeldata$tmin)]=(1/(modeldata$tminDis^alfa))
      modeldata$tminElev[is.na(modeldata$tmin)]=NA
      modeldata$tminElev[!is.na(modeldata$tmin)]=WXElev*(1/(modeldata$tminDis^alfa))
    } else{
      modeldata$tmin=NA
    }
    ################################################
    for (i in 2:length(ustns[,1])){
      WXStn=ustns[order(ustns$distance),]$id[i]
      WXDistance=ustns[order(ustns$distance),]$distance[i]
      WXElev=ustns$elevation[i]
      WXSource=ustns$source[i]
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
          modeldata$DenPrcp[is.na(modeldata$DenPrcp)]=0
          modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$DenPrcp[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+(1/(WXDistance^alfa))
          modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$DenPrcp[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]+(1/(WXDistance^alfa))
          modeldata$prcpElev[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$prcpElev[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+WXElev*(1/(WXDistance^alfa))
          modeldata$prcpElev[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=WXElev*(1/(WXDistance^alfa))
          
          modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]=modeldata$prcp.x[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]+
            modeldata$prcp.y[!is.na(modeldata$prcp.y) & !is.na(modeldata$prcp.x)]*(1/(WXDistance^alfa))*abs(((1+(PrcpAdjFac[month(modeldata$date)]*(targElev-WXElev)))/(1-(PrcpAdjFac[month(modeldata$date)]*(targElev-WXElev)))))
          modeldata$prcp.x[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]=modeldata$prcp.y[is.na(modeldata$prcp.x) & !is.na(modeldata$prcp.y)]*(1/(WXDistance^alfa))*abs(((1+(PrcpAdjFac[month(modeldata$date)]*(targElev-WXElev)))/(1-(PrcpAdjFac[month(modeldata$date)]*(targElev-WXElev)))))
          modeldata=subset(modeldata,select = -c(prcp.y))
        }
        if("tmax.y" %in% colnames(modeldata)){
          modeldata$DenTmax[is.na(modeldata$DenTmax)]=0
          modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$DenTmax[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]+(1/(WXDistance^alfa))
          modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=modeldata$DenTmax[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]+(1/(WXDistance^alfa))      
          modeldata$tmaxElev[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$tmaxElev[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]+WXElev*(1/(WXDistance^alfa))
          modeldata$tmaxElev[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=WXElev*(1/(WXDistance^alfa))         
          
          modeldata$tmax.x[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]=modeldata$tmax.x[!is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)] +
            (modeldata$tmax.y[!is.na(modeldata$tmax.y) & !is.na(modeldata$tmax.x)]-(TempLapsRate[month(modeldata$date)]*(targElev-WXElev)))*(1/(WXDistance^alfa))
          modeldata$tmax.x[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]=(modeldata$tmax.y[is.na(modeldata$tmax.x) & !is.na(modeldata$tmax.y)]-(TempLapsRate[month(modeldata$date)]*(targElev-WXElev)))*(1/(WXDistance^alfa))
          
          
          modeldata=subset(modeldata,select = -c(tmax.y))
        }
        if("tmin.y" %in% colnames(modeldata)){      
          modeldata$DenTmin[is.na(modeldata$DenTmin)]=0
          modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$DenTmin[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]+(1/(WXDistance^alfa))
          modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=modeldata$DenTmin[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+(1/(WXDistance^alfa))
          modeldata$tminElev[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$tminElev[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]+WXElev*(1/(WXDistance^alfa))
          modeldata$tminElev[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=WXElev*(1/(WXDistance^alfa))             
          
          
          modeldata$tmin.x[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]=modeldata$tmin.x[!is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]+
            (modeldata$tmin.y[!is.na(modeldata$tmin.y) & !is.na(modeldata$tmin.x)]-(TempLapsRate[month(modeldata$date)]*(targElev-WXElev)))*(1/(WXDistance^alfa))
          modeldata$tmin.x[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]=(modeldata$tmin.y[is.na(modeldata$tmin.x) & !is.na(modeldata$tmin.y)]-(TempLapsRate[month(modeldata$date)]*(targElev-WXElev)))*(1/(WXDistance^alfa))
          
          modeldata=subset(modeldata,select = -c(tmin.y))
        }
        
        modeldata=subset(modeldata,select = -c(id.y))
        colnames(modeldata)[2]="id"
        colnames(modeldata)[which(colnames(modeldata)=="prcp.x")]="prcp"
        colnames(modeldata)[which(colnames(modeldata)=="tmax.x")]="tmax"
        colnames(modeldata)[which(colnames(modeldata)=="tmin.x")]="tmin"
      } 
    }
    modeldata$MaxTemp=as.numeric(modeldata$tmax)/10/modeldata$DenTmax# Converting to C
    modeldata$MinTemp=as.numeric(modeldata$tmin)/10/modeldata$DenTmin# Converting to C
    modeldata$P=as.numeric(modeldata$prcp)/10/modeldata$DenPrcp # Converting to mm
    modeldata$prcpElevation=modeldata$prcpElev/modeldata$DenPrcp ###elevation of prcp station
    modeldata$tmaxElevation=modeldata$tmaxElev/modeldata$DenTmax
    modeldata$tminElevation=modeldata$tminElev/modeldata$DenTmin
    WXDataplot1=merge(modeldata, WXDataplot, by="date")
    WXDataplot1$weightedP=(1/WXDataplot1$prcpDis.y^alfa)/WXDataplot1$DenPrcp
    WXDataplot1$weightedTMX=(1/WXDataplot1$tmaxDis.y^alfa)/WXDataplot1$DenTmax
    WXDataplot1$weightedTMN=(1/WXDataplot1$tminDis.y^alfa)/WXDataplot1$DenTmin
    
    #WXDataplot1$weightedTMX=((1/WXDataplot1$tmaxDis.y^alfa))/WXDataplot1$DenTmax
    prcp_plot=ggplot(WXDataplot, aes(x=date,y=prcpDis, col=PrcSource))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      ylim(0,StnRadius)+
      labs(title = 'Precipitation', x = 'Date', y = 'Distance (km)')+
      theme(legend.title = element_blank())
    
    
    ####################tmax plot
    tmax_plot=ggplot(WXDataplot, aes(x=date,y=tmaxDis, col=tmaxSource))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      ylim(0,StnRadius)+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Distance (km)')+
      theme(legend.title = element_blank())
    
    ####################################tmin plot
    tmin_plot=ggplot(WXDataplot, aes(x=date,y=tminDis, col=tminSource))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      ylim(0,StnRadius)+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Distance (km)')+
      theme(legend.title = element_blank())
    
    
    prcp_plotweight1=ggplot(data = WXDataplot1 ,aes(x=date,y=weightedP, col=PrcSource.y))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      labs(title = 'Precipitation', x = 'Date', y = 'Weight of the station')+
      theme(legend.title = element_blank())
    ####################tmax plot
    tmax_plotweight1=ggplot(WXDataplot1, aes(x=date,y=weightedTMX, col=tmaxSource.y))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Weight of the station')+
      theme(legend.title = element_blank())
    ####################################tmin plot
    
    tmin_plotweight1=ggplot(WXDataplot1, aes(x=date,y=weightedTMN, col=tminSource.y))+
      geom_point(size=0.5)+
      xlim(as.Date(c(date_min,date_max)))+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Weight of the station')+
      theme(legend.title = element_blank())
    
    for (i in 2:length(ustns[,1])){
      WXStn=ustns[order(ustns$distance),]$id[i]
      WXDistance=ustns[order(ustns$distance),]$distance[i]
      WXElev=ustns$elevation[i]
      WXSource=ustns$source[i]
      WXDataplot=meteo_pull_monitors(
        monitors=WXStn,
        keep_flags = FALSE,
        date_min = date_min,
        date_max = date_max,
        var = c("TMAX","TMIN","PRCP")
      )
      
      WXDataplot$prcpid=WXStn
      WXDataplot$prcpDis=WXDistance
      WXDataplot$tmaxid=WXStn
      WXDataplot$tmaxDis=WXDistance
      WXDataplot$tminid=WXStn
      WXDataplot$tminDis=WXDistance
      WXDataplot$PrcSource=WXSource
      WXDataplot$tminSource=WXSource
      WXDataplot$tmaxSource=WXSource
      
      WXDataplot1=merge(modeldata, WXDataplot, by="date")
      
      if("prcp" %in% colnames(WXDataplot)){
        
        prcp_plotFinal=prcp_plot+geom_point(aes(x=date,y=prcpDis, col=PrcSource),data=WXDataplot,size=0.5)
        prcp_plot=prcp_plotFinal
        
        WXDataplot1$weightedP=(1/WXDataplot1$prcpDis.y^alfa)/WXDataplot1$DenPrcp 
        
        prcp_plotweight=prcp_plotweight1+geom_point(data = WXDataplot1 ,aes(x=date,y=weightedP, col=PrcSource.y),size=0.5)
        prcp_plotweight1=prcp_plotweight
        
      }
      
      if("tmax" %in% colnames(WXDataplot)){
        
        tmax_plotFinal=tmax_plot+geom_point(aes(x=date,y=tmaxDis, col=tmaxSource),data=WXDataplot,size=0.5)
        tmax_plot=tmax_plotFinal
        
        WXDataplot1$weightedTMX=(1/WXDataplot1$tmaxDis.y^alfa)/WXDataplot1$DenTmax
        tmax_plotweight=tmax_plotweight1+geom_point(data=WXDataplot1, aes(x=date,y=weightedTMX, col=tmaxSource.y),size=0.5)       
        tmax_plotweight1=tmax_plotweight
      }
      if("tmin" %in% colnames(WXDataplot)){
        
        tmin_plotFinal=tmin_plot+geom_point(aes(x=date,y=tminDis, col=tminSource),data=WXDataplot,size=0.5)
        tmin_plot=tmin_plotFinal
        WXDataplot1$weightedTMN=(1/WXDataplot1$tminDis.y^alfa)/WXDataplot1$DenTmin
        tmin_plotweight=tmin_plotweight1+geom_point(data=WXDataplot1, aes(x=date,y=weightedTMN, col=tminSource.y),size=0.5)       
        tmin_plotweight1=tmin_plotweight
      }  
    }
    setwd(getwd())
    if(printinto=="png"){
      png("prcpweight.png")
      print(prcp_plotweight1)
      dev.off()
      png("prcp.png")
      print(prcp_plot)
      dev.off()
      png("tmaxweight.png")
      print(tmax_plotweight1)
      dev.off()
      png("tmax.png")
      print(tmax_plot)
      dev.off()
      png("tminweight.png")
      print(tmin_plotweight1)
      dev.off()
      png("tmin.png")
      print(tmin_plot)
      dev.off()
    }
    if(printinto=="pdf"){
    pdf("IDWplots.pdf")
    print(prcp_plotweight1)
    print(prcp_plot)
    print(tmax_plotweight1)
    print(tmax_plot)
    print(tmin_plotweight1)
    print(tmin_plot)
    dev.off()
    }
    return(modeldata)
    
  }
  
  #############################################################################
  
  
  #################################################################################
  if(method == "closest"){
    for (i in 2:length(ustns[,1])){
      WXStn=ustns[order(ustns$distance),]$id[i]
      WXDistance=ustns[order(ustns$distance),]$distance[i]
      WXElev=ustns$elevation[i]
      WXSource=ustns$source[i]
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
          modeldata$prcpElev[is.na(modeldata$prcp.x)]=WXElev
          modeldata$PrcSource[is.na(modeldata$prcp.x)]=WXSource
          
          
          modeldata$prcp.x[is.na(modeldata$prcp.x)]=modeldata$prcp.y[is.na(modeldata$prcp.x)]
          modeldata=subset(modeldata,select = -c(prcp.y))
        }
        if("tmin.y" %in% colnames(modeldata)){
          
          modeldata$tminid[is.na(modeldata$tmin.x)]=WXStn
          modeldata$tminDis[is.na(modeldata$tmin.x)]=WXDistance
          modeldata$tminElev[is.na(modeldata$tmin.x)]=WXElev
          modeldata$tminSource[is.na(modeldata$tmin.x)]=WXSource
          
          modeldata$tmin.x[is.na(modeldata$tmin.x)]=modeldata$tmin.y[is.na(modeldata$tmin.x)]
          modeldata=subset(modeldata,select = -c(tmin.y))
        }
        if("tmax.y" %in% colnames(modeldata)){
          
          modeldata$tmaxid[is.na(modeldata$tmax.x)]=WXStn
          modeldata$tmaxDis[is.na(modeldata$tmax.x)]=WXDistance
          modeldata$tmaxElev[is.na(modeldata$tmax.x)]=WXElev
          modeldata$tmaxSource[is.na(modeldata$tmax.x)]=WXSource
          modeldata$tmax.x[is.na(modeldata$tmax.x)]=modeldata$tmax.y[is.na(modeldata$tmax.x)]
          modeldata=subset(modeldata,select = -c(tmax.y))
        } 
        modeldata=subset(modeldata,select = -c(id.y))
        colnames(modeldata)[2]="id"
        colnames(modeldata)[which(colnames(modeldata)=="prcp.x")]="prcp"
        colnames(modeldata)[which(colnames(modeldata)=="tmax.x")]="tmax"
        colnames(modeldata)[which(colnames(modeldata)=="tmin.x")]="tmin"
      } 
    }
    colnames(modeldata)[which(colnames(modeldata)=="prcpElev")]="prcpElevation"
    colnames(modeldata)[which(colnames(modeldata)=="tmaxElev")]="tmaxElevation"
    colnames(modeldata)[which(colnames(modeldata)=="tminElev")]="tminElevation"
    modeldata$MaxTemp=as.numeric(modeldata$tmax)/10# Converting to C
    modeldata$MinTemp=as.numeric(modeldata$tmin)/10 # Converting to C
    modeldata$P=as.numeric(modeldata$prcp)/10 # Converting to mm
    plot_prcp=ggplot(modeldata, aes(date,prcpDis, col=PrcSource))+
      geom_point()+
      expand_limits(y=0)+
      labs(title = 'Precipitation', x = 'Date', y = 'Distance of station from the outlet (km)')+
      theme(legend.title = element_blank())
    prcp_distance=ggplot(modeldata, aes(date,P, col=PrcSource))+
      geom_point()+
      labs(title = 'Precipitation', x = 'Date', y = 'Precipitation (mm)')+
      theme(legend.title = element_blank())
    ##############Tmax
    plot_tmax= ggplot(modeldata, aes(date,tmaxDis, col=tmaxSource))+
      geom_point()+
      expand_limits(y=0)+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Distance of station from the outlet (km)')+
      theme(legend.title = element_blank())
    tmax_distance=ggplot(modeldata, aes(date,MaxTemp, col=tmaxSource))+
      geom_point()+
      labs(title = 'Maximum Temperature', x = 'Date', y = 'Maximum Temperature (C)')+
      theme(legend.title = element_blank())
    ######MinTemp
    plot_tmin=ggplot(modeldata, aes(date,tminDis, col=tminSource))+
      geom_point()+
      expand_limits(y=0)+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Distance of station from the outlet (km)')+
      theme(legend.title = element_blank())
    tmin_distance= ggplot(modeldata, aes(date,MinTemp, col=tminSource))+
      geom_point()+
      labs(title = 'Minimum Temperature', x = 'Date', y = 'Minimum Temperature (C)')+
      theme(legend.title = element_blank())
   ################### 
    setwd(getwd())
    if(printinto=="pdf"){
    pdf("Closestplots.pdf")
    print(plot_prcp)
    print(prcp_distance)
    print(plot_tmax)
    print(tmax_distance)
    print(plot_tmin)
    print(tmin_distance)
    dev.off()
    }
    if(printinto=="png"){
      png("prcp.png")
      print(plot_prcp)
      dev.off()
      png("prcp_distance.png")
      print(prcp_distance)
      dev.off()
      png("tmax.png")
      print(plot_tmax)
      dev.off()
      png("tmax_distance.png")
      print(tmax_distance)
      dev.off()
      png("tmin.png")
      print(plot_tmin)
      dev.off()
      png("tmin_distance.png")
      print(tmin_distance)
      dev.off()
    }
    return(modeldata)
  }
}
