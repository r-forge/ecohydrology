build_wgn_file=function(metdata_df=WXData,declat=flowgage$declat,declon=flowgage$declon){
  wgdata=data.frame(matrix(nrow = 14,ncol = 12),row.names = c("tmpmx","tmpmn","tmpstdmx","tmpstdmn","pcpmm","pcpstd","pcpskw","pr_wd","pr_ww","pcpd","rainhhmx","solarav","dewpt","wndav"))
  colnames(wgdata)<-unique(months(metdata_df$date))
  metdata_df$Ppost  <- append(metdata_df$P, 0, 0)[-nrow(metdata_df)]
  metdata_df$wnd=4
  years=length(unique(year(metdata_df$date)))
  
  metdata_df$MaxTemp[is.na(metdata_df$MaxTemp)]=metdata_df$MinTemp[is.na(metdata_df$MaxTemp)] +1
  metdata_df$MinTemp[is.na(metdata_df$MinTemp)]=metdata_df$MaxTemp[is.na(metdata_df$MinTemp)] -1
  cleanup=(metdata_df$MaxTemp<metdata_df$MinTemp)
  cleanup[is.na(cleanup)]=FALSE
  metdata_df$MaxTemp[cleanup]=metdata_df$MinTemp[cleanup]+1
  metdata_df$dewpt=metdata_df$MinTemp
  metdata_df$Solar=NA
  metdata_df$Solar[!is.na(metdata_df$MaxTemp)]=Solar(lat=declat/180*pi,
                                                     Jday=julian(metdata_df$date[!is.na(metdata_df$MaxTemp)],
                                                                 origin=as.Date("2000-01-01")),
                                                     Tx=metdata_df$MaxTemp[!is.na(metdata_df$MaxTemp)],
                                                     Tn=metdata_df$MinTemp[!is.na(metdata_df$MaxTemp)])/1000
  for (i in unique(months(metdata_df$date))){
    wgdata["tmpmx",i]=mean(metdata_df$MaxTemp[months(metdata_df$date)==i],na.rm=T)
    wgdata["tmpmn",i]=mean(metdata_df$MinTemp[months(metdata_df$date)==i],na.rm=T)
    wgdata["tmpstdmx",i]=sd(metdata_df$MaxTemp[months(metdata_df$date)==i],na.rm=T)
    wgdata["tmpstdmn",i]=sd(metdata_df$MinTemp[months(metdata_df$date)==i],na.rm=T)
    wgdata["pcpmm",i]=mean(metdata_df$P[months(metdata_df$date)==i]*30,na.rm=T)
    wgdata["pcpstd",i]=sd(metdata_df$P[months(metdata_df$date)==i],na.rm=T)
    wgdata["pcpskw",i]=skewness(metdata_df$P[months(metdata_df$date)==i],na.rm=T)
    wgdata["pr_wd",i]=length(metdata_df$P[months(metdata_df$date)==i &
                                            metdata_df$P<1 & metdata_df$Ppost >
                                            1])/length((metdata_df$P[months(metdata_df$date)==i ]))
    wgdata["pr_ww",i]=length(metdata_df$P[months(metdata_df$date)==i &
                                            metdata_df$P>1 & metdata_df$Ppost >
                                            1])/length((metdata_df$P[months(metdata_df$date)==i]))
    wgdata["pcpd",i]=length(metdata_df$P[months(metdata_df$date)==i &
                                           metdata_df$P>1])/years
    wgdata["rainhhmx",i]=max(metdata_df$P[months(metdata_df$date)==i],na.rm=T)/4
    wgdata["solarav",i]=mean(metdata_df$Solar[months(metdata_df$date)==i],na.rm=T)
    wgdata["dewpt",i]=mean(metdata_df$dewpt[months(metdata_df$date)==i],na.rm=T)
    wgdata["wndav",i]=mean(metdata_df$wnd[months(metdata_df$date)==i],na.rm=T)
  }
  #outfile=paste(args[4],"/wgn.df",sep="")
  #write.table(wgdata,file=outfile)
  header=paste0("This line is not used\n  LATITUDE =",sprintf("%7.2f",declat)," LONGITUDE =",sprintf("%7.2f",declon),"
  ELEV [m] =",sprintf("%7.2f",mean(WXData$prcpElevation,na.rm=T)),"
  RAIN_YRS =  10.00
")
  outfile=paste("000010000.wgn",sep="")
  cat(header,file=outfile)
  for (i in seq(1:14)){
    if(i!=5){cat(sprintf("%6.2f",wgdata[i,]),file=outfile,append=T,sep="")
    }else{
      cat(sprintf("%6.1f",wgdata[i,]),file=outfile,append=T,sep="")
    }
    cat("\n",file=outfile,append=T,sep="")
  }
  for (filename in list.files(pattern = "wgn")){
    try(file.copy("000010000.wgn",filename,overwrite = T),silent=T)
  }
  
}

