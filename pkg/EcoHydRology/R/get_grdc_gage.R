#### Functions
get_grdc_gage=function(filename=filename){  
  # A function to make a data object similar to EcoHydrology::get_usgs_gage  
  #filename="1577050_Q_Day.Cmd.txt"
  print(filename)
  nskipline = grep("YYYY-MM-DD", readLines(filename))[2]-1
  gaugeno <- strsplit(filename, '[.]')[[1]][1]
  gaugetab <- cbind(fread(filename, header = T, skip = nskipline, sep=";",colClasses = c('character', 'character', 'numeric')), GRDC_Info = gaugeno)%>%
    setnames('YYYY-MM-DD', 'dates') %>%
    setorder(GRDC_Info, dates)
  gaugetab$dates=as.Date(gaugetab$dates)
  if(length(gaugetab$dates)<100){return("not enough data")}
  # GRDC-No.:              1577050"     
  nskipline = grep("GRDC-No", readLines(filename))[1]-1
  GRDC_No=as.numeric(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2])
  # River:                 HOLETA SHET'"                                                                          
  nskipline = grep("River:", readLines(filename))[1]-1
  GRDC_River=str_trim(as.character(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2]))
  # Station:               NEAR HOLETTA"                                                                          
  nskipline = grep("Station:", readLines(filename))[1]-1
  GRDC_Station=str_trim(as.character(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2]))
  # Country:               ET"                                                                                    
  nskipline = grep("Country:", readLines(filename))[1]-1
  GRDC_Country=str_trim(as.character(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2]))
  # Latitude (DD):       9.08"                                                                                    
  nskipline = grep("Latitude", readLines(filename))[1]-1
  GRDC_Latitude=as.numeric(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2])
  # Longitude (DD):      38.52"                                                                                   
  nskipline = grep("Longitude", readLines(filename))[1]-1
  GRDC_Longitude=as.numeric(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2])
  # Catchment area (km\xb2):      119.0"                                                                          
  nskipline = grep("Catchment", gsub("\xb2","",readLines(filename)))[1]-1
  GRDC_Catchment_area=as.numeric(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2])
  # Altitude (m ASL):        1860.0"                                                                              
  nskipline = grep("Altitude", readLines(filename))[1]-1
  GRDC_Altitude=as.numeric(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2])
  # Next downstream station:      1577600"           
  nskipline = grep("Next downstream station:", readLines(filename))[1]-1
  GRDC_Next_downstream_station=as.numeric(strsplit(read_lines(filename,n_max = 1,skip=nskipline),":")[[1]][2])
  
  gaugetab=gaugetab[!(Value %in% c(-999, -99, -9999, 99, 999, 9999))]
  
  plot(gaugetab$dates,gaugetab$Value,main=filename)
  GRDC_mindate=min(gaugetab$dates)
  GRDC_maxdate=max(gaugetab$dates)
  
  flowgage_id="04216500"
  flowgage=get_usgs_gage(flowgage_id) # Just grabbing a station as a template
  flowgage$id=as.character(GRDC_No)
  flowgage$declat=GRDC_Latitude
  flowgage$declon=GRDC_Longitude
  flowgage$elev=GRDC_Altitude
  flowgage$area=GRDC_Catchment_area
  flowgage$gagename=GRDC_Station
  flowgage$River=GRDC_River
  
  # Building flowdata df in flowgage object to match EcoHydrology::get_usgs_gage
  flowgage$flowdata=gaugetab
  flowgage$flowdata$agency="GRDC"
  flowgage$flowdata$site_no=GRDC_No
  flowgage$flowdata$date=flowgage$flowdata$dates
  flowgage$flowdata$mdate=flowgage$flowdata$dates
  flowgage$flowdata$flow=flowgage$flowdata$Value   #flow is in cubic meters per second
  flowgage$flowdata$Qm3ps=flowgage$flowdata$Value   #flow is in cubic meters per second
  return(flowgage)
}

