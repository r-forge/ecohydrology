readSWAT <-
function(outfile_type="rch",pathtofile="./"){
library(data.table)
headloc=c(sub=9,rch=9,hru=9,snu=2)
dataloc=c(sub=1,rch=1,hru=1,snu=2)
if(missing(outfile_type)){print(" 'outfile_type' is missing, should be rch, sub, or.. ask drf28 for other types")}
sfilename=paste(pathtofile,"/output.",outfile_type,sep="")
cfilename=paste0(pathtofile,"/file.cio")
nskipline=grep("NYSKIP",readLines(cfilename))
nskip=read.fortran(textConnection(readLines(cfilename)[nskipline]),"f20")
start_year = nskip + read.fortran(textConnection(readLines(cfilename)[9]), "f20")
icalenline=grep("ICALEN",readLines(cfilename))
if(length(icalenline) >0 ){
  icalen=read.fortran(textConnection(readLines(cfilename)[icalenline]),"f20")
}
myreadLines=function(fname) {
 s = file.info( fname )$size
 buf = readChar( fname, s, useBytes=T)
 strsplit( buf,"\n",fixed=T,useBytes=T)[[1]]
}
test = myreadLines(sfilename)
test=gsub("\r","",test)
headstr=test[headloc[outfile_type]]
headstr=gsub("TOT ([N,P])","TOT_\\1",headstr)
headstr=gsub("/L([A-Z])","/L \\1",headstr)
headstr=gsub("LAT Q","LAT_Q",headstr)
headstr=gsub(" mg/L","_mg/L",headstr)
headstr=gsub("WTAB ","WTAB_",headstr)
headstr=gsub("Mg/l","mg/l",headstr)
headstr=gsub("([A-Z])dgC","\\1C  ",headstr)
test[headloc[outfile_type]]=headstr
if(outfile_type=="rch" | outfile_type=="sub" | outfile_type=="snu"){substr(headstr, 1, 4) <- "INFO";test[headloc[outfile_type]]=headstr}
datastr=test[headloc[outfile_type]+dataloc[outfile_type]]
varnamestoplocs=unique(sort(c(unlist(gregexpr("[0-z] ",headstr)),unlist(gregexpr("[a-z][A-Z]",headstr)),unlist(gregexpr(")[A-Z]",headstr)),nchar(headstr))))
varnamestoplocs=varnamestoplocs[varnamestoplocs>0]
varnamestartlocs=c(1,varnamestoplocs[1:(length(varnamestoplocs)-1)]+1)
datastoplocs=unique(sort(c(unlist(regexpr("\\.[0-9]{5}E",datastr))-1,unlist(gregexpr("[0-Z] ",datastr)),nchar(datastr))))
datastoplocs=datastoplocs[datastoplocs>0]
datastartlocs=c(1,datastoplocs[1:(length(datastoplocs)-1)]+1)

test=test[headloc[outfile_type]:length(test)]
linelength=nchar(test[(1+dataloc[outfile_type])])

swatcolnames=gsub(" ","",substring(test[1],varnamestartlocs,varnamestoplocs))
swatcolnames=gsub("#","_no_",swatcolnames)
swatcolnames=gsub("/","_per_",swatcolnames)
test1=sub("(\\.[0-9]{5}E)"," \\1",test[(1+dataloc[outfile_type]):length(test)])
test1=gsub(" +"," ", test1)
swatfiledata=fread(paste(test1,collapse="\n"))
setnames(swatfiledata,swatcolnames)

if (outfile_type=="sub"){ uniqunits=length(unique(swatfiledata$SUB))
} else if (outfile_type=="rch"){ uniqunits=length(unique(swatfiledata$RCH))
} else if (outfile_type=="hru"){uniqunits=length(unique(swatfiledata$GIS))
} else if (outfile_type=="snu"){uniqunits=length(unique(swatfiledata$GISnum))
} else { print ("You need to add your file type to this function if it is not output.sub .rch or .hru")}
  
  swatfiledata$mdate = as.Date(floor((row(swatfiledata)[,1]-1)/uniqunits)+1, origin = paste(start_year - 1, "-12-31", sep = ""))
  return(swatfiledata)
}

