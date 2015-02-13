readSWAT <-
function(outfile_type="rch",pathtofile="./"){
if(missing(outfile_type)){print(" 'outfile_type' is missing, should be rch, sub, or.. ask drf28 for other types")}
sfilename=file(paste(pathtofile,"output.",outfile_type,sep=""))
nskipline=grep("NYSKIP",readLines("file.cio"))
nskip=read.fortran(textConnection(readLines(paste(pathtofile,"file.cio",sep=""))[nskipline]),"f20")
start_year = nskip + read.fortran(textConnection(readLines(paste(pathtofile,"file.cio",sep=""))[9]), "f20")
icalenline=grep("ICALEN",readLines("file.cio"))
icalen=read.fortran(textConnection(readLines(paste(pathtofile,"file.cio",sep=""))[icalenline]),"f20")
test = readLines(sfilename)
unlink(sfilename)
test=test[9:length(test)]
linelength=nchar(test[2])
if (outfile_type=="sub"){
   datalength=10
   datastartlocs=c(1,11,20,seq(25,linelength-1,datalength))
   datastoplocs=c(10,19,24,seq(24+datalength,linelength,datalength))
   numvars=(linelength-25)/datalength
#
#  1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,18e10.3,1x,e10.5,3e10.3)
#
   if(length(datastartlocs) >= 21){
    datastartlocs[21:length(datastartlocs)]=datastartlocs[21:length(datastartlocs)]+1
    datastoplocs[21:length(datastoplocs)]=datastoplocs[21:length(datastoplocs)]+1
   }
} else if (outfile_type=="rch"){
   datalength=12
   datastartlocs=c(1,11,20,seq(26,linelength,datalength))
   datastoplocs=c(10,19,25,seq(25+datalength,linelength,datalength))
   numvars=(linelength-25)/datalength
} else { print ("You need to add your file type to this function if it is not output.sub or output.rch")}
  test[1]=gsub("#","X",test[1])
  test[1]=gsub("/","Y",test[1])
  swatcolnames=gsub(" ","",substring(test[1],datastartlocs,datastoplocs))
  swatcolnames=gsub("X","_no_",swatcolnames)
  swatcolnames=gsub("Y","_per_",swatcolnames)
  test=test[2:length(test)]
  swatfiledata=data.frame(lapply(1:length(swatcolnames),function(x){return(data.frame(substr(test,datastartlocs[x],datastoplocs[x])))}))
  colnames(swatfiledata)<-swatcolnames
  swatfiledata[, c(1:numvars+3)] <- sapply(swatfiledata[, c(1:numvars+3)], as.character)
  swatfiledata[, c(2:numvars+3)] <- sapply(swatfiledata[, c(2:numvars+3)], as.numeric)
  swatfiledata$mdate = as.Date(floor((row(swatfiledata)[,1]-1)/length(unique(swatfiledata$RCH)))+1, origin = paste(start_year - 1, "-12-31", sep = ""))
  return(swatfiledata)
}

