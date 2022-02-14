swat_objective_function_rch=function (x, calib_range, calib_params, flowgage, rch,save_results=F)
{
  calib_params$current <- x
  tmpdir=as.character(as.integer((runif(1)+1)*10000))
  tmpdir=paste(c(format(Sys.time(), "%s"),tmpdir,Sys.getpid()),sep="",collapse="")
  print(tmpdir)
  dir.create(tmpdir)
  file.copy(list.files(),tmpdir)
  setwd(tmpdir)
  file.remove(list.files(pattern="output."))
  alter_files(calib_params)
  libarch = if (nzchar(base::version$arch)) paste("libs", base::version$arch, sep = "/") else "libs"
  swatbin <- "rswat2012.exe"
  junkout=system(shQuote(paste(path.package("SWATmodel"), libarch, swatbin, sep = "/")),intern = TRUE,ignore.stderr=TRUE)
  start_year = read.fortran(textConnection(readLines("file.cio")[9]), "f20")
  load("readSWAT.R")
  outdata = readSWAT("rch",".")
  test2 = subset(outdata, outdata$RCH == rch)
  test3 = merge(flowgage$flowdata, test2, all = F)
  NS = NSeff(test3$Qm3ps, test3$FLOW_OUTcms)
  print(NS)
  if(save_results){
    SWAToutput()
    for(diffname in grep("output|unixorig",grep(paste0(unique(calib_params[,1]),
						       collapse = "|"),list.files(),value=TRUE),
			 invert = TRUE,value=TRUE)){
      if(length(grep("calib",diffname))>0){next}
      junk1=readLines(diffname)
      junk2=readLines(paste0("../",diffname))
      difflocs=as.numeric(strsplit(paste0(as.data.frame(strsplit(ses(junk1,junk2),split = "c"))[2,],collapse = ","),split=",")[[1]])
      if(length(difflocs)>0){
	write(diffname,"calibdiffs.txt",append=TRUE)
	write(paste0(diffname,":",junk1[difflocs]," < ",junk2[difflocs]),"calibdiffs.txt",append=TRUE)
      }
    }
    calibdir=paste0("../calib",format(Sys.time(),format="%Y%m%d%H%M"))
    dir.create(calibdir)
    file.copy(list.files(),calibdir)
  }
  file.remove(list.files())
  setwd("../")
  file.remove(tmpdir)
  return(abs(NS - 1))
}
