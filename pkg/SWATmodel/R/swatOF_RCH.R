#note output printed after NSKIP in file.cio of the directory
#NSKIP==0 in build_swat_basic currently
swatOF_RCH = function (x, calib_params, flowgage, rch) {
  calib_params$current <- x
  tmpdir=as.character(as.integer((runif(1)+1)*10000))
  tmpdir=paste(c(format(Sys.time(), "%s"),tmpdir,Sys.getpid()),sep="",collapse="") #print(tmpdir)
  dir.create(tmpdir)
  file.copy(list.files(),tmpdir)
  setwd(tmpdir)
  file.remove(list.files(pattern="output."))
  alter_files(calib_params)
  libarch = if (nzchar(version$arch)) paste("libs", version$arch, sep = "/") else "libs"
  swatbin <- "rswat2012.exe"
  system(shQuote(paste(path.package("SWATmodel"), libarch, swatbin, sep = "/")))
  outrch = as.data.frame(readSWAT(outfile_type = "rch")) #the whole output.rch
  outrch = outrch[outrch$RCH==rch,] #just rows for the desired rch, only printed after NSKIP...
  junk=merge(outrch, flowgage$flowdata, by.x="mdate", by.y="mdate", all=F)
  NS = topmodel::NSeff(Qobs = junk$flow/3600/24, Qsim = junk$FLOW_OUTcms)
  #print(NS)
  file.remove(list.files())
  setwd("../")
  file.remove(tmpdir)
  return(abs(NS - 1))
}

