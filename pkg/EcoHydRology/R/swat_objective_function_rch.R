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
  junkout=system(shQuote(paste(path.package("SWATmodel"), libarch, swatbin, sep = "/")),intern = TRUE)
  start_year = read.fortran(textConnection(readLines("file.cio")[9]), "f20")
  load("readSWAT.R")
  outdata = readSWAT("rch",".")
  test2 = subset(outdata, outdata$RCH == rch)
  test3 = merge(flowgage$flowdata, test2, all = F)
  NS = NSeff(test3$Qm3ps, test3$FLOW_OUTcms)
  print(NS)
  if(save_results){file.copy(list.files(),"../")}
  file.remove(list.files())
  setwd("../")
  file.remove(tmpdir)
  return(abs(NS - 1))
}

