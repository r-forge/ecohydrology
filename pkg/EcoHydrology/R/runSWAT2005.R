runSWAT2005 <-
function(){
Sys.setenv(GFORTRAN_STDIN_UNIT=-1)
Sys.setenv(GFORTRAN_STDOUT_UNIT=-1)
Sys.setenv(GFORTRAN_STDERR_UNIT=-1)
library.dynam("EcoHydrology","EcoHydrology")
.Fortran("swat2005",1)
library.dynam.unload("EcoHydrology",grep("EcoHydrology",searchpaths(),value=T))
}

