runSWAT2005 <-
function(){
Sys.setenv(GFORTRAN_STDIN_UNIT=-1)
library.dynam("rswat","EcoHydrology")
.Fortran("swat2005",1)
library.dynam.unload("rswat",grep("EcoHydrology",searchpaths(),value=T))
}

