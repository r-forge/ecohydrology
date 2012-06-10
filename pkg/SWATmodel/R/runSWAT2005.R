runSWAT2005 <-
function(){
libarch= if (nzchar(version$arch)) paste('libs', version$arch, sep='/') else 'libs'
swatbin<- "rswat2005.exe"
system(paste(.Library,"/EcoHydRology/",libarch,"/",swatbin,sep=""))

}

