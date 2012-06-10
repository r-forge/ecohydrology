runSWAT2009 <-
function(){
libarch= if (nzchar(version$arch)) paste('libs', version$arch, sep='/') else 'libs'
swatbin<- "rswat2009.exe"
system(paste(.Library,"/EcoHydRology/",libarch,"/",swatbin,sep=""))

}

