build_swat_basic <-
function(tmpdir){
data(swat_general)
if(missing(tmpdir)){
  tmpdir=readline("Please enter a temp directory where you want to build your run...\n")
}
dir.create(tmpdir)
setwd(tmpdir)
for (file in names(swat_general)){print(file); cat(unlist(swat_general[file]),file=file,sep="\n")}
print(paste("You are now in the project directory of: ",getwd()))
}

