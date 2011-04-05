alter_files <-
function(change_params){
library(operators)
for(ft in unique(change_params$filetype)){
        print(ft)
        files=list.files(,paste(ft,"$",sep=""))
        for (file in files) {
                fileorig=paste(file,".unixorig",sep="");
                junk%<%fileorig
                file_change_params=subset(change_params,filetype==ft)
                for( i in 1:length(rownames(file_change_params))){
                        startstr=file_change_params[i,"startstr"]
                        endstr=file_change_params[i,"endstr"]
                        current=file_change_params[i,"current"]
                        param=file_change_params[i,"parameter"]
                        multi=file_change_params[i,"multi"]
                        alter_type=file_change_params[i,"alter_type"]
                        frformat=unlist(strsplit(as.character(file_change_params[i,"frformat"]),","))
                        if(multi==F & alter_type=="new"){
                                junk=gsub(paste(".*",param,".*",sep=""),sprintf("%s%16.4f%s",startstr,as.real(current),endstr),junk)
                        }
                        if(multi==T & alter_type=="percent"){
                                junkline=grep(param,junk,value=T)
                                #print(paste(i,junkline,frformat))
                                a=read.fortran(con1<-textConnection(junkline),frformat)
                                close(con1)
                                stringb=sprintf("%s",a[1])
                                j=2
                                while ( !is.na(a[j])) {
                                        stringb=paste(stringb,sprintf("%12.2f",a[j]*current),sep="")
                                        j=j+1
                                }
                                junk=gsub(paste(".*",param,".*",sep=""),stringb,junk)
                        }
                }
                cat(junk,file=file,sep="\n")
        }
}
}

