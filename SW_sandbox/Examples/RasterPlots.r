##  Created by Janet Barclay May 2013
# 	janetbarclay@gmail.com


#Plotting a Raster
library(raster)
library(rgdal)


#function for plotting the raster
rasterplot<-function(displayraster, rastermax=3.65, rastermin=-.5, titletext2="", scalelabel="", legendlabel="", scaletype="log"){
	
	numcols=100
	numlabels=10
	
	
	r.range <- c(rastermin, rastermax)
		

		cols1=colorRampPalette(c("midnightblue","darkblue", "darkolivegreen4","gainsboro","gold","darkorange","darkred"))(numcols)
		cols<-colorRampPalette(cols1)(100)

	
	brk = seq(r.range[1], r.range[2], by = (r.range[2]-r.range[1])/numcols)
		labelbrk<-seq(r.range[1], r.range[2], by = ((r.range[2]-r.range[1])/numlabels))
	if (scaletype=="log") {
		arg=list(at=labelbrk,labels=format(10^labelbrk,digits=1,sci=FALSE))
		} else {
		arg=list(at=labelbrk,labels=format(labelbrk,digits=1,sci=FALSE))
		}
	plot(displayraster, main=titletext2,breaks=brk, col=cols, axis.args=arg, legend.shrink=1, legend.width=1.5, ylab="Northing(m)", xlab="Easting(m)", legend=TRUE)
	mtext(scalelabel,side=4,line=-1, cex=.8)

	
	
	}
	
	
# RasterLayer
r <- raster(nrows=20, ncols=10)
r <- setValues(r, 1:ncell(r))

s <- raster(nrows=20, ncols=10)
s <- setValues(s, (1:ncell(s))*3)

#plotting the raster on a linear scale
par(mfrow=c(1,2))
rasterplot(r, rastermax=600,rastermin=1, scaletype="linear", scalelabel="this is the scale label", titletext2="R")
rasterplot(s, rastermax=600,rastermin=1, scaletype="linear", scalelabel="this is the scale label", titletext2="S")

#plotting the raster on a linear scale
#the raster min / max in this case are the log10 of the raster min / max from the last example
par(mfrow=c(1,2))
rasterplot(log10(r), rastermax=2.8,rastermin=0, scaletype="log", scalelabel="this is the scale label", titletext2="Log(R)")
rasterplot(log10(s), rastermax=2.8,rastermin=0, scaletype="log", scalelabel="this is the scale label", titletext2="Log(S)")