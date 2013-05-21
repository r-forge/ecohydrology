##  Created by Janet Barclay May 2013
# 	janetbarclay@gmail.com


#Bar Plot with Error Bars
ColorVector<-c("#676A55", "#B92120", "#72A376", "#365339", "#A7AA95", "#3C7483")

# install.packages("dichromat")  ## seems to be a bug in ggplot2 that requires this..
library(ggplot2)
library(gridExtra)

#Making Sample Data
Names<-c("1","2","3","4","5")
Heights<-c(1,2,3,4,5)
ErrorBars<-c(.5,1,1,3,2)
TableA<-data.frame(Names,Heights,ErrorBars)
TableB<-TableA[1:3,]
TableC<-TableA[c(1,5,3,4),]
TableD<-TableA[2:5,]


#Generating the Plots - qplot make the plot frame, geom_bar creates the bars, geom_errorbar addes the error bars
plot1<-qplot(TableA$Names, TableA$Heights, geom="bar", ylab="Heights", xlab="Names")+geom_bar(colour="black", fill=ColorVector[1:nrow(TableA)])+geom_errorbar(aes(ymax=TableA$Height+TableA$Error, ymin=TableA$Height-TableA$Error))
plot2<-qplot(TableB$Names, TableB$Heights, geom="bar", ylab="Heights", xlab="Names")+geom_bar(colour="black", fill=ColorVector[3])+geom_errorbar(aes(ymax=TableB$Height+TableB$Error, ymin=TableB$Height-TableB$Error))
plot3<-qplot(TableC$Names, TableC$Heights, geom="bar", ylab="Heights", xlab="Names")+geom_bar(colour="black", fill=ColorVector[2])+geom_errorbar(aes(ymax=TableC$Height+TableC$Error, ymin=TableC$Height-TableC$Error))
plot4<-qplot(TableD$Names, TableD$Heights, geom="bar", ylab="Heights", xlab="Names")+geom_bar(colour="black", fill=ColorVector[5])+geom_errorbar(aes(ymax=TableD$Height+TableD$Error, ymin=TableD$Height-TableD$Error))

#Plots 4 plots in a grid
sidebysideplot <- grid.arrange(plot1, plot2,plot3, plot4, ncol=2)

#opens a new screen and makes 1 plot
dev.new()
plot1