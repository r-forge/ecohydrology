swat_objective_function <-
function(x,calib_range,change_params){
library(topmodel)
start_date<-"1993-12-31"
change_params$current<-x
alter_files(change_params)
runSWAT2005()
        sim=read.fortran(file="output.rch",c("X5","F5","X9","F6","X12","F12"),skip=9)
        sim$V4=as.Date(as.integer((row(sim)[,1]+(sim$V2[1]*max(sim$V1))-1)/max(sim$V1)),origin=start_date)
        sim_df=data.frame(date=sim$V4,sim_flow=sim$V3,charstr=sim$V1)
        gdata_df=read.csv("BN_ElDiem_Flow-1-1-1999_12-31-2004ForDan.csv",header=T)
#       gdata=read.table(file="gage.dat",skip=2)
#       gdata_df=data.frame(date=as.Date(gdata$V3,"%Y-%m-%d"),obs_flow=as.numeric(as.character(gdata$V4))*0.0283168466,quality=gdata$V5)
#       gdata_df=data.frame(date=as.Date(gdata$V3,"%Y-%m-%d"),obs_flow=as.numeric(as.character(gdata$V4))*0.0283168466,quality=gdata$V5)
        gdata_df$date=as.Date(gdata_df$Date)
        gdata_df$obs_flow=gdata_df$Flow.cms.
        sim_df=subset(sim_df,date>calib_range[1] & date < calib_range[2] & charstr==5)
        obj_df=merge(sim_df,gdata_df)
        par(mfrow=c(2,2))
        plot(obj_df$date,obj_df$obs_flow)
        plot(obj_df$date,obj_df$sim_flow)
        plot(obj_df$obs_flow,obj_df$sim_flow)
        NS<-NSeff(obj_df$obs_flow,obj_df$sim_flow)
        print(NS)
        abs(NS-1)
}
