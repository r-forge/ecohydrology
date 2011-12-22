BaseflowSeparation <-
function(streamflow, filter_parameter=0.925, passes=3){
#####################
##PASS 1
bt1<-vector(length=length(streamflow))
rt1<-vector(length=length(streamflow))
bt1[1]<-0
rt1[1]<-0
for (i in 2:length(bt1)){
if ((filter_parameter*bt1[i-1]+((1-filter_parameter)/2)*(streamflow[i]+streamflow[i-1]))>streamflow[i]){
bt1[i]<-streamflow[i]
} else bt1[i]<-filter_parameter*bt1[i-1]+((1-filter_parameter)/2)*(streamflow[i]+streamflow[i-1])
rt1[i]<-streamflow[i]-bt1[i]
}

#####################
##PASS 2
bt2<-vector(length=length(streamflow))
rt2<-vector(length=length(streamflow))
bt2[length(bt2)]<-0
rt2[1]<-0
for (i in (length(bt2)-1):1){
if ((filter_parameter*bt2[i+1]+((1-filter_parameter)/2)*(bt1[i]+bt1[i+1]))>bt1[i]){
bt2[i]<-bt1[i]
} else bt2[i]<-filter_parameter*bt2[i+1]+((1-filter_parameter)/2)*(bt1[i]+bt1[i+1])
rt2[i]<-streamflow[i]-bt2[i]
}

#####################
##PASS 3
bt3<-vector(length=length(streamflow))
rt3<-vector(length=length(streamflow))
bt3[1]<-0
rt3[1]<-0
for (i in 2:length(bt3)){
if ((filter_parameter*bt3[i-1]+((1-filter_parameter)/2)*(bt2[i]+bt2[i-1]))>bt2[i]){
bt3[i]<-bt2[i]
} else bt3[i]<-filter_parameter*bt3[i-1]+((1-filter_parameter)/2)*(bt2[i]+bt2[i-1])
rt3[i]<-streamflow[i]-bt3[i]
}

if (passes == 3) f<-data.frame(bt3,rt3)
else if (passes == 1) f<-data.frame(bt1,rt1)
else f <- data.frame(bt2,rt2)
return(f)
}

