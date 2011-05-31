Soil.Storage <-
function(S.avg,field.capacity, soil.water.content, porosity){
w2<-(log(1-(field.capacity/(1-0.4348*S.avg/(2.381*S.avg)))-field.capacity)-log(1-(porosity/(1-2.54/(2.381*S.avg)))-porosity
))/(porosity-field.capacity)##
w1<-log(1-field.capacity/(1-0.4348*S.avg/(2.381*S.avg))-field.capacity)+w2*field.capacity
return(2.381*S.avg*(1-(soil.water.content/(soil.water.content+exp(w1-w2*soil.water.content)))))
}

