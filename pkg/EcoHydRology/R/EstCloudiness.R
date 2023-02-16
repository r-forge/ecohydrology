EstCloudiness <- function (Tx, Tn, trans=NULL, transMin = 0.15, transMax = 0.75, opt = "linear") 
{
	if (is.null(trans))	trans <- transmissivity(Tx, Tn)
    if (opt=="Black") {
		cl <- (0.34 - sqrt(0.34^2 + 4*0.458*(0.803-trans)))/(-2*0.458)
		cl[which(trans > 0.803)] <- 0
	} else {
		cl <- 1 - (trans-transMin) / (transMax-transMin)
	}
	cl[which(cl > 1)] <- 1
    cl[which(cl < 0)] <- 0
    return(cl)
}
