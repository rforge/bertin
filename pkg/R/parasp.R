#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

parasp<-
# set aspect ratio match data matrix or given aspect ratio
# usage: opar<-par(no.readonly=TRUE); on.exit(par(opar)); parasp(dat)
function(dat, aspr=dim(dat)[1]/dim(dat)[2], tol=0.01)
{
	#$Revision$
	if (aspr<= 0) stop("parasp: aspr must be positive")
	pin<-par("pin")
	ar <- pin[1]/pin[2]
		cat("parasp: ",ar, aspr,ar/aspr,"\n")
	if (abs(1- ar/aspr)>tol) {
		cat("changing.\n")
		if (ar < aspr) 
			pin[2] <- pin[1]/aspr  else pin[1] <- pin[2]* aspr
	par("fig", "fin", "plt", "pin","usr")
		par(pin=pin)
	par("fig", "fin", "plt", "pin","usr")
	}
}
