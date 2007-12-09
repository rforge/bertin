parasp<-
# set aspect ratio match data matrix or given aspect ratio
# usage: opar<-par(no.readonly=TRUE); on.exit(par(opar)); parasp(dat)
function(dat, aspr=dim(dat)[1]/dim(dat)[2], tol=0.01)
{	if (aspr<= 0) stop("parasp: aspr must be positive")
	pin<-par("pin")
	ar <- pin[1]/pin[2]
	if (abs(ar/aspr)>tol) {
		if (ar < aspr) 
			pin[2] <- pin[1]/aspr  else pin[1] <- pin[2]* aspr
		par(pin=pin)
	}
}
