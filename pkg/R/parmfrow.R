parmfrow <- function (dims, aspr=dims[1]/dims[2], minin=0.25)
# all internal aspect ratios ar in dy/dx form.
# note: dims is in (y,x) order as returned by dim
# dim is in row,col
# din is in x,y
# image shows matrix in transposed flipped form
{
 	if (aspr<= 0) stop("parmfrow: aspr must be positive")
	if (minin<= 0.1) stop("parmfrow: mini must be at least 0.1 inch")
	din<-par("din")
	dar <- din[2]/din[1]
	if (aspr>1) {
		#print("matrix high") 
		minx <- minin; miny <- minin/aspr} else
		{
		#print("matrix wide")
		minx <- minin*aspr; miny <-   minin
		}
	#cat(din,dar,"\n")
	#if (dar>1) print("device high") else print("device wide")

	nx <- floor(din[1] / minx)
	ny <- floor(din[2] / miny)
	#cat(nx,ny,"\n")
	if (interactive()) 
		par(mfrow=c(ny,nx), ask=TRUE) else par(mfrow=c(ny,nx))
}
