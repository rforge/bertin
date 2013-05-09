#$Id$
#$Revision$
#$Date$
#$Author$

# aspect ratio ok
# single row: not ok, eg. bertinrect(Hotel[19,]) bertinrect(Hotel[,12])
bertinrect <- function(x,main,
	sepwd=0.05,
	mar= c(1,1,6,4)+0.1,
	...){
#$Revision$
	# [i,j] bottom left is at user coordinates (i,j)
	# sepwd is internal margin
	if (missing(main)) {main <- deparse(substitute(x))}	
	x <- as.matrix(x) #! support lists and data frames as well

	#oldpar <- par(no.readonly = TRUE)
	#on.exit(par(oldpar))

    par(mar=mar)
 #cat("mar  set up.pin:",par("pin")," mai:",par("mai"))
 #cat(" usr:",par("usr"),"\n")
	parasp(t(x))#par(pin=c(3,4))
 #cat("pin  set up.pin:",par("pin")," mai:",par("mai"))
# cat(" usr:",par("usr"),"\n")

	plot(c(1, ncol(x)+1), c(1, nrow(x)+1), 
		main=main,type= "n", xlab="", ylab="", 
		axes=FALSE,...)
#cat("Plot set up.pin:",par("pin")," mar:",par("mar"))
#cat(" usr:",par("usr"),"\n")



	#! improve. use scaling as in plot.window
	ranges <- apply(x,1,range,finite=TRUE) # transposed
	ranges[1,] <- pmin(0,ranges[1,]) #tack at zero
	ranges[2,] <- pmax(0,ranges[2,]) #tack at zero
	scale <- ranges[2,] - ranges [1,]
	scale[] <- ifelse(scale[]==0,0.1,scale[])
	scale[!is.finite(scale[] )] <- 0.1 	# fix zero scales
	scale[scale[]==0] <- 0.1
	#add some margin. ! improve. use scaling as in plot.window
	scale <- (1-2*sepwd)/scale
	zeroline <- -ranges[1,]*scale

	xleft <- matrix((1:ncol(x)), nrow(x), ncol(x), byrow=TRUE)+sepwd
	xright <- xleft+1-2*sepwd
	xbottom <-  1+nrow(x)-(matrix((1:nrow(x)),nrow(x),ncol(x))) + 
		sepwd +zeroline #box zero line
	xtop <- x*scale+xbottom

if (any(ranges[2,]==0)){
	abline(h= xbottom[ranges[2,]==0],lty=3,col="gray")}
	rect(xleft,xbottom,xright,xtop,...)
#cat("Rect set up.pin:",par("pin"))
#cat(" usr:",par("usr"),"\n")

	#textnames(x)
       if (!is.null(colnames(x))){ 
       	for (col in (1:dim(x)[2])) 
       		text(col, par("usr")[4], colnames(x)[col], 
  #          pos = 3, xpd = NA, offs = 1, srt = 90, cex=0.6)}
             adj=c(0,1),xpd = NA,  srt = 90, cex=0.6)}
       if (!is.null(rownames(x))) {
       	r <- par("usr")[2] #right
       	for (row in (1:dim(x)[1])) 
       		text(r, nrow(x)-row+1.4, rownames(x)[row], 
            pos = 4, xpd = NA, offs = 0, srt = 0, cex=0.6)
            
   if (!all(is.finite(x))) {
   	badpos <- !is.finite(x)
   	xbottom <-  1+nrow(x)-(matrix((1:nrow(x)),nrow(x),ncol(x))) + sepwd #box zero line
   	text(xleft[badpos]+0.4,xbottom[badpos],labels=x[badpos], pos=3, offs=0.5,col="red",cex=0.6)
   }
} 
}
