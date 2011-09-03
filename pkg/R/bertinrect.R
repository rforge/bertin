# $Id$
bertinrect <- function(x,main = deparse(substitute(x)),
	sepwd=0.05,
	mar= c(2,2,6,6)+0.1,
	...){
	# [i,j] bottom left is at user coordinates (i,j)
	# sepwd is internal margin
	
	oldpar <- par(mar=mar)
	on.exit(par(oldpar))
	#parasp(x)
	plot(c(1, ncol(x)+1), c(1, nrow(x)+1), 
	main=main,type= "n", xlab="", ylab="", axes=FALSE)

	x <- as.matrix(x) #! support lists and data frames as well

	#! improve. use scaling as in plot.window
	ranges <- apply(x,1,range,finite=TRUE) # transposed
	ranges[1,] <- pmin(0,ranges[1,]) #tack at zero
	ranges[2,] <- pmax(0,ranges[2,]) #tack at zero
	scale <- ranges[2,] - ranges [1,]
	scale[] <- ifelse(scale[]==0,0.1,scale[])
	scale[!is.finite(scale[] )] <- 0.1 	# fix zero scales
	scale[scale[]==0] <- 0.1
	#add some margin. ! improve. use scaling as in plot.window
	#for now, is handled by sepwd
	#ranges[1,] <- ranges[1,]- 0.04*scale[]
	#ranges[2,] <- ranges[2,]+ 0.04*scale[]
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

	#textnames(x)
       if (!is.null(colnames(x))){ 
       	for (col in (1:dim(x)[2])) 
       		text(col+0.8, nrow(x)+1, colnames(x)[col], 
            pos = 3, xpd = NA, offs = 1, srt = 90, cex=0.6)}
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

