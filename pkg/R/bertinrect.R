#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# rect function (xleft, ybottom, xright, ytop, density = NULL, angle = 45, 
#    col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"), 
#    ...) 
#
# aspect ratio ok
# single row: not ok, eg. bertinrect(Hotel[19,]) bertinrect(Hotel[,12])
bertinrect <- function(z, 
	main = deparse(substitute(z)), 
	sepwd=0.05,
	mar= c(2,1,2,1)+0.1, # default mar= c(1,1,6,4)+0.1,
	pars,
	names=TRUE, 
	...){
#$Revision$
	# [i,j] bottom left is at user coordinates (i,j)
	# sepwd is internal margin
	if (missing(main)) {main <- deparse(substitute(z))}	
	z <- as.matrix(z) #! support lists and data frames as well
	nrow <- nrow(z)
	ncol <- ncol(z)
 
 titleline<- 2 
	if (missing(pars)){
    strwrow <- max(strwidth(rownames(z),"inch"))
    strcol <- max(strwidth(colnames(z),"inch"))
    chwidth <- par("cin")[1] * 0.6 # using our cex=0.6
    lineheight <- par("lheight")*par("cin")[2]
    titleline <- ceiling(strcol/lineheight)+0.5
	#mai <- par("mai")
	mai <- c(0, chwidth, strcol + chwidth, strwrow + chwidth) + mar* lineheight
	#mai[3]<-strcol + 2*chwidth +4.1* lineheight# up: usual 4.1 lines
	#mai[4]<-strwrow + 2*chwidth
	#mai <- mai + mar* lineheight
	par(mai=mai)
	plot.new()
	#adjust plot region
	pin <- par("pin")
	aspp <- pin[2]/pin[1]
	aspx <- nrow/ncol
	if (aspp > aspx) {pin[2]<-pin[2]/aspp*aspx} else {pin[1] <- pin[1]/aspx*aspp}
	par(pin=pin)
	} else {
		plot.new()
		if(!is.null(pars)) { par(pars)}
	}
	
#	}
#	else 
#	{
#		plot.new()
#		if(!is.null(pars)) { par(pars)}
#	}


	plot.window(c(1, ncol(z)+1), c(1, nrow(z)+1),xaxs="i",yaxs="i", asp=1)
#	title(main=main, sub=as.character(titleline ),line=titleline, ...)
	title(main=main, line=titleline)
	p <-par("cin","din","fin","pin","plt","mai", "mar","usr")
#	oldpar <- par(no.readonly = TRUE)
	#on.exit(par(oldpar))
#    par(mar=mar)
 #	parasp(t(z))#par(pin=c(3,4))

#	plot(c(1, ncol(z)+1), c(1, nrow(z)+1), 
#	main=main,type= "n", xlab="", ylab="", axes=FALSE,mar=mar,...)

	#usrold <- par(usr= c(1, (ncol(z)+1)*(1+2*sepwd), 1, (nrow(z)+1)*(1+2*sepwd)) )
	#! improve. use scaling as in plot.window
	ranges <- apply(z,1,range,finite=TRUE) # transposed
	ranges[1,] <- pmin(0,ranges[1,]) #tack at zero
	ranges[2,] <- pmax(0,ranges[2,]) #tack at zero
	scale <- ranges[2,] - ranges [1,]
	scale[] <- ifelse(scale[]==0,0.1,scale[])
	scale[!is.finite(scale[] )] <- 0.1 	# fix zero scales
	scale[scale[]==0] <- 0.1
	#add some margin. ! improve. use scaling as in plot.window
	scale <- (1-2*sepwd)/scale
	zeroline <- -ranges[1,]*scale

	xleft <- matrix((1:ncol(z)), nrow(z), ncol(z), byrow=TRUE)+sepwd
	xright <- xleft+1-2*sepwd
	xbottom <-  1+nrow(z)-(matrix((1:nrow(z)),nrow(z),ncol(z))) + 
		sepwd +zeroline #box zero line
	xtop <- z*scale+xbottom

if (any(ranges[2,]==0)){
	abline(h= xbottom[ranges[2,]==0],lty=3,col="gray")}
	rect(xleft,xbottom,xright,xtop,...)

	#textnames(z)
   #          pos = 3, xpd = NA, offs = 1, srt = 90, cex=0.6)}
      if (!is.null(colnames(z))){ 
       	for (col in (1:dim(z)[2])) 
       		text(col+0.2, par("usr")[4]+0.5, colnames(z)[col], 
             adj=c(0,1),xpd = NA, offset = 4, srt = 90, cex=0.6)}
       if (!is.null(rownames(z))) {
       	r <- par("usr")[2] #right
       	for (row in (1:dim(z)[1])) 
       		text(r, nrow(z)-row+1.4, rownames(z)[row], 
            pos = 4, xpd = NA, offset = 0.2, srt = 0, cex=0.6)
            
   if (!all(is.finite(z))) {
   	badpos <- !is.finite(z)
   	xbottom <-  1+nrow(z)-(matrix((1:nrow(z)),nrow(z),ncol(z))) + sepwd #box zero line
   	text(xleft[badpos]+0.4,xbottom[badpos],labels=z[badpos], pos=3, offs=0.5,col="red",cex=0.6)
   }
} 
	par(usr=c(1, ncol(z)+1, nrow(z), 0))
	p <-par("cin","din","fin","pin","plt","mai", "mar","usr")
	invisible(p)
}

# bertinrect(Hotel)