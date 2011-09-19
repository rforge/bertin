#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

imagem33 <- 
function (z, 
	zlim = range(z[is.finite(z)]), 
	xlim = c(1,ncol(z)), 
    ylim = c(1,nrow(z)), 
    col = heat.colors(12),
	add = FALSE, xaxs = "i", yaxs = "i", 
	xlab, ylab,
	main = deparse(substitute(z)),
	mar= c(2,2,6,6)+0.1,
	breaks, oldstyle=FALSE,
	names=TRUE, 
	coloffs=0.5, rowoffs=0.5, ...)
{
# a variant of image.default keeping matrix orientation

#$Revision$
#! adjust calling structure with image()
#      image.default(x, y, z, zlim, xlim, ylim, col = heat.colors(12),
#           add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
#           breaks, oldstyle = FALSE, ...)

#
	
	zi <- t(z)
	opin <- par("pin","mar")#; #on.exit(par(pin=opin))
	par(mar=mar)
	parasp(zi)
	image(
		1:nrow(zi),1:ncol(zi), zlim=zlim,
		#xlim=xlim,
		ylim=c(ncol(zi)+0.5,0.5), 
		col=col, add=add, xaxs=xaxs, yaxs=yaxs,
		xlab="", ylab="",z=zi, 
		main=main,
		breaks=breaks, oldstyle=oldstyle,
#		useRaster=TRUE,	
#Error in image.default(1:nrow(zi), 1:ncol(zi), zlim = zlim, ylim = c(ncol(zi) +  : 
#  formal argument "useRaster" matched by multiple actual arguments
#  ????
#		frame.plot=FALSE,
		...)
# print(paste("pin:",par("pin")))
# print(paste("usr:",par("usr")))

#	image(
#		1:ncol(z), 1:nrow(z),z=t(z),zlim=zlim,
#		xlim=xlim,
#		ylim=c(nrow(z)+0.5,0.5), 
#	)

	if (names) {
		#! improve placement of names. use either nrow/ncol or par("usr")

	# textnames <-	function (zi, coloffs=-1, rowoffs=NULL) {
		# note: image interchanges rows/colums
		for (x in (1:dim(zi)[1]) ) # column labels
			text(x, par("usr")[4]-coloffs , rownames(zi)[x], adj=c(0,0.5),#pos=3, 
				xpd=NA,offs= coloffs, srt=90, cex=0.6)
		
		r <- par("usr")[2]+rowoffs
		for (y in (1:dim(zi)[2]))  # row labels
			text(r, y, colnames(zi)[y], adj=c(0,0.5),#pos=4, 
			xpd=NA, offs=rowoffs,srt=0,cex=0.6)
#	} # textnames
	
#		textnames(zi,coloffs=-4,rowoffs=1)
	} # names
}#imagem

#imagem(mm, main="mm")
#imagem(t(mm), main="t(mm)")

# imagem38
imagem38 <- 
function (z, 
	zlim, 
	xlim = c(1,ncol(z)), 
    ylim = c(1,nrow(z)), 
    col = heat.colors(12),
	add = FALSE, xaxs = "i", yaxs = "i", 
	xlab, ylab,
	main = deparse(substitute(z)),
	pars, # mar= c(2,2,6,6)+0.1,
	breaks, oldstyle=FALSE,
	names=TRUE, 
	coloffs=0.5, rowoffs=0.5, ...)
{
# a variant of image.default keeping matrix orientation

#$Revision$
#! adjust calling structure with image()
#      image.default(x, y, z, zlim, xlim, ylim, col = heat.colors(12),
#           add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
#           breaks, oldstyle = FALSE, ...)

#
	if (missing(main)) {main <- deparse(substitute(z))}	
	zi <- t(as.matrix(z)) #! support lists and data frames as well
#	opin <- par("pin","mar")#; #on.exit(par(pin=opin))
#	par(mar=mar)
#	parasp(zi)

#! clean up. remove x (or zi)
	x <- as.matrix(zi) #! support lists and data frames as well
	if (missing(zlim)) zlim <- range(zi[is.finite(zi)])
	nrow <- nrow(x)
	ncol <- ncol(x)
	if (missing(pars)){
    strwrow <- max(strwidth(rownames(x),"inch"))
    strcol <- max(strwidth(colnames(x),"inch"))
    chwidth <- par("cin")[1]
	mai <- par("mai")
	mai[2]<- chwidth
	mai[3]<-strcol + 2*chwidth
	mai[4]<-strwrow + 2*chwidth
	par(mai=mai)
#	plot.new()
	#adjust plot region
	pin <- par("pin")
	aspp <- pin[2]/pin[1]
	aspx <- nrow/ncol
	if (aspp > aspx) {pin[2]<-pin[2]/aspp*aspx} else {pin[1] <- pin[1]/aspx*aspp}
	par(pin=c(pin[2],pin[1]))
#	par(pin=pin)
	} else {
#		plot.new()
		if(!is.null(pars)) { par(pars)}
	}


	image(
		1:nrow(zi),1:ncol(zi), zlim=zlim,
		#xlim=xlim,
		ylim=c(ncol(zi)+0.5,0.5), 
		col=col, add=add, xaxs=xaxs, yaxs=yaxs,
		xlab="", ylab="",z=zi, 
		main=main,
		breaks=breaks, oldstyle=oldstyle,
#		useRaster=TRUE,	
#Error in image.default(1:nrow(zi), 1:ncol(zi), zlim = zlim, ylim = c(ncol(zi) +  : 
#  formal argument "useRaster" matched by multiple actual arguments
#  ????
#		frame.plot=FALSE,
		...)

	p <-par("cin","din","fin","pin","plt","mai", "mar","usr")

# print(paste("pin:",par("pin")))
# print(paste("usr:",par("usr")))

#	image(
#		1:ncol(z), 1:nrow(z),z=t(z),zlim=zlim,
#		xlim=xlim,
#		ylim=c(nrow(z)+0.5,0.5), 
#	)

	if (names) {
		#! improve placement of names. use either nrow/ncol or par("usr")

	# textnames <-	function (zi, coloffs=-1, rowoffs=NULL) {
		# note: image interchanges rows/colums
		for (x in (1:dim(zi)[1]) ) # column labels
			text(x, par("usr")[4]-coloffs , rownames(zi)[x], adj=c(0,0.5),#pos=3, 
				xpd=NA,offs= coloffs, srt=90, cex=0.6)
		
		r <- par("usr")[2]+rowoffs
		for (y in (1:dim(zi)[2]))  # row labels
			text(r, y, colnames(zi)[y], adj=c(0,0.5),#pos=4, 
			xpd=NA, offs=rowoffs,srt=0,cex=0.6)
#	} # textnames
	
#		textnames(zi,coloffs=-4,rowoffs=1)
	} # names
	
	 invisible(p)
}#imagem38

imagem <- 
function (z, 
	zlim, 
	xlim = c(1,ncol(z)), 
    ylim = c(1,nrow(z)), 
    col = heat.colors(12),
	add = FALSE, xaxs = "i", yaxs = "i", 
	xlab, ylab,
	main = deparse(substitute(z)),
	pars, # mar= c(2,2,6,6)+0.1,
	breaks, oldstyle=FALSE,
	names=TRUE, 
	coloffs=0.5, rowoffs=0.5, ...)
{
# a variant of image.default keeping matrix orientation
# note: image() shows matrix columns as rows, i.e. transposes
#$Revision$
#! adjust calling structure with image()
#      image.default(x, y, z, zlim, xlim, ylim, col = heat.colors(12),
#           add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
#           breaks, oldstyle = FALSE, ...)

#
	if (missing(main)) {main <- deparse(substitute(z))}	
	zi <- t(as.matrix(z)) #! support lists and data frames as well
#	opin <- par("pin","mar")#; #on.exit(par(pin=opin))
#	par(mar=mar)
#	parasp(zi)

#! clean up. remove x (or zi)
	z <- as.matrix(z) #! support lists and data frames as well
	if (missing(zlim)) zlim <- range(zi[is.finite(zi)])
	nrow <- nrow(z)
	ncol <- ncol(z)
	if (missing(pars)){
    strwrow <- max(strwidth(rownames(z),"inch"))
    strcol <- max(strwidth(colnames(z),"inch"))
    chwidth <- par("cin")[1]
	mai <- par("mai")
	mai[2]<- chwidth
	mai[3]<-strcol + 2*chwidth
	mai[4]<-strwrow + 2*chwidth
	par(mai=mai)
#	plot.new()
	#adjust plot region
	pin <- par("pin")
	aspp <- pin[2]/pin[1]
	aspx <- nrow/ncol
	if (aspp > aspx) {pin[2]<-pin[2]/aspp*aspx} else {pin[1] <- pin[1]/aspx*aspp}
	par(pin=pin)
	} else {
		#plot.new()
		if(!is.null(pars)) { par(pars)}
	}
	

	image.default(
		1:nrow(zi),1:ncol(zi), zlim=zlim,
		#xlim=xlim,
		ylim=c(ncol(zi)+0.5,0.5), 
		col=col, add=add, xaxs=xaxs, yaxs=yaxs,
		xlab="", ylab="",z=zi, xaxt="n",  yaxt="n",
		main=main,
		breaks=breaks, oldstyle=oldstyle,
#		useRaster=TRUE,	
#Error in image.default(1:nrow(zi), 1:ncol(zi), zlim = zlim, ylim = c(ncol(zi) +  : 
#  formal argument "useRaster" matched by multiple actual arguments
#  ????
#		frame.plot=FALSE,
#	asp=1,
		...)

	p <-par("cin","din","fin","pin","plt","mai", "mar","usr")

# print(paste("pin:",par("pin")))
# print(paste("usr:",par("usr")))

par(usr=c(1, ncol(z)+1, 1, nrow(z)+1))#,xaxs="i",yaxs="i"
	if (names) {
		#! improve placement of names. use either nrow/ncol or par("usr")

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
            pos = 4, xpd = NA, offset = 0.5, srt = 0, cex=0.6)
}
}
	
	 invisible(p)
}#imagem

# imagem(Hotel)
