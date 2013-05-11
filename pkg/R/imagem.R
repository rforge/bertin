#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

#      image.default(x, y, z, zlim, xlim, ylim, col = heat.colors(12),
#           add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
#           breaks, oldstyle = FALSE, ...)

imagem <- 
function (z, 
	zlim, 
	xlim = c(1,ncol(z)), 
    ylim = c(1,nrow(z)), 
    palette = heat.colors(12),
	add = FALSE, xaxs = "i", yaxs = "i", 
	xlab, ylab,
	main = deparse(substitute(z)),
	pars,
	aspz,
	mar= c(1,1,2,1)+0.1, # default mar= c(1,1,6,4)+0.1, # mar= c(2,2,6,6)+0.1,
	breaks, oldstyle = FALSE,
	names = TRUE, 
	...) {
# a variant of image.default keeping matrix orientation
# note: image() shows matrix columns as rows, i.e. transposes
#$Revision$
#! adjust calling structure with image()
# keep imagem() and bertinrect() in parallel 
	# [i,j] bottom left is at user coordinates (i,j)
	
	if (missing(main)) {main <- deparse(substitute(z))}	

	# z <- as.matrix(z) #! support lists and data frames as well
	if (missing(zlim)) zlim <- range(z[is.finite(z)])
	nrow <- nrow(z)
	ncol <- ncol(z)

	# if (missing(asp) ) {
		# if (missing(aspz)) {
		# asp=1
	# } else {
		# asp=aspz
	# }
	# }

	if (missing(aspz)) {
		aspz=nrow(z)/ncol(z)
	} else {
		aspz=aspz*nrow(z)/ncol(z)
	}
	
	titleline<- 1 
	if (missing(pars)){
    strwrow <- max(strwidth(rownames(z),"inch"))
    strcol <- max(strwidth(colnames(z),"inch"))
	chwidth <- par("cin")[1] * 0.6 # using our cex=0.6
    lineheight <- par("lheight")*par("cin")[2]
	titleline <- ceiling(strcol/lineheight)+0.5
	#mai <- par("mai")
	mai <- c(0, chwidth, strcol + chwidth, strwrow + chwidth) + mar * lineheight
	#mai[3]<-strcol + 2*chwidth +4.1* lineheight# up: usual 4.1 lines
	#mai[4]<-strwrow + 2*chwidth
	#mai <- mai + mar* lineheight
	par(mai=mai)
#	plot.new()
	#adjust plot region
	pin <- par("pin")
	aspp <- pin[2]/pin[1]
	#aspz <- nrow/ncol
	if (aspp > aspz) {pin[2]<-pin[2]/aspp*aspz} else {pin[1] <- pin[1]/aspz*aspp}
	par(pin=pin)
	} else {
		#plot.new()
		if(!is.null(pars)) { par(pars)}
	}
	

	#zi <- t(as.matrix(z)) #! support lists and data frames as well
	image.default(
		1:ncol, 1:nrow, #1:nrow(zi),1:ncol(zi), 
		zlim=zlim,
		#xlim=xlim,
		ylim=c(nrow+0.5,0.5), #ylim=c(ncol(zi)+0.5,0.5), 
		col= palette, add=add, xaxs=xaxs, yaxs=yaxs,
		xlab="", ylab="",z=t(z), xaxt="n",  yaxt="n",
#		main=main,
		breaks=breaks, oldstyle=oldstyle,
#		useRaster=TRUE,	
#Error in image.default(1:nrow(zi), 1:ncol(zi), zlim = zlim, ylim = c(ncol(zi) +  : 
#  formal argument "useRaster" matched by multiple actual arguments
#  ????
#		frame.plot=FALSE,
#	asp=1,
		...)

	title(main=main, line=titleline) # let sub etc be handled by image

	p <-par("cin","din","fin","pin","plt","mai", "mar","usr") # for debug

par(usr=c(1, ncol(z)+1, 1, nrow(z)+1))#,xaxs="i",yaxs="i"
	if (names) {
		#! improve placement of names. use either nrow/ncol or par("usr")

	#textnames(z)
	pu <- par("usr")
   #          pos = 3, xpd = NA, offs = 1, srt = 90, cex=0.6)}
      if (!is.null(colnames(z))){ 
      	colwidth <- (pu[2]-pu[1])/ncol
      	rowheight <- (pu[4]-pu[3])/nrow
       	for (col in (1:dim(z)[2])) 
       		text(col+0.5*colwidth, par("usr")[4]+0.1*rowheight, colnames(z)[col], #pos=3,
             adj=c(0,1),xpd = NA, offset = 2.0, srt = 90, cex=0.6)}
       if (!is.null(rownames(z))) {
       	r <- par("usr")[2] #right
       	for (row in (1:dim(z)[1])) 
       		text(r, nrow(z)-row+1.4, rownames(z)[row], 
            pos = 4, xpd = NA, offset = 0.2, srt = 0, cex=0.6)

	#! handle non-finite values
	
	}
}
	par(usr=c(1, ncol(z)+1, nrow(z), 0))
	p <-par("cin","din","fin","pin","plt","mai", "mar","usr")
	invisible(p)
}#imagem


# imagem(Hotel)
