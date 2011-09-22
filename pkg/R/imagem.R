#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

imagem <- 
function (z, 
	zlim, 
	xlim = c(1,ncol(z)), 
    ylim = c(1,nrow(z)), 
    palette = heat.colors(12),
	add = FALSE, xaxs = "i", yaxs = "i", 
	xlab, ylab,
	main = deparse(substitute(z)),
	pars, # mar= c(2,2,6,6)+0.1,
	mar= c(2,1,3,1)+0.1, # default mar= c(1,1,6,4)+0.1,
	breaks, oldstyle = FALSE,
	names = TRUE, 
	...)
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
	 
 titleline<- 1 
	if (missing(pars)){
    strwrow <- max(strwidth(rownames(z),"inch"))
    strcol <- max(strwidth(colnames(z),"inch"))
    chwidth <- par("cin")[1]
    lineheight <- par("lheight")*par("cin")[2]
    titleline <- ceiling(strcol/lineheight)+0.5
	#mai <- par("mai")
	mai <- c(0, chwidth, strcol + chwidth, strwrow + chwidth) + mar* lineheight
	#mai[3]<-strcol + 2*chwidth +4.1* lineheight# up: usual 4.1 lines
	#mai[4]<-strwrow + 2*chwidth
	#mai <- mai + mar* lineheight
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
		col= palette, add=add, xaxs=xaxs, yaxs=yaxs,
		xlab="", ylab="",z=zi, xaxt="n",  yaxt="n",
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
}
}
	par(usr=c(1, ncol(z)+1, nrow(z), 0))
	p <-par("cin","din","fin","pin","plt","mai", "mar","usr")
	
	 invisible(p)
}#imagem

# imagem(Hotel)
