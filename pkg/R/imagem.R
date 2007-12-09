imagem <- 
# a variant of image.default keeping matrix orientation
# gs 011112
function (z, zlim = range(z[is.finite(z)]), xlim = c(1,ncol(z)), 
    ylim = c(1,nrow(z)), col = heat.colors(12),
	add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,main,
	breaks, oldstyle=FALSE,
	names=TRUE, coloffs=-1, rowoffs=4,...)
{
#! adjust calling structure with image()
#      image.default(x, y, z, zlim, xlim, ylim, col = heat.colors(12),
#           add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
#           breaks, oldstyle = FALSE, ...)

#! improve placement of names. use either nrow/ncol or par("usr")

	textnames <-
	function (zi, coloffs=-1, rowoffs=NULL) {
		# note: image interchanges rows/colums
		for (x in (1:dim(zi)[1]) ) # column labels
			text(x, ncol(zi)+0.5, rownames(zi)[x], pos=3, 
				xpd=NA,offs= coloffs, srt=270)
		
		r <- par("usr")[2]
		for (y in (1:dim(zi)[2]))  # row labels
			text(r, y, colnames(zi)[y], pos=4, xpd=NA, offs=rowoffs,srt=0)
	} # textnames
	
	
	zi <- t(z)
	opin <- par("pin"); on.exit(par(pin=opin))
	parasp(zi)
	image(
		1:nrow(zi),1:ncol(zi), zlim=zlim,
		#xlim=xlim,
		ylim=c(ncol(zi)+0.5,0.5), 
		col=col, add=add, xaxs=xaxs, yaxs=yaxs,
		xlab="", ylab="",z=zi, 
		main=main,
		breaks=breaks, oldstyle=oldstyle,
#		frame.plot=FALSE,	
		...)

#	image(
#		1:ncol(z), 1:nrow(z),z=t(z),zlim=zlim,
#		xlim=xlim,
#		ylim=c(nrow(z)+0.5,0.5), 
#	)

	if (names) {
		textnames(zi,coloffs=-4,rowoffs=1)
	} # names
}#imagem

#imagem(mm, main="mm")
#imagem(t(mm), main="t(mm)")
