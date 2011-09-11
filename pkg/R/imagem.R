#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$
imagem <- 
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
