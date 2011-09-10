#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

##ffrom image:
##(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
##    z, zlim = range(z[is.finite(z)]), xlim = range(x[is.finite(x)]), 
##    ylim = range(y[is.finite(y)]), col = heat.colors(12), add = FALSE, 
##    xaxs = "i", yaxs = "i", xlab, ylab, breaks, oldstyle = FALSE, useRaster = FALSE,
##    ...) 
##

image.bertin <- function
(z, roworder,colorder,
	main, col = gray((255:0 / 255)^0.5),useRaster = TRUE,...)
{#$Revision$
	if (missing(roworder)) {
	roworder <- attr(z,"roworder")
	if (is.null(roworder) ) roworder <- 1:nrow(z)
	}

	if (missing(colorder)) {
	colorder <- attr(z,"colorder")
	if (is.null(colorder) ) colorder <- 1:ncol(z)	
	}

	if (missing(main)) {
	main <- attr(z,"main")
	if (is.null(main) ) main <- deparse(substitute(z))	
	}

	imagem(z[roworder,colorder], main=main, col=col, useRaster=useRaster, ...)

	attr(z,"roworder")<-roworder
	attr(z,"colorder")<-colorder
	attr(z,"main")<-main
	invisible(z)
}

