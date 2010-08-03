image.bertin <-
function
##(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
##    z, zlim = range(z[is.finite(z)]), xlim = range(x[is.finite(x)]), 
##    ylim = range(y[is.finite(y)]), col = heat.colors(12), add = FALSE, 
##    xaxs = "i", yaxs = "i", xlab, ylab, breaks, oldstyle = FALSE, 
##    ...) 
##
(z, roworder,colorder,
	main="",col = gray((0:8 / 8)^0.5)
)
{	if (missing(roworder)) {
	roworder <- attr(z,"roworder")
	if (is.null(roworder) ) roworder <- 1:nrow(z)
	}

	if (missing(colorder)) {
	colorder <- attr(z,"colorder")
	if (is.null(colorder) ) colorder <- 1:ncol(z)	
	}

	imagem(z[roworder,colorder],main=main,col=col)
}

