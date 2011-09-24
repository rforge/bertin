#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

#args(plot.default)
#function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL, 
#   log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
#    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, 
#    panel.last = NULL, asp = NA, ...) 
    
plot.bertin <- function(z, roworder,colorder, var.orientation=c("byrow", "bycolumn", "global"),
	main, 
	sub,
	mar= c(1,1,3,1)+0.1, # default mar= c(1,1,6,4)+0.1,
	zcol, 
	palette = gray((255:0 / 255)^0.5),
	showpalette=TRUE, ...)
{
#$Revision$
bzcol <- function(v) {
	vcol <- bertin:::imagecolindex(v, ncolour=length(palette))
	vcol
} # bzcol

	if (missing(var.orientation)) {
	var.orientation <-  attr(z,"var.orientation")
	if (is.null(var.orientation) ) var.orientation <- "byrow"
	}
	 else {
	var.orientation <- match.arg(var.orientation)
	}
		
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

	if (missing(sub)) {
	sub <- attr(z,"sub")
	}
	
	if (missing(zcol)) {
	zcol <- attr(z,"zcol")
	if (is.null(zcol) ) {
		zcol <- 
		switch(var.orientation, 
	byrow= t(apply(z,1, bzcol)),
	bycolumn= apply(z,2, bzcol),
	global= imagecolindex(z,ncolour=length(palette))
	)
	}
	}
#! check whether zcol is in the range of palette!

	oldpalette <-palette(palette)
	bertinrect(z[roworder,colorder], 
		main=main, 
		col= palette[zcol[roworder,colorder]], mar=mar,...)
	#bertinrect(z[roworder,colorder], main=main, col= zcol)
	palette(oldpalette)

	if (!is.null(sub)) {title(sub=sub, line=1)}
	if (showpalette) {bertin:::colrampmarg(palette, main="Score Colour Codes (by %)")}
#! improve. feed back user scale

	attr(z,"roworder")<-roworder
	attr(z,"colorder")<-colorder
	attr(z,"main")<-main
	attr(z,"zcol")<-zcol
	attr(z,"var.orientation") <- var.orientation
	invisible(z)
}