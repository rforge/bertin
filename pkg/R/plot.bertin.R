#$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/bertin/pkg/R/bertin.R $
#$Id: bertin.R 25 2011-09-06 11:19:19Z gsawitzki $
#$Revision: 25 $
#$Date: 2011-09-06 13:19:19 +0200 (Tue, 06 Sep 2011) $
#$Author: gsawitzki $

#args(plot.default)
#function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL, 
#   log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
#    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, 
#    panel.last = NULL, asp = NA, ...) 
    
plot.bertin <- function(z, roworder,colorder, var.orientation=c("byrow", "bycolumn", "global"),
	main="", zcol, palette = gray((255:0 / 255)^0.5))
{#$Revision: 25 $
bzcol <- function(v) {
	vcol <- imagecolindex(v,ncolour=length(palette))
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
		col= palette[zcol[roworder,colorder]])
	#bertinrect(z[roworder,colorder], main=main, col= zcol)
	palette(oldpalette)

	attr(z,"roworder")<-roworder
	attr(z,"colorder")<-colorder
	attr(z,"main")<-main
	attr(z,"zcol")<-zcol
	attr(z,"var.orientation") <- var.orientation
	invisible(z)
}