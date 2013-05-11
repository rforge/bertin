#$Id: 00bertinworksheet.R 77 2013-05-09 14:02:45Z gsawitzki $
overlaygroup <- function (group, 
pos, 
byrow=TRUE, 
col="green", lwd=2,...)
{
	if (!missing(group)) {
		n<-(length(group))
		x <- 1:(length(group)-1)
		x <- x [group[-n] != group[-1]]
		if (!missing(pos) && (x!=pos)) {stop("group and pos have conflicting information.")}
		pos <- x
	}
	if (byrow) {
		abline(h=pos, col= col, lwd=lwd)

	} else {
				abline(v=pos, col= col, lwd=lwd)
	}
	pos
 }


# group <- sort(rep( (1:4),3))
# overlaygroup(dgroup,col="blueviolet")
