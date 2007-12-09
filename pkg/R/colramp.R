colramp <-
# test color image ramp
function (col=  heat.colors(256),  nr=256, horizontal=FALSE, main,...)
{
#! add support for functions passed as col
#! add support for translated scale, e.g. log
if (is.function(col) )
{
#cat(deparse(col),nr)
stop("function support not yet implemented")
#eval(call(col,nr))
}
if (missing(main)) main<-deparse(substitute(col))
if (missing(nr)) nr<-length(col)
if (horizontal)
	{
	a <- matrix(nrow= nr, ncol= 1)
	a[,1]<-c(1:nr)
	oldpar <- par(yaxt="n")
	image(,,a,col=col,main=main,...)
	} else
	{
	a <- matrix(ncol= nr, nrow= 1)
	a[1,]<-c(1:nr)
	oldpar <- par(xaxt="n")
	image(,,a,col=col,main=main,...)
	}
	par(oldpar)
}
# colramp()
# colramp(horiz=FALSE)

# colramp(heat.colors)
# colramp(heat.colors(128),main="Heat colors")

