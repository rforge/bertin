# Generate a plot window
plot.empty <- function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL, 
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, 
    panel.last = NULL, asp = NA, ...) 
{
    localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
    localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
    localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
    localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    ylabel <- if (!missing(y)) 
        deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel, log)
    xlab <- if (is.null(xlab)) 
        xy$xlab
    else xlab
    ylab <- if (is.null(ylab)) 
        xy$ylab
    else ylab
    xlim <- if (is.null(xlim)) 
        range(xy$x[is.finite(xy$x)])
    else xlim
    ylim <- if (is.null(ylim)) 
        range(xy$y[is.finite(xy$y)])
    else ylim
    plot.new()
    localWindow(xlim, ylim, log, asp, ...)
    panel.first
 #   plot.xy(xy, type, ...)
    panel.last
    if (axes) {
        localAxis(if (is.null(y)) 
            xy$x
        else x, side = 1, ...)
        localAxis(if (is.null(y)) 
            x
        else y, side = 2, ...)
    }
    if (frame.plot) 
        localBox(...)
    if (ann) 
        localTitle(main = main, sub = sub, xlab = xlab, ylab = ylab, 
            ...)
    invisible()
}



plot.empty(c(0,1),c(0,1))
## or just use parametere:
plot(c(0, 1), c(0, 1), type= "n", xlab="", ylab="", axes=FALSE)
# op <- par(bg = "thistle")
# par(op)
# bottom, delta of row 
rowlim <- function(row, nrrows) { c((row-1)/nrrows, 1/nrrows)} 
# left, delta of column 
collim <- function(col, nrcols) { c((col-1)/nrcols, 1/nrcols)} 

# This version requires normalized x (values in [0,1])
bertinrects <- function(x,...){
# operates by column= R convention
# bottom left
xleft <- matrix((0:(ncol(x)-1))/ncol(x),nrow(x),ncol(x),byrow=TRUE)
xright <- xleft+1/ncol(x)
xbottom <-  matrix((0:(nrow(x)-1))/nrow(x),nrow(x),ncol(x))
xtop <- x/nrow(x)+xbottom
rect(xleft,xbottom,xright,xtop,...)
}



# This version should work for signed x (values in [-1,1])
bertinrectssigned <- function(x,sepwd=0.1,...){
# operates by column= R convention
main <- deparse(substitute(x))
plot(c(0, 1), c(0, 1), main=main,type= "n", xlab="", ylab="", axes=FALSE)

# bottom left
xleft <- matrix((0:(ncol(x)-1))/ncol(x),nrow(x),ncol(x),byrow=TRUE)+sepwd/(2*ncol(x))
xright <- xleft+1/ncol(x)-sepwd/(2*ncol(x))

xbottom <-  matrix((0:(nrow(x)-1))/nrow(x),nrow(x),ncol(x)) + 0.5/nrow(x)+sepwd/(2*nrow(x))
xtop <- 0.5*x*(1-sepwd)/nrow(x)+xbottom
rect(xleft,xbottom,xright,xtop,...)
}

# This version should work for signed x (values in [-1,1])
bertinrectssigned <- function(x,sepwd=0.1,...){
# operates by column= R convention
# [i,j] bottom left is (i,j)
main <- deparse(substitute(x))
oldpar <- par(mar=c(2,2,8,8)+0.1)
plot(c(1, ncol(x)+1), c(1, nrow(x)+1), main=main,type= "n", xlab="", ylab="", axes=FALSE)

# bottom left
xleft <- matrix((1:ncol(x)), nrow(x), ncol(x),byrow=TRUE)+sepwd
xright <- xleft+1-sepwd

xbottom <-  matrix((1:nrow(x)),nrow(x),ncol(x) + sepwd)
xtop <- 0.5*x*(1-sepwd)+xbottom
rect(xleft,xbottom,xright,xtop,...)

}#bertinrectssigned

bertinrectssigned <- function(x,sepwd=0.1,...){
# operates by column= R convention
# [i,j] bottom left is (i,j)
main <- deparse(substitute(x))
oldpar <- par(mar=c(2,2,8,8)+0.1)
plot(c(1, ncol(x)+1), c(1, nrow(x)+1), main=main,type= "n", xlab="", ylab="", axes=FALSE)

# bottom left
xleft <- matrix((1:ncol(x)), nrow(x), ncol(x),byrow=TRUE)+sepwd
xright <- xleft+1-sepwd

xbottom <-  matrix((1:nrow(x)),nrow(x),ncol(x) + sepwd)
xtop <- 0.5*x*(1-sepwd)+xbottom
rect(xleft,xbottom,xright,xtop,...)

}}#bertinrectssigned



# bertinrectssigned(x)
bertinrectssigned(Brmatrix, col=rainbow(12))
bertinrectssigned(Brmatrix, density=0.1) # bad
bertinrectssigned(Brmatrix, border=TRUE,lwd=2, sepwd=0.4) 
bertinrectssigned(Brmatrix, col=rainbow(2))

textnames <- function(zi, coloffs = -1, rowoffs = NULL) {
       if (!is.null(colnames(zi))){ for (x in (1:dim(zi)[2])) text(x+0.8, ncol(zi)+1, colnames(zi)[x], 
            pos = 3, xpd = NA, offs = coloffs, srt = 90)}
        
       if (!is.null(rownames(zi))) {
       	r <- par("usr")[2] #right
       	for (y in (1:dim(zi)[1])) text(r, y, rownames(zi)[y], 
            pos = 4, xpd = NA, offs = rowoffs, srt = 0)
}    }

bertinrectssigned(Brmatrix, border=TRUE,lwd=2, sepwd=0.4) 
textnames(Brmatrix,coloffs=1, rowoffs=0)


# This version should work for any numeric x
bertinrect <- function(x,
	sepwd=0.05,
	mar= c(4,2,6,6)+0.1,
	...){
	# [i,j] bottom left is at user coordinates (i,j)
	# sepwd is internal margin
	main <- deparse(substitute(x))
	oldpar <- par(mar=mar)
	plot(c(1, ncol(x)+1), c(nrow(x)+1,1), 
	main=main,type= "n", xlab="", ylab="", axes=FALSE)

	x <- as.matrix(x) #! support lists and data frames as well

	#! improve. use scaling as in plot.window
	ranges <- apply(x,1,range,finite=TRUE) # transposed
	ranges[1,] <- pmin(0,ranges[1,]) #tack at zero
	ranges[2,] <- pmax(0,ranges[2,]) #tack at zero
	scale <- ranges[2,] - ranges [1,]
	scale[] <- ifelse(scale[]==0,0.1,scale[])
	scale[!is.finite(scale[] )] <- 0.1 	# fix zero scales
	scale[scale[]==0] <- 0.1
	#add some margin. ! improve. use scaling as in plot.window
	#for now, is handled by sepwd
	#ranges[1,] <- ranges[1,]- 0.04*scale[]
	#ranges[2,] <- ranges[2,]+ 0.04*scale[]
	scale <- (1-2*sepwd)/scale
	zeroline <- -ranges[1,]*scale

	xleft <- matrix((1:ncol(x)), nrow(x), ncol(x),byrow=TRUE)+sepwd
	xright <- xleft+1-2*sepwd

	xbottom <- (matrix((1:nrow(x)),nrow(x),ncol(x))) + 
		sepwd +zeroline #box zero line
	xtop <- x*scale+xbottom

if (any(ranges[2,]==0)){
	abline(h= xbottom[ranges[2,]==0],lty=3,col="gray")
}
	rect(xleft,xbottom,xright,xtop,...)

	#textnames(x)
       if (!is.null(colnames(x))){ 
       	for (col in (1:dim(x)[2])) 
       		text(col+0.8, nrow(x)+1, colnames(x)[col], 
            pos = 3, xpd = NA, offs = 1, srt = 90, cex=0.6)}
        
       if (!is.null(rownames(x))) {
       	r <- par("usr")[2] #right
       	for (row in (1:dim(x)[1])) 
       		text(r, row, rownames(x)[row], 
            pos = 4, xpd = NA, offs = rowoffs, srt = 0, cex=0.6)
            
   if (!all(is.finite(x))) {
   	badpos <- !is.finite(x)
   	xbottom <-  0.4+nrow(x)-(matrix((1:nrow(x)),nrow(x),ncol(x))) + sepwd #box zero line
   	text(xleft[badpos]+0.4,xbottom[badpos],x[badpos], pos=3, offs=0,col="red",cex=0.6)
   }
} 
}

bertinrect(Hotel)
bertinrect(Brmatrixx)

