#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# colour ramp, as a separate picture. Use setup by layout.
colramp <-
function (col=  heat.colors(256),  nr=100, horizontal=TRUE, main,...)
{
# test color image ramp

#! add support for functions passed as col
#! add support for translated scale, e.g. log
if (is.function(col) )
{
stop("function support not yet implemented")
#eval(call(col,nr))
}
if (missing(main)) main<-deparse(substitute(col))
#if (missing(nr)) nr<-length(col)
if (horizontal)
	{
	a <- matrix(nrow= nr, ncol= 1)
	a[,1]<-c(1:nr)
	oldpar <- par(yaxt="n")
	image(x= (1:nr)/nr*100,, a, xlim=c(1,100), col=col,main="", xlab="",...)
	title(main=main, line=0.5, cex.main=0.8)
	} else
	{
	a <- matrix(ncol= nr, nrow= 1)
	a[1,]<-c(1:nr)
	oldpar <- par(xaxt="n")
	image(,y= (1:nr)/nr*100,z=a, ylim=c(1,100), main="",col=col, ylab="",...)
	title(main=main, line=0.5, cex.main=0.8)
	}
	par(oldpar)
}
# colramp()
#a colramp(horiz=FALSE)
# colramp(col=c("white","black"))

# colour ramp imbedded in picture margin 1
colrampmarg5 <- function(colpalette =  palette(), vals=0:100, main = "scores",...){
	if (missing(main)) main<-deparse(substitute(colpalette))
	oldpar2 <-  par("plt", "mfg", "new")
	plt <- par("plt") # x1,x2, y1, y2
	plt[1] <- plt[1]+0.02
	plt[2] <- plt[2]-0.02
	plt[3] <- 0.05
	plt[4] <- 0.08
	par(plt=plt,  new=TRUE)
#	oldpar1 <- par(no.readonly=TRUE)
#	oldpar <- par(yaxt="n", plt = plt,  new=TRUE)
	nr <- length(vals)
 	a <- matrix(nrow= nr, ncol= 1)
	a[,1]<-c(1:nr)
	# image add=FALSE: new frame. 
	image(x= vals,, a, xlim=range(vals), col= colpalette, main="", xlab="", ylab="", xaxt="n",yaxt="n",add=TRUE,...)		
#	image(x= (1:nr)/nr*100,, a, xlim=c(1,100), col=col, main="", xlab="", ylab="", xaxt="s",padj=-3, tcl=-0.2, cex.axis=0.6, yaxt="n", add=FALSE,...)		
	title(main=main, line=0.2, cex.main=0.6)
	axis(1,padj=-3, tcl=-0.2, cex.axis=0.6)
#	par(oldpar1)
#	par(mfg= oldpar1$mfg)
par(oldpar2)
}


# colour ramp imbedded in picture margin 1
colrampmarg <- function(colpalette =  palette(), vals=0:100, main,...){
	17 + 4
	if (missing(main)) main<-deparse(substitute(colpalette))
	plt <- par("plt") # x1,x2, y1, y2
	plt[1] <- plt[1]+0.02
	plt[2] <- plt[2]-0.02
	plt[3] <- 0.05
	plt[4] <- 0.08
	oldpar1 <- par(no.readonly=TRUE)
	oldpar <- par(yaxt="n", plt = plt,  new=TRUE)
	nr <- length(vals)
 	a <- matrix(nrow= nr, ncol= 1)
	a[,1]<-c(1:nr)
	# image add=FALSE: new frame. 
	image(x= vals,, a, xlim=range(vals), col= colpalette, main="", xlab="", ylab="", xaxt="n",yaxt="n",add=FALSE,...)		
#	image(x= (1:nr)/nr*100,, a, xlim=c(1,100), col=col, main="", xlab="", ylab="", xaxt="s",padj=-3, tcl=-0.2, cex.axis=0.6, yaxt="n", add=FALSE,...)		
	title(main=main, line=0.2, cex.main=0.6)
	axis(1,padj=-3, tcl=-0.2, cex.axis=0.6)
	par(oldpar1) #?? does not set -- see next line
	par(mfg= oldpar1$mfg, pin=oldpar1$pin, plt=oldpar1$plt, new=FALSE)
#	oldpar1
}


colrampmarg3 <- function(palette =  palette(), vals =0:100, main,...){
	if (missing(main)) main<-deparse(substitute(palette))
	plt <- par("plt") # x1,x2, y1, y2
	plt[1] <- plt[1]+0.02
	plt[2] <- plt[2]-0.02
	plt[3] <- 0.05
	plt[4] <- 0.08
	oldpar1 <- par(no.readonly=TRUE)
	oldpar <- par(yaxt="n", plt = plt,  new=TRUE)
	nr <- length(vals) # shift by 1 ?
 	a <- matrix(nrow= nr, ncol= 1)
	a[,1]<-c(1:nr)
#	image(x= (1:nr)/nr*100,, a, xlim=c(1,100), col= palette, main="", xlab="", ylab="", xaxt="n",yaxt="n",add=FALSE,...)		
	image(x= 0:100,, a, xlim=c(0,100), col= heat.colors(256), 
		main="", xlab="", ylab="", xaxt="s",tcl=-0.2,padj=-3, cex.axis=0.6, yaxt="n", add=FALSE,...)		
	title(main=main, line=0.2, cex.main=0.6)
#	axis(1,padj=-3, tcl=-0.2, cex.axis=0.6)
	par(oldpar1)
	par(mfg= oldpar1$mfg)
}

# colour ramp imbedded in picture margin 1
colrampmarg2 <- function(colpalette =  palette(), nr=100, main,...){
	if (missing(main)) main<-deparse(substitute(colpalette))
	plt <- par("plt") # x1,x2, y1, y2
	plt[1] <- plt[1]+0.02
	plt[2] <- plt[2]-0.02
	plt[3] <- 0.05
	plt[4] <- 0.08
	oldpar1 <- par(no.readonly=TRUE)
	oldpar <- par(yaxt="n", plt = plt,  new=TRUE)
 	a <- matrix(nrow= nr, ncol= 1)
	a[,1]<-c(1:nr)
	image(x= (1:nr)/nr*100,, a, xlim=c(1,100), col= colpalette, main="", xlab="", ylab="", xaxt="n",yaxt="n",add=FALSE,...)		
#	image(x= (1:nr)/nr*100,, a, xlim=c(1,100), col=col, main="", xlab="", ylab="", xaxt="s",padj=-3, tcl=-0.2, cex.axis=0.6, yaxt="n", add=FALSE,...)		
	title(main=main, line=0.2, cex.main=0.6)
	axis(1,padj=-3, tcl=-0.2, cex.axis=0.6)
	par(oldpar1)
	par(mfg= oldpar1$mfg)
}


# p <- imagem(Hotel); colrampmarg()

# colrampmarg(main="")
# plot.new()
# p <-par("cin","din","fin","pin","plt","mai", "mar","usr");str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
# colramp(heat.colors)
# colramp(heat.colors(128),main="Heat colors")
# colramp(heat.colors(128),main="Heat colors", horizontal=FALSE)
# colrampmarg(c("black","white"))

colrampmarg1 <- function(col=  heat.colors(256), nr=100, minlab=0, maxlab=100, main,...){
	if (missing(main)) main<-deparse(substitute(col))
	plt <- par("plt") # x1,x2, y1, y2
	plt[1] <- plt[1]+0.02
	plt[2] <- plt[2]-0.02
	plt[3] <- 0.05
	plt[4] <- 0.08
	oldpar1 <- par(no.readonly=TRUE)
	oldpar <- par(yaxt="n", plt = plt,  new=TRUE)#, mar=c(2,0,2,0)+0.1)#,mar=c(1,0,1,0)+0.1)#, mar=c(1,0,0,0)+0.1)plt=c(0,1,0,1), 
#        mar=c(3.5,12.0,0.5,12.0), new=TRUE, xaxt="n", las=1)
 	#colramp(col=col, nr=nr, horizontal=TRUE, ...)
 	a <- matrix(nrow= nr, ncol= 1)
	a[,1]<-c(1:nr)
#	image(x= (1:nr)/nr*100,, a, xlim=c(1,100), col=col,main="", xlab="", cex.axis=0.6, tcl=-0.2, padj=0, add=FALSE,...) #, tcl=0.5
	image(x= (1:nr)/nr*100,, a, xlim=c(1,100), col=col, main="", xlab="", ylab="", xaxt="n",yaxt="n",add=TRUE,...) #, tcl=0.5 ## use current picture
	title(main=main, line=0.5, cex.main=0.6)
	axis(1,padj=-3, tcl=-0.2, cex.axis=0.6)
	cat("par0 ");str(par())
	par(oldpar1)
	cat("oldpar1 ");str(oldpar1)
	cat("par1 ");str(par())
} # colrampmarg1
# p <- imagem(Hotel); colrampmarg()