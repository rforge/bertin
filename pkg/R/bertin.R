#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# br <- bertinrank(Brmatrix)
########################
# bertin matrix support
########################
bertinrank <- function (z, var.orientation=c("byrow", "bycolumn", "global"), ...)
{
branks <- function(v)
#! find proper rank correction for missing data
{
	vr <- rank(v)
	nrna <- length(v[is.na(v)])
	if (nrna > 0) {
		vr<- vr + nrna/(2*length(v))
		vr[is.na(v)] <- NA
	}
	vr
} # branks
var.orientation <- match.arg(var.orientation)
scores <- switch(var.orientation, 
	byrow= t(apply(z,1,branks)),
	bycolumn= apply(z,2,branks),
	global=branks(z)
	)
	# maybe ranking has destroyed attributes
	dim(scores) <- dim(z)
	colnames(scores) <- colnames(z)
	rownames(scores) <- rownames(z)	

	attr(scores,"var.orientation") <- var.orientation
	## inline, to avoid copying -- could go to a function
	xx <- apply(scores,1,mean,na.rm=TRUE)
	names(xx) <- rownames(z)
	attr(scores,"rowmeans") <- xx
	xx <- order(xx)
	names(xx) <- rownames(z)[xx]
	attr(scores,"roworder") <- xx

	xx <- apply(scores,2,mean,na.rm=TRUE)
	names(xx) <- colnames(z)
	attr(scores,"colmeans") <- xx
	xx <- order(xx)
	names(xx) <- colnames(z)[xx]
	attr(scores,"colorder") <- xx

	attr(scores,"class") <- "bertin"
	scores
}

bertinzscore <- function (z, var.orientation=c("byrow", "bycolumn", "global"), ...)
{
bzscore <- function(v)
#! find proper rank correction for missing data
{   mn <- mean(v)
	sd <- sd(v)
	v <- if (sd != 0)  (v-mn)/sd else 0
	v
} # branks
var.orientation <- match.arg(var.orientation)
scores <- switch(var.orientation, 
	byrow= t(apply(z,1, bzscore)),
	bycolumn= apply(z,2, bzscore),
	global= bzscore(z)
	)
	# maybe ranking has destroyed attributes
	dim(scores) <- dim(z)
	colnames(scores) <- colnames(z)
	rownames(scores) <- rownames(z)	

	attr(scores,"var.orientation") <- var.orientation
	## inline, to avoid copying -- could go to a function
	xx <- apply(scores,1,mean,na.rm=TRUE)
	names(xx) <- rownames(z)
	attr(scores,"rowmeans") <- xx
	xx <- order(xx)
	names(xx) <- rownames(z)[xx]
	attr(scores,"roworder") <- xx

	xx <- apply(scores,2,mean,na.rm=TRUE)
	names(xx) <- colnames(z)
	attr(scores,"colmeans") <- xx
	xx <- order(xx)
	names(xx) <- colnames(z)[xx]
	attr(scores,"colorder") <- xx

	attr(scores,"class") <- "bertin"
	scores
}

bertin.ranks0 <-
function (z,byrow=FALSE)
{
branks <- function(v)
#! find proper rank correction for missing data
{
	vr <- rank(v)
	nrna <- length(v[is.na(v)])
	if (nrna > 0) {
		vr<- vr + nrna/(2*length(v))
		vr[is.na(v)] <- NA
	}
	vr
} # branks

	if (byrow){scores <- t(apply(z,1,branks))
	colnames(scores) <- colnames(z)
	} else {
	scores <-  apply(z,2,branks)
	rownames(scores) <- rownames(z)
	}

	## inline, to avoid copying -- could go to a function
	xx <- apply(scores,1,mean,na.rm=TRUE)
	names(xx) <- rownames(z)
	attr(scores,"rowmeans") <- xx
	xx <- order(xx)
	names(xx) <- rownames(z)[xx]
	attr(scores,"roworder") <- xx

	xx <- apply(scores,2,mean,na.rm=TRUE)
	names(xx) <- colnames(z)
	attr(scores,"colmeans") <- xx
	xx <- order(xx)
	names(xx) <- colnames(z)[xx]
	attr(scores,"colorder") <- xx

	attr(scores,"class") <- "bertin"
	scores
}

image.bertin <- function(z, roworder,colorder,
	main="",col = gray((255:0 / 255)^0.5))
{
##(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
##    z, zlim = range(z[is.finite(z)]), xlim = range(x[is.finite(x)]), 
##    ylim = range(y[is.finite(y)]), col = heat.colors(12), add = FALSE, 
##    xaxs = "i", yaxs = "i", xlab, ylab, breaks, oldstyle = FALSE, 
##    ...) 
##
	if (missing(roworder)) {
	roworder <- attr(z,"roworder")
	if (is.null(roworder) ) roworder <- 1:nrow(z)
	}

	if (missing(colorder)) {
	colorder <- attr(z,"colorder")
	if (is.null(colorder) ) colorder <- 1:ncol(z)	
	}

	imagem(z[roworder,colorder],main=main,col=col)
}

plot.bertin <- function(z, roworder,colorder,
	main="",col = gray((255:0 / 255)^0.5))
{
	bertinrect(z,main=main,col=col)
}