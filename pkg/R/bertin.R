#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# br <- bertinrank(Brmatrix)
########################
# bertin matrix support
########################

ordercor <- function(z, pivot, var.orientation=c("byrow", "bycolumn") ){
	var.orientation <- match.arg(var.orientation)
	
	ord <- switch(var.orientation,
		byrow=order(cor(t(bertinrank(z)))[pivot,]),
		bycolumn=order(cor(bertinrank(z))[,pivot])
	)
	ord
	}

#!  order rows/columns by their mean
ordermean <- function(z, var.orientation=c("byrow", "bycolumn") ){
	var.orientation <- match.arg(var.orientation)
	
	ord <- switch(var.orientation,
		byrow= order(cor(t(bertinrank(z)))[pivot,]),
		bycolumn=order(cor(bertinrank(z))[,pivot])
	)
	ord
	}


bertinrank <- function (z, var.orientation=c("byrow", "bycolumn", "global"), na.last = TRUE,
     ties.method = c("average", "first", "random", "max", "min"))
{
branks <- function(v)
#! find proper rank correction for missing data
{
	vr <- rank(v, na.last, ties.method)
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

	attr(scores,"class") <- "bertin"
	scores
}

bertinrank37 <- function (z, var.orientation=c("byrow", "bycolumn", "global"), ...)
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

bertinzscore <- function (z, var.orientation=c("byrow", "bycolumn", "global"), trim = 0, na.rm = FALSE, ...)
{
bzscore <- function(v)
#! find proper correction for missing data
{   mn <- mean(v, trim, na.rm,...)
	sd <- sd(v,na.rm)
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

	attr(scores,"class") <- "bertin"
	scores
}


bertinrangescore <- function (z, var.orientation=c("byrow", "bycolumn", "global"), na.rm=TRUE, finite=FALSE)
{
brangescore <- function(v)
#! find proper correction for missing data
{   r <- range(v, na.rm=na.rm, finite=finite)
	sc <- r[2]-r[1]
	if (sc==0) {sc <- 1}
	v <- (v-r[1])/sc
	v
} # brangescore
var.orientation <- match.arg(var.orientation)
if (is.null(var.orientation)) var.orientation <- attr(z,"var.orientation")
if (is.null(var.orientation)) var.orientation <- "byrow"
scores <- switch(var.orientation, 
	byrow= t(apply(z,1, brangescore)),
	bycolumn= apply(z,2, brangescore),
	global= brangescore(z)
	)
	# maybe ranking has destroyed attributes
	dim(scores) <- dim(z)
	colnames(scores) <- colnames(z)
	rownames(scores) <- rownames(z)	

	attr(scores,"var.orientation") <- var.orientation

	attr(scores,"class") <- "bertin"
	scores
}


#37
bertinzscore37 <- function (z, var.orientation=c("byrow", "bycolumn", "global"), trim = 0, na.rm = FALSE, ...)
{
bzscore <- function(v)
#! find proper rank correction for missing data
{   mn <- mean(v, trim, na.rm,...)
	sd <- sd(v,na.rm)
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


