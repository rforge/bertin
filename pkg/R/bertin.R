########################
# bertin matrix support
########################

bertin.ranks <-
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

image.bertin <- function
##(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
##    z, zlim = range(z[is.finite(z)]), xlim = range(x[is.finite(x)]), 
##    ylim = range(y[is.finite(y)]), col = heat.colors(12), add = FALSE, 
##    xaxs = "i", yaxs = "i", xlab, ylab, breaks, oldstyle = FALSE, 
##    ...) 
##
(z, roworder,colorder,
	main="",col = gray((255:0 / 255)^0.5)
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
