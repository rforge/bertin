\name{bertin.ranks}
\alias{bertin.ranks}
\title{Calculate row or column statistics for Bertin analysis}
\description{Calculate row or column rank scores for a given matrix.
	Marginal means and order statistics are cached
}
\usage{
	bertin.ranks(z, byrow=FALSE)
}
	
\arguments{
	\item{z}{A data matrix}
	\item{byrow}{Margin for building ranks. Default is to build ranks by column.}
}
\value{
	\item{scores}{An augmented matrix of scores.
	The matrix has the class "bertin" and carries the attributes:
	\describe{
	\item{rowmeans}{row wise mean ranks}
	\item{roworder}{order of rows by row means}
	\item{colmeans}{column wise mean ranks}
	\item{colorder}{order of columns by row means}
	}
	}
}

\author{G. Sawitzki}
\examples{
data(Hotel)
hotelrks <- bertin.ranks(Hotel,byrow=TRUE)

## The function is currently defined as
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
}

\keyword{univar}

