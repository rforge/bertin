\name{bertinrank}
\alias{bertinrank}
\title{Calculate row or column ranks for Bertin analysis.}
\description{Calculate row or column rank scores for a given matrix.
	Marginal means and marginal permutation are cached.
}
\usage{
	bertinrank(z,  var.orientation=c("byrow", "bycolumn", "global"),...)

}
	
\arguments{
	\item{z}{A data matrix}
	\item{var.orientation}{"byrow", "bycolumn", "global"). By defaults, following Bertin's conventions variables are in rows. Use
	\code{var.orientation="bycolumn"} for \R{} conventions.}
	\item{\dots}{other parameters,  (see \code{\link{rank}}).}
}
\value{
	\item{scores}{An augmented matrix of rank scores.
	The matrix has the class "bertin" and carries the attributes:
	\describe{
	\item{var.orientation}{cached.}
	\item{rowmeans}{row wise mean ranks}
	\item{roworder}{order of rows by row means}
	\item{colmeans}{column wise mean ranks}
	\item{colorder}{order of columns by column means}
	}
	This list is subject to change. 
	}
}

\author{G. Sawitzki}
\examples{
data(Hotel)
hotelrks <- bertinrank(Hotel)
}

\keyword{univar}

