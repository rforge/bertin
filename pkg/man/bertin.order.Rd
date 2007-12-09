\name{bertin.order}
\alias{bertin.order}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate a permutation order}
\description{Generate a permutation order statistics based on FUN}
\usage{
bertin.order(z, MARGIN = 1, pivot = 1, FUN = cor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{z}{a data matrix.}
	\item{MARGIN}{a vector giving the subscripts which the function will be
          applied over. `1' indicates rows, `2' indicates columns.}
	\item{pivot}{Main plot title.}
	\item{FUN}{the function to be applied.}
}
% \details{ If necessary, more details than the description above }
\value{a vector of order statitics}
%\references{ put references to the literature/web site here  }
\author{G. Sawitzki}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (z, MARGIN=1, pivot=1, FUN=cor)
{
if (MARGIN==1)
{ dist <- vector(mode="numeric", length=nrow(z) )
for (i in (1: nrow(z))){
	dist[i] <- FUN(z[pivot,],z[i,])
  }
  } else {
dist <- vector(mode="numeric", length=ncol(z) )
for (i in (1: ncol(z))){
	dist[i] <- FUN(z[,pivot],z[,i])
  }
  }
order(dist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
