\name{bertinrangescore}
\alias{bertinrangescore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Range standardisation of a matrix
}
\description{
Calculate range standardized matrix  for Bertin analysis.
}
\usage{
bertinrangescore(z, var.orientation = c("byrow", "bycolumn", "global"), na.rm = TRUE, finite = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{z}{A data matrix}
	\item{var.orientation}{"byrow", "bycolumn", "global"). By defaults, following Bertin's conventions variables are in rows. Use
	\code{var.orientation="bycolumn"} for \R{} conventions.}
  \item{na.rm}{passed to  \code{\link{range}}}
  \item{finite}{passed to  \code{\link{range}}}
}
\details{Calculates \code{(x-min(x))/ ( max(x)-min(x)}. If \code{range(x)} has zero length, one is returned as value.}
\value{
	\item{scores}{An augmented matrix of rank scores.
	The matrix has the class "bertin" and carries the attributes:
	\describe{
	\item{var.orientation}{cached.}
}
	This list is subject to change. 
}
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{G. Sawitzki}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{bertinrank}}, \code{\link{bertinzscore}}.}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar }
