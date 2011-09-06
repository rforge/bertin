%$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/bertin/pkg/vignettes/bertinR.Rnw $
%$Id: bertinR.Rnw 17 2011-09-04 12:30:06Z gsawitzki $
%$Revision: 17 $
%$Date: 2011-09-04 14:30:06 +0200 (Sun, 04 Sep 2011) $
%$Author: gsawitzki $

\name{bertinzscore}
\alias{bertinzscore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate standardized matrix}
\description{
Calculate standardized matrix  for Bertin analysis.
	Marginal means and marginal permutation, sorting by score, are cached.
}
\usage{
bertinzscore(z, var.orientation = c("byrow", "bycolumn", "global"), trim = 0, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{z}{A data matrix}
	\item{var.orientation}{"byrow", "bycolumn", "global"). By defaults, following Bertin's conventions variables are in rows. Use
	\code{var.orientation="bycolumn"} for \R{} conventions.}
  \item{trim}{the fraction (0 to 0.5) of observations to be
    trimmed from each end of \code{x} before the mean is computed.
    Values of trim outside that range are taken as the nearest endpoint.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{\dots}{other parameters,  (see \code{\link{mean}}).}}

\details{Calculates \code{(x-mean)/sd}. If \code{sd} is zero, zero is returned as value.}
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
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{G. Sawitzki}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{bertinrank}}.}
\examples{
data(Hotel)
hotelzscores <- bertinzscore(Hotel)
plot.bertin(hotelzscores)
}
\keyword{ univar }% __ONLY ONE__ keyword per line
