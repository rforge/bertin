\name{bertin-package}
\alias{bertin-package}
\alias{bertin}
\docType{package}
\title{Bertin matrices}
\description{Bertin matrices are a means to analyze structures in a matrix 
using two-dimensional display techniques.
Basic data structures for Bertin analysis are implemented as class based on matrix, 
using additional attributes.

The details are subject to change. At present, a Bertin matrix has attributes
\describe{
\item{scores}{the matrix values. Usually ranks by row or by column}
\item{rowmeans}{a vector of mean values of scores, by row}
\item{colwmeans}{a vector of mean values of scores, by columns}
\item{roworder}{a permutation vector to define the arrangement of rows}
\item{colorder}{a permutation vector to define the arrangement of columns}
}
}
\details{
\tabular{ll}{
Package: \tab bertin\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-12-08\cr
License: \tab LGPL \cr
}
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
}
\author{G. Sawitzki

Maintainer: G. Sawitzki <gs@statlab.uni-heidelberg.de>
}
\references{
J. Bertin: \emph{La graphique et le traitement graphique de l'information.} Flammarion: Paris 1977. 

A. de Falguerolles; F. Friedrich & G Sawitzki: \emph{A Tribute to J. Bertin's Graphical Data Analysis.}
Published in: W. Bandilla, F. Faulbaum (eds.): SoftStat '97. Advances in Statistical Software 6. 
Lucius & Lucius, Stuttgart 1997
}

\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}

% rm tmp.pdf;  R CMD Rd2dvi --pdf --output=tmp.pdf "/Users/gs/vy/man/AA002Bertin.Rd" ; open tmp.pdf

