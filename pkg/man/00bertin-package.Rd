\name{bertin-package}
\alias{bertin-package}
\alias{bertin}
\docType{package}
\title{Bertin matrices}
\description{
\tabular{ll}{
Package: \tab bertin\cr
Type: \tab Package\cr
Version: \tab 0.1-2xx\cr
First Release Date: \tab 2007-12-08\cr
This Release Date: \tab 2011-01-08\cr
License: \tab LGPL \cr
}

Bertin matrices are a means to analyze structures in a matrix 
using two-dimensional display techniques.

For a start, think of a bertin matrix as a regular display, like in a spreadsheet. Each element of the data matrix is represented in a cell, givin a regular display grid. Instead of a numeric representation, the cell shows a symbolic image - a bar, or a typical token. Now feel fee to change the arrnagement of rows or coumns (i.e  cases and variables, in conventional terms. Usuallay, the sequence of cases and variables is just acccidental, and changes should be free. For the rare other cases, add an indicator to allow for recovery after permutation.

Betin's strategy is to rearrange rows and columns to gain insigtht in the internal data structure,arranging variables and cases by similarity.

This package tries to provide some tools to analyze data using permutation techniques with the help of visualizations.

}
\details{

The details are subject to change. 

The basic functions try to allow as much of user control as possible.

An abstract data structure,  currently matrices of class \code{bertin}, are added to cache information that is presumably invariant over  variatons od display.

At present, a Bertin matrix is a matrix of class \code{bertin} which has has attributes
\describe{
\item{scores}{the matrix values. For example, ranks by row or by column. The original data matrix have to be transformed to comparable scales to allow for permutations. \code{scores} is a shadow matrix in transformed form.}
\item{rowmeans}{a vector of mean values of scores, by row}
\item{colwmeans}{a vector of mean values of scores, by columns}
\item{roworder}{a permutation vector to define the arrangement of rows}
\item{colorder}{a permutation vector to define the arrangement of columns}
}
}
\author{G. Sawitzki

Maintainer: G. Sawitzki <gs@statlab.uni-heidelberg.de>
}

\seealso{Package  \pkg{tableplot}, package \pkg{seriation}.}

\references{
J. Bertin: \emph{La graphique et le traitement graphique de l'information.} Flammarion: Paris 1977. 

A. de Falguerolles; F. Friedrich & G Sawitzki: \emph{A Tribute to J. Bertin's Graphical Data Analysis.}
Published in: W. Bandilla, F. Faulbaum (eds.): SoftStat '97. Advances in Statistical Software 6. 
Lucius & Lucius, Stuttgart 1997. (provided in the doc section of this package)
}

\keyword{ package }
\keyword{ hplot }

\examples{
data(Hotel)
image.bertin(bertinrank(Hotel), main= "Hotel data")
plot.bertin(bertinrank(Hotel), main= "Hotel data")
}

% rm tmp.pdf;  R CMD Rd2dvi --pdf --output=tmp.pdf "/Users/gs/vy/man/AA002Bertin.Rd" ; open tmp.pdf

