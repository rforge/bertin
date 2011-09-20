%#$Id$
\name{image.bertin}
\alias{image.bertin}
\title{Display Bertin image}
\description{Display a Bertin matrix image.
}
\details{A matrix is displayed, with values color encoded by \code{col}.
Optional permutations can be applied to rows and/or columns. The layout of the original data matrix is preserved in the display.
}
\synopsis{
	image.bertin(z, roworder,colorder,
	main="",
	col = gray((0:8 / 8)^0.5), 
	useRaster = FALSE,
	...
}
	
\arguments{
	\item{z}{a data matrix.}
	\item{roworder, colorder}{Permutation or selection vector for rows or columns to display.}
	\item{main}{Main plot title.}
	\item{col}{A color palette to encode the data.}
	\item{useRaster}{logical; if TRUE a bitmap raster is used to plot the image instead of polygons. The grid must be regular in that case, otherwise an error is raised.  \code{TRUE}  seems to give aliasing problems.}
	 \item{\dots}{additional parameters, passed to \code{image}.}
}

\value{
A data matrix with display specification added as attributes. Details are subject to change.
This is  \code{\link{invisible}}.
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
}

\references{
J. Bertin: \emph{La graphique et le traitement graphique de l'information.} Flammarion: Paris 1977. 

A. de Falguerolles; F. Friedrich & G Sawitzki: \emph{A Tribute to J. Bertin's Graphical Data Analysis.}
Published in: W. Bandilla, F. Faulbaum (eds.): SoftStat '97. Advances in Statistical Software 6. 
Lucius & Lucius, Stuttgart 1997. (provided in the doc section of this package)
}

\author{G. Sawitzki}

\note{
Still experimental. This is a \pkg{bertin} specific wrapper for   \code{\link{imagem}}, which is a wrapper for .\code{\link{image}}. 
}

\examples{
data(Hotel)
hotelrks <- bertinrank(Hotel)
image.bertin(hotelrks)
}

\keyword{hplot}
% rm tmp.pdf;  R CMD Rd2dvi --pdf --output=tmp.pdf "/Users/gs/vy/man/imagebertin.Rd" ; open tmp.pdf



