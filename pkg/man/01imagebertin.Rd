\name{image.bertin}
\alias{image.bertin}
\title{Display Bertin image}
\description{Display a Bertin matrix image.
}
\details{A matrix is displayed, with values color encoded by \code{col}.
Optional permutations can be applied to rows and/or columns.
}
\synopsis{
	image.bertin(z, roworder,colorder,
	main="",col = gray((0:8 / 8)^0.5)
}
	
\arguments{
	\item{z}{a data matrix.}
	\item{roworder, colorder}{Permutation or selection vector for rows or columns to display.}
	\item{main}{Main plot title.}
	\item{col}{A color palette to encode the data.}
}

\references{
J. Bertin: \emph{La graphique et le traitement graphique de l'information.} Flammarion: Paris 1977. 

A. de Falguerolles; F. Friedrich & G Sawitzki: \emph{A Tribute to J. Bertin's Graphical Data Analysis.}
Published in: W. Bandilla, F. Faulbaum (eds.): SoftStat '97. Advances in Statistical Software 6. 
Lucius & Lucius, Stuttgart 1997. (provided in the doc section of this package)
}

\author{G. Sawitzki}
\examples{
data(Hotel)
hotelrks <- bertinrank(Hotel)
image.bertin(hotelrks)
}

\keyword{hplot}
% rm tmp.pdf;  R CMD Rd2dvi --pdf --output=tmp.pdf "/Users/gs/vy/man/imagebertin.Rd" ; open tmp.pdf



