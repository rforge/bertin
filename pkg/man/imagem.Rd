\name{imagem}
\alias{imagem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{imagem}
\description{A variant of image, adapted to matrices. \code{imagem} keeps the aspect ratio and orientation of the matrix.
Row names and column names are shown if available. Arguments correspond tp those of \code{image} as far as possible.}
\usage{
imagem(z, 
	zlim = range(z[is.finite(z)]), 
	xlim = c(1, ncol(z)), 
	ylim = c(1, nrow(z)), 
	col = heat.colors(12), 
	add = FALSE, 
	xaxs = "i", yaxs = "i", 
	xlab, ylab, 
	main, 
	breaks, 
	oldstyle = FALSE, 
	names = TRUE, coloffs = -1, rowoffs = 4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{a matrix containing the values to be plotted (\code{NA}s are
    allowed).  Note that \code{x} can be used instead of \code{z} for
    convenience.}
  \item{zlim}{the minimum and maximum \code{z} values for which colors
    should be plotted, defaulting to the range of the finite values of
    \code{z}. Each of the given colors will be used to color an
    equispaced interval of this range. The \emph{midpoints} of the
    intervals cover the range, so that values just outside the range
    will be plotted.}
  \item{xlim, ylim}{ranges for the plotted \code{x} and \code{y} values,
    defaulting to the ranges of \code{x} and \code{y}.}
   \code{\link{topo.colors}}, \code{\link{terrain.colors}} or similar
    functions.}
  \item{add}{logical; if \code{TRUE}, add to current plot (and disregard
    the following four arguments).  This is rarely useful because
    \code{image} \sQuote{paints} over existing graphics.}
  \item{xaxs, yaxs}{style of x and y axis.  The default \code{"i"} is
    appropriate for images.  See \code{\link{par}}.}
  \item{xlab, ylab}{each a character string giving the labels for the x and
    y axis.  Default to the \sQuote{call names} of \code{x} or \code{y}, or to
    \code{""} if these were unspecified.}
  \item{main}{ Main title. }
  \item{mar}{ Margin sizes, passed to \code{par}. Adjust this parameter for row/column names, or change font size. }  
  \item{breaks}{a set of breakpoints for the colours: must give one more
    breakpoint than colour.}
  \item{oldstyle}{logical. If true the midpoints of the colour intervals
    are equally spaced, and \code{zlim[1]} and \code{zlim[2]} were taken
    to be midpoints.  The default is to have colour intervals of equal
    lengths between the limits.}
  \item{names}{ Describe \code{names} here }
  \item{coloffs}{ Describe \code{coloffs} here }
  \item{rowoffs}{ Describe \code{rowoffs} here }
   \item{\dots}{\link{graphical parameters} for \code{\link{plot}} may also be
    passed as arguments to this function, as can the plot aspect ratio
    \code{asp} and \code{axes} (see \code{\link{plot.window}}).}
}
% \details{ If necessary, more details than the description above }
\value{
  Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
%\references{ put references to the literature/web site here  }
\author{G. Sawitzki}
%\note{ further notes 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

\seealso{
  \code{\link{image}} for a general purpose variant.
}
\examples{
nrow <- 3; ncol <- 5
zunif <- matrix(runif(nrow*ncol), nrow, ncol)
colnames(zunif) <- colnames(zunif, do.NULL=FALSE)
 rownames(zunif) <- rownames(zunif, do.NULL=FALSE)
imagem(zunif, yaxt="n"))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{aplot}