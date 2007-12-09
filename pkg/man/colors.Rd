\name{Palettes}
\title{Various color palettes}
\alias{Palettes}
\alias{vyColors}
\alias{green.colors}
\alias{red.colors}
\alias{blue.colors}
\alias{blueyellow.colors}
\alias{blueyellow2.colors}
\alias{blueyellow4.colors}
\alias{bluered2.colors}
\alias{bluered4.colors}
\alias{greenred.colors}
\alias{greenred2.colors}
\alias{greenred4.colors}
\alias{loggray.colors}
\alias{tail.colors}
\alias{jet.colors}
\alias{wh.colors}
\description{Color palettes for visualization of statistics.
These functions create a vector of \code{n} ``contiguous'' colors.
}

\usage{
green.colors(n=100, rev=FALSE)
red.colors(n=100, rev=FALSE)
blue.colors(n=100, rev=FALSE)

blueyellow.colors(n=100, rev=FALSE)
blueyellow2.colors(n=100, rev=FALSE)
blueyellow4.colors(n=100, rev=FALSE)

greenred.colors(n=100, rev=FALSE)
greenred2.colors(n=100, rev=FALSE)
greenred4.colors(n=100, rev=FALSE)

loggray.colors(n,p=2, rev=FALSE)

tail.colors(n=100, q1=0.125, q2=0.25, q3=1-q2,q4=1-q1, rev=FALSE)

}
	
\arguments{
\item{n}{the number of colors (\eqn{\ge 1}{>= 1}) to be in the palette.}
\item{rev}{flag to revert color sequence.}
\item{p}{power}
\item{q1, q2, q3, q4}{cut points}
}

\value{A character vector, \code{cv}, of color names.  This can be used
  either to create a user--defined color palette for subsequent
  graphics by \code{\link{palette}(cv)}, a \code{col=} specification
  in graphics functions or in \code{par}.
}


\details{
  Conceptually, all of these functions actually use (parts of) a line
  defined by a start and an end colour.
  
  At present, to avoid visual artefacts, the blueyellow family is most recommended.
  
  Some palettes come in families. These are variants to reduce the sensory impact of
  middle values by reducing the saturation. For example, blueyellow2.colors uses a quadratic
  degradation, blueyellow4.colors a fourth order degradation.
  
  The red, green and blue color palettes provide a range of shades of that colors.
  
  The loggray.colors provide logarithmic gray scales.
  
  The tail.colors is a special facility to mark certain quantiles.
  
}

\examples{
par(mfrow=c(2,1)); colramp(green.colors(100)); colramp(green.colors(100, rev=TRUE))
par(mfrow=c(2,1)); colramp(red.colors(100)); colramp(red.colors(100, rev=TRUE))
par(mfrow=c(2,1)); colramp(blue.colors(100)); colramp(blue.colors(100, rev=TRUE))

par(mfrow=c(2,1)); colramp(blueyellow.colors(100)); colramp(blueyellow.colors(100, rev=TRUE))
par(mfrow=c(2,1)); colramp(blueyellow2.colors(100)); colramp(blueyellow2.colors(100,rev=TRUE))
par(mfrow=c(2,1)); colramp(blueyellow4.colors(100)); colramp(blueyellow4.colors(100,rev=TRUE))

par(mfrow=c(2,1)); colramp(bluered2.colors(100)); colramp(bluered2.colors(100,rev=TRUE))
par(mfrow=c(2,1)); colramp(bluered4.colors(100)); colramp(bluered4.colors(100,rev=TRUE))

par(mfrow=c(2,1)); colramp(greenred2.colors(100)); colramp(greenred2.colors(100,rev=TRUE))
par(mfrow=c(2,1)); colramp(greenred4.colors(100)); colramp(greenred4.colors(100,rev=TRUE))

par(mfrow=c(2,1)); colramp(tail.colors(100)); colramp(tail.colors(100,rev=TRUE))
}
\seealso{
  \code{\link{Palettes}},
  \code{\link{colors}}, \code{\link{palette}},  \code{\link{hsv}},
  \code{\link{rgb}}, \code{\link{gray}} and \code{\link{col2rgb}} for
  translating to RGB numbers.
}

\author{G. Sawitzki}

\keyword{color}
\keyword{dplot}
