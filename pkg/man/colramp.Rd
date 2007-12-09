\name{colramp}
\alias{colramp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Displays a horizontal or vertical color ramp.}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colramp(col = heat.colors(256), nr = 256, horizontal = FALSE, main, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{col}{A color palette.}
	\item{nr}{Number of steps in display.}	
	\item{horizontal}{Orientation. Defaults to FALSE.}
  	\item{main}{Passed to \code{plot}}
	\item{...}{Additional parameters for \code{plot}.}
}
% \details{ If necessary, more details than the description above }

%\references{ put references to the literature/web site here  }
\author{G. Sawitzki}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (col=  heat.colors(256),  nr=256, horizontal=FALSE, main,...)
{
#! add support for functions passed as col
#! add support for translated scale, e.g. log
if (is.function(col) )
{
#cat(deparse(col),nr)
stop("function support not yet implemented")
#eval(call(col,nr))
  }
if (missing(main)) main<-deparse(substitute(col))
if (missing(nr)) nr<-length(col)
if (horizontal)
	{
	a <- matrix(nrow= nr, ncol= 1)
	a[,1]<-c(1:nr)
	oldpar <- par(yaxt="n")
	image(,,a,col=col,main=main,...)
	} else
	{
	a <- matrix(ncol= nr, nrow= 1)
	a[1,]<-c(1:nr)
	oldpar <- par(xaxt="n")
	image(,,a,col=col,main=main,...)
	}
	par(oldpar)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{color}
