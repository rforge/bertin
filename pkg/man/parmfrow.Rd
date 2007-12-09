\name{parmfrow}
\alias{parmfrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ function to do ...  }
\description{
   A concise (1-5 lines) description of what the function does. 
}
\usage{
parmfrow(dims, aspr = dims[1]/dims[2], minin = 0.25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dims}{ Describe \code{dims} here }
  \item{aspr}{ Describe \code{aspr} here }
  \item{minin}{ Describe \code{minin} here }
}
% \details{ If necessary, more details than the description above }
\value{
  Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ put references to the literature/web site here  }
\author{ who you are }
\note{ further notes 

 Make other sections like Warning with \section{Warning }{....} 
}
\seealso{ objects to See Also as \code{\link{help}},  }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dims, aspr=dims[1]/dims[2], minin=0.25)
# all internal aspect ratios ar in dy/dx form.
# note: dims is in (y,x) order as returned by dim
# dim is in row,col
# din is in x,y
# image shows matrix in transposed flipped form
{
 	if (aspr<= 0) stop("parmfrow: aspr must be positive")
	if (minin<= 0.1) stop("parmfrow: mini must be at least 0.1 inch")
	din<-par("din")
	dar <- din[2]/din[1]
	if (aspr>1) {
		#print("matrix high") 
		minx <- minin; miny <- minin/aspr} else
		{
		#print("matrix wide")
		minx <- minin*aspr; miny <-   minin
		}
	#cat(din,dar,"\n")
	#if (dar>1) print("device high") else print("device wide")

	nx <- floor(din[1] / minx)
	ny <- floor(din[2] / miny)
	#cat(nx,ny,"\n")
	if (interactive()) 
		par(mfrow=c(ny,nx), ask=TRUE) else par(mfrow=c(ny,nx))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
