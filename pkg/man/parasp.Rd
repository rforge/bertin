\name{parasp}
\alias{parasp}
\title{Set up aspect ratio}
\description{Set aspect ratio match data matrix or given aspect ratio
 The \code{par("pin")} graphics parameter is adjusted to meet the requested aspect ratio
 wihin a chosen tolerance.
}

%\synopsis{	parasp(dat, aspr=dim(dat)[1]/dim(dat)[2], tol=0.01)}
%\usage{\code{opar<-par(no.readonly=TRUE); on.exit(par(opar)); parasp(dat)}}
\usage{
parasp(dat, aspr = dim(dat)[1]/dim(dat)[2], tol = 0.01)
}
	
\arguments{
	\item{dat}{A matrix to be used as a template to define aspr}
	\item{aspr}{Required aspect ratio}
	\item{tol}{Tolerance for \code{par("pin")}}
}
\value{
	\item{pin}{Old parameters from \code{par("pin")}}
}
\author{G. Sawitzki}

\seealso{
\code{\link{par}}
}

\examples{
 parasp(aspr=sqrt(2))	# set to harmonic ratio
 xx <- matrix(ncol=4,nrow=5); parasp(xx)	# set to match row/col ration in xx
## The function is currently defined as
function(dat, aspr=dim(dat)[1]/dim(dat)[2], tol=0.01)
{	if (aspr<= 0) stop("parasp: aspr must be positive")
	pin<-par("pin")
	ar <- pin[1]/pin[2]
	if (abs(ar/aspr)>tol) {
		if (ar < aspr) 
			pin[2] <- pin[1]/aspr  else pin[1] <- pin[2]* aspr
		par(pin=pin)
	}
  }
}
\keyword{dplot}

