\name{whatis}
\alias{whatis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inspect an R object}
\description{Another variant to provide a friendly "whatis" facility.}
\usage{
whatis(ob)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ob}{An \R language object.}
}
%\details{  ~~ If necessary, more details than the description above ~~}
\value{
\item{NULL}{At present, whatis is only used for the side effect of listing object information. 
}
}
%\references{ ~put references to the literature/web site here ~ }
\author{G. Sawitzki}
\seealso{
\code{\link{class}}
\code{\link{mode}}
\code{\link{typeof}}
\code{\link{attributes}}
}


\examples{
 whatis(whatis)
 
## The function is currently defined as
function (ob) {
	obname<-deparse(substitute(ob))
	cat(obname)
	cat(" type=",typeof(ob),sep="")
	cat(" mode=",mode(ob),sep="")
	xx <- class(ob); if (!is.null(xx)) cat(paste(" class:",class(ob)))
	cat(paste(" length=",length(ob)))
	xx <- dim(ob); if (!is.null(xx)) cat(" dim=",xx)
	xx <- names(attributes(ob))
		if (!is.null(xx)) 
			cat("\n"," names(attributes(", obname, "))=",xx,xxsep=" ")
	xx <- dimnames(ob)
	if (!is.null(xx)) { 
		xx <- names(xx)
		if (!is.null(xx)) 
			cat("\n"," names(dimnames(",obname,"))=",xx)
	}
	cat("\n")
	if (typeof(ob)=="expression") {
		cat("expression components: ");str(as.list(ob[[1]]))
	}
  } # whatis
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
\keyword{classes}
