\name{pdfa4}
\alias{pdfa4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PDF for a4}
\description{Open an a4 PDF port in portrait mode}
\usage{
pdfa4(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ Passed to \code{\link{pdf}}. }
}
% \details{ If necessary, more details than the description above }

\author{G. Sawitzki}
\seealso{ objects to See Also as \code{\link{pdfa4w}},  }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
# bertin:::pdfa4(file="Rplot.pdf"); ... ; dev.off()
## The function is currently defined as
function(...) pdf(height=11.192913, width=7.767717, ...)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ device }
