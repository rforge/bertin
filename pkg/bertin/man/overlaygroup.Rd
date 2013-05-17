%$Id: 00bertinworksheet.R 77 2013-05-09 14:02:45Z gsawitzki $
\name{overlaygroup}
\alias{overlaygroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{overlaygroup
}
\description{
Overlays group separators for Bertin plots.}
\usage{
overlaygroup(group, pos, byrow = TRUE, col = "green", lwd = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{group}{A grouping variable. Separators are to be inserted where the group changes.
}
  \item{pos}{Positions for grouping separators. Usually derived from \code{grop}.
}
  \item{byrow}{ TRUE if cases are  by row.}
  \item{col}{Colour for gorup separator.}
  \item{lwd}{Line width. Preferably small}
  \item{\dots}{Additionalparamters passed to abline, for example line style.
}
}
%\details{}
\value{
	 \item{pos}{A vector of indices where a separator is added..}
}
%\references{%% ~put references to the literature/web site here ~}
\author{G. Sawitzki}
%\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{aplot}% __ONLY ONE__ keyword per line
