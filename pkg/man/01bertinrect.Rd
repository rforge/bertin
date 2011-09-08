\name{bertinrect}
\alias{bertinrect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bertinrect
}
\description{
Display a data matrix using parallel coordinates as suggested by J. Bertin. Coordinates are set up such that each element \code{x[i, j]}is represented in a unit square with bottom left at coordinates \code{(i, j)}. By convention, variables are in rows, and cases are in columns.
}
\usage{
bertinrect(x, main, sepwd = 0.05, mar = c(1, 1, 6, 4) + 0.1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data matrix, or data structure that can be casted to a matrix.}
  \item{main}{a main title for the plot, see also \code{\link{title}}.}
  \item{sepwd}{
a margin width. The display of an element should be based at \code{(i + sepwd, j + sepwd)}.}
  \item{mar}{Margin parameters, see   \code{par}. Row and column names are displayd in the margins. Adjust \code{mar}  or adjust the font size if necessary.}
  \item{\dots}{
additional parameters, passed to \code{rect}.
}
}
\details{
All facilities of \code{rect} are accessible, to supply colour, shading, rearrangement.
}
\value{
This is subject to change.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
de Falguerolles, A., Friedrich, F., Sawitzki, G. (1997):
A Tribute to J. Bertin's Graphical Data Analysis.
In: Proceedings of the SoftStat '97 (Advances in Statistical Software 6),
11--20.

This paper is included in the documentation.

\url{http://bertin.r-forge.r-project.org}
}
\author{
G. Sawitzki
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{image.bertin}}, for a quick solution which uses display space to a maximum.
}
\examples{
data(Hotel)
bertinrect(Hotel)

nrow <- 3; ncol <- 5
zunif <- matrix(runif(nrow*ncol), nrow, ncol)
colnames(zunif) <- colnames(zunif, do.NULL=FALSE)
 rownames(zunif) <- rownames(zunif, do.NULL=FALSE)
bertinrect(zunif, yaxt="n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{cluster }
\keyword{tree }
% __ONLY ONE__ keyword per line
