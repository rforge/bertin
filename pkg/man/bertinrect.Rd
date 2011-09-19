%#$Id$
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
bertinrect(z, main, sepwd = 0.05, pars, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{a data matrix, or data structure that can be casted to a matrix.}
  \item{main}{a main title for the plot, see also \code{\link{title}}.}
  \item{sepwd}{
a margin width. The display of an element should be based at \code{(i + sepwd, j + sepwd)}.}
  \item{pars}{Graphical parameters, see   \code{par}. See details}
  \item{\dots}{additional parameters, passed to \code{\link{rect}}.}
}

\details{
All facilities of \code{rect} are accessible, to supply colour, shading, rearrangement.

If \code{pars} is undefined, default parameters are selected, based on the data valuues and lables. The picture aspect ratio is chosen to allocate unit squares to each data cell.
If \code{pars}  is \code{NULL}, no graphical parameters are changed, else \code{pars} is passed to \code{\link{par}}


Row and column names are displayd in the margins. Adjust \code{mar}  or adjust the font size if necessary.
}

\value{
A list of graphical parameters, used to set \code{par} is returened as invisible result.
Use this to generate series of plots with comparable geometry-

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
