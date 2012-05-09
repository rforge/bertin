\name{colrampmarg}
\alias{colrampmarg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Marginal colour ramp}
\description{Draw a colour ramp in the bottom margin of a plot.}
\usage{
colrampmarg(colpalette = palette(), vals = 0:100, main, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colpalette}{The colour palette to show.}
  \item{vals}{Values to use for the axis definition.}
  \item{main}{Title to show in the margin above the color ramp. }
  \item{\dots}{passed to \code{\link{image}}}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
G, Sawitzki
}
%\note{
%The colour ramp and title will use about three margin lines.
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
