\name{imageBG}
\alias{imageBG}
\title{Neutral background for NA elements in an image}
\description{
Sets NA grid values of z to slelcted replacement colour.. 
}
\usage{
imageBG(x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, length.out = ncol(z)), 
z, zlim = range(z[is.finite(z)]), xlim = range(x), ylim = range(y), 
col = "#80808080", add = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{same as image}
  \item{y}{same as image}
  \item{z}{same as image}
  \item{zlim}{same as image}
  \item{xlim}{same as image}
  \item{ylim}{same as image}
  \item{col}{A colour to be used for NA entries. Preferablya grey tone with intensity 
  mathing the average signsalhere~~
}
  \item{add}{same as image}
   \item{\dots}{Passed to image
}
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
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%%}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, zlim = range(z[is.finite(z)]), 
    xlim = range(x), ylim = range(y), col = "#80808080", add = TRUE, 
    ...) 
{
    z <- ifelse(is.na(e[[k]]), 1, NA)
    image(x, y, z, zlim, xlim, ylim, col, add, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }% __ONLY ONE__ keyword per line
