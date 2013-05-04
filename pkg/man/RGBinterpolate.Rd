% RGBinterpolate <- function (n=100, fromRGB, toRGB, bias=1.0){
	% if ((n <- as.integer(n[1])) > 0) {
		% from <-col2rgb(fromRGB)/255
		% to <-col2rgb(toRGB)/255
		% ifrom <- ((n:0)/n)^bias; ito <- ((0:n)/n)^bias
		% rgb(ifrom*from[1]+ito*to[1],ifrom*from[2]+ito*to[2],ifrom*from[3]+ito*to[3])
		% }
    % else character(0)
% }
\name{RGBinterpolate}
\alias{RGBinterpolate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RGBinterpolate}
\description{
Create a colour palette by linear interpolation in RGB space}
\usage{
RGBinterpolate(n=100, fromRGB, toRGB, bias=1.0)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Length-1}
  \item{fromRGB}{Name are hex representation of initial colour}
  \item{toRGB}{Name are hex representation of final colour}
  \item{bias}{Power for transformation of index}
}
% \details{
% %  ~~ If necessary, more details than the description above ~~
% }
\value{
a colour palette of length n+1}
% \references{
% %% ~put references to the literature/web site here ~
% }
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{library(\link{colorspace})}, ~~~
}
\examples{
	colramp(RGBinterpolate(n=100,"green","red"))
	colramp(RGBinterpolate(n=100,"green","red", bias=0.5))
	colramp(c(RGBinterpolate(n=20,"green","red"),RGBinterpolate(n=30,"red","blue")))
}

\keyword{ hplot }% __ONLY ONE__ keyword per line