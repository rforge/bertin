\name{plot.bertin}
\alias{plot.bertin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bertin plot}
\description{Display a Bertin plot of a data matrix.}
\usage{
plot.bertin(z, roworder, colorder, main = "", col = gray((255:0/255)^0.5))
}
\arguments{
  \item{z}{a data matrix.}
  \item{roworder}{permutation used for rows.}
  \item{colorder}{permutation used for columns.}
  \item{main}{main title.}
  \item{col}{a matrix used as colour attributes in teh display of the data matrix.}
}
\details{To come.
%%  ~~ If necessary, more details than the description above ~~
}
%\value{
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
%}
\references{
%% ~put references to the literature/web site here ~
}
\author{
G. Sawitzki
}
\note{
Still experimental
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{image.bertin}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(bertin)
data(Hotel)
plot.bertin(Hotel)

hotelrk <- bertinrank(Hotel)
hotelvars <-bertin.order(hotelrk, pivot=19)
plot.bertin(Hotel, roworder=hotelvars)

## The function is currently defined as
function (z, roworder, colorder, main = "", col = gray((255:0/255)^0.5)) 
{
    bertinrect(z, main = main, col = col)
  }
}
\keyword{ hplot }% __ONLY ONE__ keyword per line
