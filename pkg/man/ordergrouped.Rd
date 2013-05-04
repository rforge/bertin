%#$Id: imagem.Rd 45 2011-09-23 20:12:00Z gsawitzki $
\name{ordergrouped}
\alias{ordergrouped}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Ordering Permutation}
\concept{sort data frame}
\description{
  \code{order} returns a permutation which rearranges its first
  argument by value, and optioanlly by an additiona grouping variable.
  It is an extension of \code{order}.
}
\usage{
ordergrouped(\dots, group, na.last = TRUE, decreasing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{group}{An optional factor variable for grouping.}
  \item{na.last}{See \code{order}. }
  \item{decreasing}{See \code{order}.}
}
\details{Arguments are passed to \code{order}. If \code{group} is provided,
The permutations are then rearranged to respect the grouping.}
\value{
  An integer vector unless any of the inputs has \eqn{2^{31}}{2^31} or
  more elements, when it is a double vector.
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{G. Sawitzki}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
x  <- c(1:3, 6:2)
names(x) <- c("n1gg1","n2g1","n3g2","n4g2","n5g2","n6g3","n7g3","n8g3")
gr <- c("g1","g1", "g2", "g2", "g2", "g3", "g3", "g3")

xperm <- ordergrouped(x, group=gr)
x[xperm]
gr[xperm]

## The function is currently defined as
function (..., group, na.last = TRUE, decreasing = FALSE) 
{
    if (missing(group)) {
        order(..., na.last = na.last, decreasing = decreasing)
    }
    else {
        ox <- order(..., na.last = na.last, decreasing = decreasing)
        ogro <- order(group[ox])
        ox[ogro]
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{manip}
