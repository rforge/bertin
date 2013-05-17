%#$Id: 00bertinworksheet.R 77 2013-05-09 14:02:45Z gsawitzki $
% Based on: File src/library/utils/man/str.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{str.bertin}
\title{Compactly Display the Structure of an R Object of Class Bertin}
%\alias{str}
\alias{str.bertin}
%\alias{str.default}
%\alias{strOptions}
\usage{

\method{str}{bertin}(object,  \dots)
}
\description{
  Compactly display the internal \bold{str}ucture of an \R object, a
  diagnostic function and an alternative to \code{\link{summary}}
  (and to some extent, \code{\link{dput}}).  Ideally, only one line for
  each \sQuote{basic} structure is displayed.  It is especially well suited
  to compactly display the (abbreviated) contents of (possibly nested)
  lists.  The idea is to give reasonable output for \bold{any} \R
  object.  It calls \code{\link{args}} for (non-primitive) function objects.

  \code{strOptions()} is a convenience function for setting
  \code{\link{options}(str = .)}, see the examples.
}
\arguments{
  \item{object}{any \R object of class berin about which you want to have some
    information.}

  \item{\dots}{potential further arguments (required for Method/Generic
    reasons).}
}
\value{
  \code{str} does not return anything, for efficiency reasons.
  The obvious side effect is output to the terminal.
}
\author{G. Sawitzki
}
%\seealso{
%  \code{\link{ls.str}} for the generic link function;
%  \code{\link{ls.str}} for \emph{listing} objects with their structure;
%  \code{\link{summary}}, \code{\link{args}}.
%}
%\examples{}
\keyword{print}
\keyword{documentation}
\keyword{utilities}
