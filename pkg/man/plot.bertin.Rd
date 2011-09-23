%$Id$
%$Revision$
\name{plot.bertin}
\alias{plot.bertin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bertin plot}
\description{Display a Bertin plot of a data matrix. 
This is a high level variant of \code{\link{bertinrect}}.}
\usage{
plot.bertin(z, 
            roworder, colorder,
            var.orientation = c("byrow", "bycolumn", "global"),  
            main, sub,
            mar= c(6,1,2,1)+0.1,
            zcol, 
            palette = gray((255:0 / 255)^0.5),
            showpalette = TRUE,
            ...)
}
\arguments{
  \item{z}{a data matrix.}
  \item{roworder}{permutation used for rows.}
  \item{colorder}{permutation used for columns.}
  \item{var.orientation}{variable oritentation. Should be one of "byrow"=Bertin conventions, "bycolumn"=\R{} conventions, "global".}
  \item{main}{main title.}
  \item{sub}{sub-title.}
  \item{mar}{margin. Margin space for row and column labels will be added.}
  \item{zcol}{a data matrix of index values for use with the colour palette.}
  \item{palette}{a colour palette for the display of the data matrix.}
  \item{showpalette}{include palette ramp in display.}
  \item{\dots}{additional parameters, passed to \code{bertinrect}.}
}

\details{A grid of rectangles is displayed to represent the data. Typically, the value is encoded as height.
See the references below for details.

\code{sub} is shown in the first margin line. To get more control of title format and pages, use \code{\link{title}}.

If \code{showpalette} is TRUE, the palette is shown in the plot margin area.
}

\value{
A data matrix with display specification added as attributes. Details are subject to change.
This is  \code{\link{invisible}}.
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
}
\references{
J. Bertin: \emph{La graphique et le traitement graphique de l'information.} Flammarion: Paris 1977. 

A. de Falguerolles; F. Friedrich & G Sawitzki: \emph{A Tribute to J. Bertin's Graphical Data Analysis.}
Published in: W. Bandilla, F. Faulbaum (eds.): SoftStat '97. Advances in Statistical Software 6. 
Lucius & Lucius, Stuttgart 1997. (provided in the doc section of this package)
}
\author{
G. Sawitzki
}
\note{
Still experimental. This is a \pkg{bertin} specific wrapper for   \code{\link{bertinrect}}. 

We try to follow Bertin's original model, but consistency with \R{} traditions suggest other choices occasionally.

Following Bertin, we display variables as rows as a default. This can be controlled by \code{var.orientation}.

We use a grey level palette as a default, whereas Bertin used black and white. Use \code{palette = c("white", "black")} as an argument to get Bertin's choice.

Break points for colour choice are based on range. We use the mid-range as default cut point for a two colour choice, whereas Bertin uses the mean as cut point.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{image.bertin}}}

\examples{
library(bertin)
data(Hotel)
plot.bertin(Hotel)

plot.bertin(Hotel, palette=c("white","black"))

# rows rearranged by rank correlation to varible 19 (Occupation)
plot.bertin(Hotel, roworder = bertin.order(bertinrank(Hotel), pivot="Occupation"))
}
\keyword{ hplot }
