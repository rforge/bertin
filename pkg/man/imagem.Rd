\name{imagem}
\alias{imagem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{imagem}
\description{A variant of image, adapted to matrices}
\usage{
imagem(z, 
	zlim = range(z[is.finite(z)]), 
	xlim = c(1, ncol(z)), 
	ylim = c(1, nrow(z)), 
	col = heat.colors(12), 
	add = FALSE, 
	xaxs = "i", yaxs = "i", 
	xlab, ylab, 
	main, 
	breaks, 
	oldstyle = FALSE, 
	names = TRUE, coloffs = -1, rowoffs = 4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ Describe \code{z} here }
  \item{zlim}{ Describe \code{zlim} here }
  \item{xlim}{ Describe \code{xlim} here }
  \item{ylim}{ Describe \code{ylim} here }
  \item{col}{ Describe \code{col} here }
  \item{add}{ Describe \code{add} here }
  \item{xaxs}{ Describe \code{xaxs} here }
  \item{yaxs}{ Describe \code{yaxs} here }
  \item{xlab}{ Describe \code{xlab} here }
  \item{ylab}{ Describe \code{ylab} here }
  \item{main}{ Describe \code{main} here }
  \item{breaks}{ Describe \code{breaks} here }
  \item{oldstyle}{ Describe \code{oldstyle} here }
  \item{names}{ Describe \code{names} here }
  \item{coloffs}{ Describe \code{coloffs} here }
  \item{rowoffs}{ Describe \code{rowoffs} here }
  \item{\dots}{ Describe \code{\dots} here }
}
% \details{ If necessary, more details than the description above }
\value{
  Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
%\references{ put references to the literature/web site here  }
\author{G. Sawitzki}
%\note{ further notes 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ objects to See Also as \code{\link{help}}, ~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (z, zlim = range(z[is.finite(z)]), xlim = c(1,ncol(z)), 
    ylim = c(1,nrow(z)), col = heat.colors(12),
	add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,main,
	breaks, oldstyle=FALSE,
	names=TRUE, coloffs=-1, rowoffs=4,...)
{
#! adjust calling structure with image()
#      image.default(x, y, z, zlim, xlim, ylim, col = heat.colors(12),
#           add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
#           breaks, oldstyle = FALSE, ...)

#! improve placement of names. use either nrow/ncol or par("usr")

	textnames <-
	function (zi, coloffs=-1, rowoffs=NULL) {
		# note: image interchanges rows/colums
		for (x in (1:dim(zi)[1]) ) # column labels
			text(x, ncol(zi)+0.5, rownames(zi)[x], pos=3, 
				xpd=NA,offs= coloffs, srt=270)
		
		r <- par("usr")[2]
		for (y in (1:dim(zi)[2]))  # row labels
			text(r, y, colnames(zi)[y], pos=4, xpd=NA, offs=rowoffs,srt=0)
	} # textnames
	
	
	zi <- t(z)
	opin <- par("pin"); on.exit(par(pin=opin))
	parasp(zi)
	image(
		1:nrow(zi),1:ncol(zi), zlim=zlim,
		#xlim=xlim,
		ylim=c(ncol(zi)+0.5,0.5), 
		col=col, add=add, xaxs=xaxs, yaxs=yaxs,
		xlab="", ylab="",z=zi, 
		main=main,
		breaks=breaks, oldstyle=oldstyle,
#		frame.plot=FALSE,	
		...)

#	image(
#		1:ncol(z), 1:nrow(z),z=t(z),zlim=zlim,
#		xlim=xlim,
#		ylim=c(nrow(z)+0.5,0.5), 
#	)

	if (names) {
		textnames(zi,coloffs=-4,rowoffs=1)
	} # names
  }#imagem
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
