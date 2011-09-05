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
imagem(zunif, yaxt="n")


## The function is currently defined as
function(x,
	sepwd=0.05,
	mar= c(2,2,6,6)+0.1,
	...){
	# [i,j] bottom left is at user coordinates (i,j)
	# sepwd is internal margin
	main <- deparse(substitute(x))
	oldpar <- par(mar=mar)
	plot(c(1, ncol(x)+1), c(1, nrow(x)+1), 
	main=main,type= "n", xlab="", ylab="", axes=FALSE)

	x <- as.matrix(x) #! support lists and data frames as well

	#! improve. use scaling as in plot.window
	ranges <- apply(x,1,range,finite=TRUE) # transposed
	ranges[1,] <- pmin(0,ranges[1,]) #tack at zero
	ranges[2,] <- pmax(0,ranges[2,]) #tack at zero
	scale <- ranges[2,] - ranges [1,]
	scale[] <- ifelse(scale[]==0,0.1,scale[])
	scale[!is.finite(scale[] )] <- 0.1 	# fix zero scales
	scale[scale[]==0] <- 0.1
	#add some margin. ! improve. use scaling as in plot.window
	#for now, is handled by sepwd
	#ranges[1,] <- ranges[1,]- 0.04*scale[]
	#ranges[2,] <- ranges[2,]+ 0.04*scale[]
	scale <- (1-2*sepwd)/scale
	zeroline <- -ranges[1,]*scale

	xleft <- matrix((1:ncol(x)), nrow(x), ncol(x), byrow=TRUE)+sepwd
	xright <- xleft+1-2*sepwd
	xbottom <-  1+nrow(x)-(matrix((1:nrow(x)),nrow(x),ncol(x))) + 
		sepwd +zeroline #box zero line
	xtop <- x*scale+xbottom

if (any(ranges[2,]==0)){
	abline(h= xbottom[ranges[2,]==0],lty=3,col="gray")}
	rect(xleft,xbottom,xright,xtop,...)

	#textnames(x)
       if (!is.null(colnames(x))){ 
       	for (col in (1:dim(x)[2])) 
       		text(col+0.8, nrow(x)+1, colnames(x)[col], 
            pos = 3, xpd = NA, offs = 1, srt = 90, cex=0.6)}
       if (!is.null(rownames(x))) {
       	r <- par("usr")[2] #right
       	for (row in (1:dim(x)[1])) 
       		text(r, nrow(x)-row+1.4, rownames(x)[row], 
            pos = 4, xpd = NA, offs = rowoffs, srt = 0, cex=0.6)
            
   if (!all(is.finite(x))) {
   	badpos <- !is.finite(x)
   	xbottom <-  1+nrow(x)-(matrix((1:nrow(x)),nrow(x),ncol(x))) + sepwd #box zero line
   	text(xleft[badpos]+0.4,xbottom[badpos],labels=x[badpos], pos=3, offs=0.5,col="red",cex=0.6)
   }
  } 
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{cluster }
\keyword{tree }
% __ONLY ONE__ keyword per line
