#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# a commom worksheet for bertin
#!/bin/sh
cd /Users/gs/projects/rforge/bertin

svn propset svn:keywords "Date Author Id Revision HeadURL" pkg/R/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" work/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" pkg/man/*.Rd
export _R_CHECK_TIMINGS_=0
export _R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_=TRUE
R CMD CHECK pkg/bertin  --no-multiarch  --timings
R CMD BUILD --compact-vignettes pkg --no-multiarch
rm bertin.pdf; R CMD Rd2dvi -o bertin.pdf --pdf --title="Bertin" pkg
% rm tmp.pdf;  R CMD Rd2pdf --no-clean --output=tmp.pdf "/Users/gs/projects/rforge/bertin/pkg/man/00bertin-package.Rd" ; open tmp.pdf
rm bertin.pdf; R CMD Rd2pdf -o bertin.pdf  --internals --title="Bertin" pkg

R CMD Rd2pdf -o bertin.pdf  --internals --no-clean --title="Bertin" pkg

	cd /Users/gs/projects/rforge/bertin/pkg/inst/doc
	mv bertinR.pdf bertinR_temp.pdf
	qpdf bertinR_temp.pdf bertinR.pdf
	rm bertinR_temp.pdf
    cd /Users/gs/projects/rforge/bertin

R CMD BUILD --compact-vignettes pkg --no-multiarch

R CMD BUILD pkg

# install package
R CMD INSTALL bertin
# generate a pdf help file
rm bertin.pdf; R CMD Rd2dvi -o bertin.pdf --pdf --title="Bertin" pkg

#### end of shell commands
# R cmds for ad hoc construction
# setwd("/Users/gs/projects/rforge/bertin/")
# install.packages("/Users/gs/projects/rforge/bertin/bertin_0.1-76.tar.gz", repos=NULL, type="source")


################################
# To do list
# To do list: Current
clean up work/*

# To do list: Queued
20130511
bertinrect: only last colname is shown

20130509
bertinrect: main is leaking outside
Example:
library(bertin)
data(Hotel)
bertinrect(Hotel)
#################################

#
 h2 <- h1[2:7,]
> h2
               Jan Fev Mars Avril May Juin Juil Aout Sept Oct Nov Dec
Locale          69  70   77    71  37   36   39   39   55  60  68  72
USA              7   6    3     6  23   14   19   14    9   6   8   8
AmerSud          0   0    0     0   8    6    6    4    2  12   0   0
Europe          20  15   14    15  23   27   22   30   27  19  19  17
MOrientAfrique   1   0    0     8   6    4    6    4    2   1   0   1
Asie             3  10    6     0   3   13    8    9    5   2   5   2
> apply(h2,2,sum)
  Jan   Fev  Mars Avril   May  Juin  Juil  Aout  Sept   Oct   Nov   Dec 
  100   101   100   100   100   100   100   100   100   100   100   100 
> h2 <- h1[8:9,]
> apply(h2,2,sum)
  Jan   Fev  Mars Avril   May  Juin  Juil  Aout  Sept   Oct   Nov   Dec 
  100   100   100   100   100   100   100   100   100   100   100   100 
> h2 <- h1[10:12,]
> apply(h2,2,sum)

__insftall<- function(){
	detach("package:bertin", unload=TRUE)
	install.packages("bertin", repos="http://r-forge.r-project.org",type="source")
	
	detach("package:bertin", unload=TRUE)
	 remove.packages("bertin")
	install.packages("/Users/gs/projects/rforge/bertin/bertin_0.1-76.tar.gz", repos=NULL, type="source", INSTALL_opts="--no-multiarch ")
	library(bertin)
}
setwd("/Users/gs/projects/rforge/bertin/pkg/inst/doc")
enc <- options(width = 56);  Sweave("bertinR.Rnw",output="bertinR.tex", keep.source=TRUE, debug = TRUE, eps = FALSE); options(enc)
tools::compactPDF("/Users/gs/projects/rforge/bertin/pkg/inst/doc/bertinR.pdf") 
# # \textperthousand
####

 z<- Brmatrix; main <- "M"; showpalette=FALSE

quartz(width=5,height=5)
testplot(Brmatrix)
data(Hotel)
testplot(Hotel)

# debugging support: show selected par values
showpar <- function (where){
	p <-par("cin","din","fig","fin","pin","plt","mai", "mar","mfg","new","usr")
	if (!missing(where)){cat(where, "\n")}
	str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
	invisible(p)
	}
showpar("bertinrect done")
bertinrect(z, main= main) # top space insufficient
bertinrect(z, main= main, mar=c(5, 4, 4, 2) + 0.1) #ok
imagem(z,main=main)
testplot1 <- function(z, main=deparse(substitute(z)), showpalette=FALSE)
{
	oldpar <- par(mfrow=c(2,2))
	bertinrect(z, main= main); showpar("bertinrect done")
	title(sub="bertinrect", line=1)
#par(mfg=c(1,2)) # fix for stray display allocation
	imagem(z, main= main)
	title(sub="imagem", line=1)
#par(mfg=c(2,1)) # fix for stray display allocation -- this is very bad
	plot.bertin(z, main= main,showpalette= showpalette);
#par(mfg=c(2,1)) # fix for stray display allocation -- this is very very bad
	title(sub="plot.bertin", line=0)
par(mfg=c(2,2)) # fix for stray display allocation #!!!!!!!
	image.bertin(z, main= main,showpalette= showpalette);showpar("image.bertin done")
		title(sub="image.bertin", line=0)
	par(oldpar)
}
testplot1(Brmatrix, showpalette=TRUE) # z<- Brmatrix; main <- "M"; showpalette=FALSE
mtext("line0", side=1, line=0,adj=0.5)
mtext("line1", side=1, line=1,adj=0.5)
mtext("line1.5", side=1, line=1.5,adj=0.5)
plot.bertin(z, main= main,showpalette= FALSE);
colrampmarg()
showpar("colrampmarg1 done")
colrampmarg4()
plot(runif(10))
showpar()
debug(colrampmarg1)
z <-as.matrix(Hotel)
p <- imagem(Hotel) # Rand zu groß
p <- imagem(Hotel, mar=c(0,0,1,0)) # Rand zu groß

p <- image.bertin(Hotel, sub="image.bertin")
p <- plot.bertin(Hotel, sub="plot.bertin")

#  Hotel perm
colmeans <- apply(Hotel2,1,mean)
rowperm <- c(19,18,20, 8, 11, 4, 12, 13,16,14,1,2, 7, 9, 10, 17, 6, 3, 5,15)
plot.bertin(Hotel2, roworder=rowperm,
	zcol=ifelse(Hotel2[,]>colmeans, 2,1),
	palette=c("white","black"))
title(sub="Variables rearranged", line=0)

 
zrange <- range(z, finite=TRUE)
image(z=t(matrix(seq(zrange[1],zrange[2],length.out=length(col)),
        1, length(col))),
        zlim=zrange,main="", ylab="", xlab="", col=col)
par(oldpar)

library(bertin)

 prompt, promptClass, and promptMethods functions  
      image.bertin(hotelrk,roworder=ordercor(hotelrk,19),colorder=1:12)
prompt(bertinrangescore)
prompt(colrampmarg)
# use image.default as template for imagem.

quartz(width=12,height=12)
quartz(width=5,height=5)
par(mfcol=c(2,2))
	

image.bertin(Bcmatrixx)

imagem(Bcmatrixx)

par(mfrow=c(2,2))
p <- bertinrect(hotelrank);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- plot.bertin(hotelrank);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- imagem(hotelrank);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- image.bertin(hotelrank);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")

p <- bertinrect(Hotel2);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- bertinrect(USJudgeRatings);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- bertinrect(t(USJudgeRatings)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- bertinrect(t(Brmatrix)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- bertinrect(t(Bcmatrix)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")

p <- plot.bertin(Hotel2);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- plot.bertin(USJudgeRatings);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- plot.bertin(t(USJudgeRatings)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- plot.bertin(t(Brmatrix)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- plot.bertin(t(Bcmatrix)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")

p <- imagem(Hotel2);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- imagem(USJudgeRatings);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- imagem(t(USJudgeRatings)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- imagem(t(Brmatrix)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- imagem(t(Bcmatrix)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")

p <- image.bertin(Hotel2);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- image.bertin(USJudgeRatings);str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- image.bertin(t(USJudgeRatings)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- image.bertin(t(Brmatrix)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")
p <- image.bertin(t(Bcmatrix)); str(p, give.head=FALSE, no.list=TRUE, comp.str=" ")

x <- Hotel
USJudgeRatings1 <- USJudgeRatings[,-1]
colmeans <- apply(USJudgeRatings1,2,mean)
colorder<- order(colmeans)
rowmeans <-  apply(USJudgeRatings1,1,mean)
roworder <- order(rowmeans)
col=ifelse(USJudgeRatings1[,]>colmeans, "black","white")
bertinrect(t(USJudgeRatings1)[colorder, roworder],col=t(col)[colorder, roworder], sepwd=0.1, mar=c(0.05,0.05,10,4), main="")

testplot
function(z, main=deparse(substitute(z)))
{
	oldpar <- par(mfrow=c(2,2))
	bertinrect(z, main= main)
	title(sub="bertinrect", line=1)
	plot.bertin(z, main= main)
	title(sub="plot.bertin", line=1)
	imagem(z, main= main)
	title(sub="imagem", line=1)
	image.bertin(z, main= main)
	title(sub="image.bertin", line=1)
	par(oldpar)
}


# suggest graphic parameters for a matrix display
suggestpari <- function(z, rownames=NULL, colnames=NULL, maxwidth=6, cex=1){
	dfltmai <- c(1.02,0.82,0.82,0.42) # mar=c(5, 4, 4, 2) + 0.1
	nrow <- nrow(z)
	ncol <- ncol(z)
	
	if (missing(rownames)) {
	rownames <-  rownames(z)}
	if (is.null(rownames) ) rownames <- rownames(z)
	

	if (missing(colnames)) {
	colnames <-  colnames(z)}
	if (is.null(colnames) ) colnames <- colnames(z)
		

	rowi <- max(strwidth(rownames,"inches"))*cex
	coli <- max(strwidth(colnames,"inches"))*cex
	mai <- c(dfltmai[1], dfltmai[2], coli+ dfltmai[3], rowi )
	plotwidth <- maxwidth - rowi - dfltmai[2]
	aspr <- nrow/ncol
	height <- aspr*plotwidth + rowi + dfltmai[1] + dfltmai[3] # maybe smaller
	return(list(mai=mai,width=maxwidth,heigth=height))
}

suggestpari(Hotel2)
suggestpari(Hotel)
suggestpari(USJudgeRatings)
suggestpari(t(USJudgeRatings))

#! source("/Users/gs/projects/rforge/bertin/pkg/tests/01bincode.R")
#! 2011-09-09 image.bertin(bincodedatav) gives main="x", should be bincodedatav

todo <- function(){
#! 2011-10-05 define representation for negated variables. font=4 bold italic
#! 2011-10-05 basic geometry: include col/rownames
#! make ramp sub-header lien height
p <- bertinrect(t(USJudgeRatings));sp() #left/bottom margin in USJudgeRatings example
p <- bertinrect(t(USJudgeRatings), mar=c(1, 1,2,3), main="USJ");sp() # main title hiden
#! 2011-09-10 anti-alias in embedded pdf
#! 2011-09-13 mar needed for colour samples. added in example
#! bertin.R ordermean to do
#! use strwidth to find settings for cin
#! check scores etc for innocent leading ... Use args as named!
#! allow for par values as argument in bertinrect
#! propagate par setup from bertinrect
#! judgeperm1 vertical coordinates not ok
}	
done <- function(){
# 2011-09-23 plot.bertin example cleaned up
}
?heatmap

sampleimagex <- function(z, 
	col = grey((1:256)/256), xlab, ylab, main, 
	colinvert=FALSE){
	if (colinvert) col <- col[length(col):1]
	# x1, x2. y1, y2
	oldpar <- par(fig=c(0, 1, 0.2, 1), 
		mar=c(2.5,1.5,0.5,0.5), new=FALSE)
	imagem(z, col=col, useRaster=TRUE)
	print(par("fig", "mar"))
	par(yaxt="n", 
		fig=c(0, 1, 0, 0.2), 
		mar=c(0.1,0.1,0.1,0.1),
		new=TRUE)
		#mar=c(3.5,12.0,0.5,12.0), 
#	colramp(col=col, horizontal=TRUE)
	zrange <- range(z, finite=TRUE)
	cat("zrange:",zrange,"\n")
	image(z=t(matrix(seq(zrange[1],zrange[2], 	
		length.out=length(col)), 
		1, length(col))), 
		x=z,
	#	xlim=zrange,
		zlim=zrange,
		main="", ylab="", xlab="", col=col)
	par(oldpar)
}

hotelrk <- bertinrank(Hotel)
sampleimagex(hotelrk)

samplemap <- function(z, col = grey((1:256)/256), xlab, ylab, main, colinvert=FALSE){
if (colinvert) col <- col[length(col):1]
# x1, x2. y1, y2
oldpar <- par(fig=c(0, 1, 0.2, 1), mar=c(2.5,1.5,0.5,0.5), new=FALSE)
imagem(z, col=col)#imagem(z, xlab=xlab, ylab=ylab, main=main, col=col, mar=c(0.1,,0.1,0.1,0.1))
par(yaxt="n", fig=c(0, 1, 0, 0.2), mar=c(3.5,4.0,0.5,4.0), new=TRUE)
zrange <- range(z, finite=TRUE)
image(z=t(matrix(seq(zrange[1],zrange[2],length.out=length(col)), 1, length(col))), zlim=zrange,main="", ylab="", xlab="", col=col)
par(oldpar)
}

samplemap(Brmatrix)
samplemap(Brmatrix, colinvert=TRUE)

#! 2011-09-08 new page is opened at plot 3 -- should be on same page
devAskNewPage(ask=TRUE)
oldpar <- par(mfrow=c(2,2)) 
par("fig", "fin", "plt", "pin","new", "mfg")
x1 <-image.bertin(Brmatrix, main="1 image.bertin")
par("fig", "fin", "plt", "pin","new")
x2 <- plot.bertin(Brmatrix, main="2 plot.bertin")
par("fig", "fin", "plt", "pin","new")
x3 <-imagem(Brmatrix, main="3 imagem")
par("fig", "fin", "plt", "pin","new")
x4 <- bertinrect(Brmatrix, main="4 bertinrect")
par("fig", "fin", "plt", "pin","new")
par(oldpar)

oldpar <- par(mfrow=c(2,2)) 
image(Brmatrix)
imagem(Brmatrix)
bertinrect(Brmatrix)
par(oldpar)

oldpar <- par(mfrow=c(2,2)) 
image(Bcmatrix)
imagem(Bcmatrix)
bertinrect(Bcmatrix)
par(oldpar)


bertinrect(t(USJudgeRatings))
bertinrect(t(USJudgeRatings),col=t(col), sepwd=0.1, 	mar=c(0.2,0.2,10,6),main="USJudgeRatings")
 
 imagem(t(USJudgeRatings), ,main="USJudgeRatings",coloffs=1.0, rowoffs=1.0,mar=c(0.2,0.2,12,6))  
 image.bertin(t(USJudgeRatings))    

data(Hotel)
hotelrks <- bertinrank(Hotel,byrow=TRUE)
image.bertin(hotelrks)

par(mfrow=c(2,2))

bertinrect(hotelrks,main="rect")
bertinrect0(hotelrks,main="rect0")# left not shown
bertinrect1(hotelrks,main="rect1")# right not shown
bertinrect2(hotelrks,main="rect2")# right not shown

bertinrect2(hotelrks,mar=c(1,1,3,4))
bertinrect1(Brmatrixx)

bertinrect(hotelrks)
bertinrect2(Brmatrixx)

bertinrect(hotelrks[19,])
bertinrect(Hotel[,12])
bertinrect(12)

imagem(hotelrks)
imagem(Brmatrixx)
imagem(Brmatrixx, col=gray((1:256)/256))

library(seriation)

nrow <- 5
ncol <- 3
x <- matrix(1, nrow, ncol)
colnames(x) <- colnames(x, do.NULL=FALSE)
rownames(x) <- rownames(x, do.NULL=FALSE)

bertinplot(x)

x[1,] <- c(1,2,3)

bertinplot(x)
bertinplot(x,)

nrow <- 5
ncol <- 3
x <- matrix(rnorm(nrow*ncol), nrow, ncol)
colnames(x) <- colnames(x, do.NULL=FALSE)
rownames(x) <- rownames(x, do.NULL=FALSE)
bertinplot(x)

highlight <- matrix(TRUE, nrow-1, ncol)
bertinplot(x,highlight=highlight)

Nrcases <- 7 

# Test vectors, used to build a matrix
Bzero <- rep(0, Nrcases)
Bone <- rep(1, Nrcases)
Bmone <- rep(-1, Nrcases)
Binc <- (1:Nrcases)
Bdec <- (Nrcases:1)
Bstep <- c(Bmone[Binc < Nrcases/2], Bone[Binc >= Nrcases/2])
Bhat <- Bone; Bhat[(floor(Nrcases/3)+1):(Nrcases-floor(Nrcases/3)) ] <- 0.5
Bnazero <- rep(c(NA,0),length.out= Nrcases)
Bnanzero <- rep(c(NaN,0),length.out= Nrcases)
Binf <- rep(c(Inf,0,-Inf),length.out= Nrcases)

# Basic test matrices
Brmatrix <- rbind(Bzero, Bone, Bmone, Binc, Bdec, Bstep, Bhat)
colnames(Brmatrix) <- colnames(Brmatrix,FALSE)

## R may use internal housekeeping to keep matrix columns hogogeneous. Check!
## Use row matrix and column matrix for tests.
Bcmatrix <- cbind(Bzero, Bone, Bmone, Binc, Bdec, Bstep, Bhat)
rownames(Bcmatrix) <- rownames(Bcmatrix,FALSE)

# Basic test matrices with random error
BrRndmatrix <- Brmatrix+rnorm(nrow(Brmatrix)*ncol(Brmatrix))

# Test matrices with IEEE specials
Brmatrixx <- rbind(Bzero, Bone, Bmone, Binc, Bdec, Bstep, Bhat, Bnazero, Bnanzero, Binf)
Bcmatrixx <- cbind(Bzero, Bone, Bmone, Binc, Bdec, Bstep, Bhat, Bnazero, Bnanzero,Binf)

BrRndmatrixx <- Brmatrixx+rnorm(nrow(Brmatrixx)*ncol(Brmatrixx))

## Test commands
 
 image.bertin(Brmatrix)
 image.bertin(Bcmatrixx)
 image.bertin(Brmatrixx, colorder=7:1, roworder =c(7:10,1:6))
 image.bertin(Bcmatrixx, roworder=7:1, colorder =c(7:10,1:6))
 
 ###
 
##
setwd("/Volumes/Untitled/bertin1")cd 

source("R/parasp.R")
source("R/imagem.R")
source("R/bertin.R")
source("R/bertinorder.R")
source("R/colors.R")
source("R/colramp.R")
source("R/pageformats.R")
source("R/parasp.R")
source("R/parmfrow.R")
source("R/whatis.R")
ll <- ls()
package.skeleton(name="bertin", ll)

rm bertin.pdf; R CMD Rd2dvi --pdf --output=bertin.pdf --title="Bertin" /Users/gs/projects/rforge/bertin/pkg/man; open bertin.pdf
