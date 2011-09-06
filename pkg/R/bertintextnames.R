#$Id: bertinR.Rnw 10 2011-08-31 18:49:48Z gsawitzki $
#$Revision: 10 $
#$Date: 2011-08-31 20:49:48 +0200 (Wed, 31 Aug 2011) $
#$Author: gsawitzki $

#add row/columnnames to Bertin display
#This funtion is repeated inline where appropriate
bertintextnames <-
	function (x, coloffs=-1, rowoffs=NULL) {
		# note: image interchanges rows/colums
       if (!is.null(colnames(x))){ 
       	for (col in (1:dim(x)[2])) 
       		text(col+0.8, nrow(x)+1, colnames(x)[col], 
            pos = 3, xpd = NA, offs = 1, srt = 90, cex=0.6)}
       if (!is.null(rownames(x))) {
       	r <- par("usr")[2] #right
       	for (row in (1:dim(x)[1])) 
       		text(r, nrow(x)-row+1.4, rownames(x)[row], 
            pos = 4, xpd = NA, offs = 0, srt = 0, cex=0.6)}
	} # textnames
