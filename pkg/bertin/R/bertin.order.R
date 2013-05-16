#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

#! or ordercor() ? see bertin.R
bertin.order <-
function (z, MARGIN=1, pivot=1, FUN=cor)
{
if (MARGIN==1)
{ dist <- vector(mode="numeric", length=nrow(z) )
for (i in (1: nrow(z))){
	dist[i] <- FUN(z[pivot,],z[i,])
}
} else {
dist <- vector(mode="numeric", length=ncol(z) )
for (i in (1: ncol(z))){
	dist[i] <- FUN(z[,pivot],z[,i])
}
}
order(dist)
}

