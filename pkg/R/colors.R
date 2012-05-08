################### gs colors
###################
## green.colors
###################
green.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb(0,(n:0)/n, 0)
		else
			rgb(0,(0:n)/n, 0)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(green.colors(100)); colramp(green.colors(100, rev=TRUE))

wgreen.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb(1-(0:n)/n,1, 1-(0:n)/n)
		else
			rgb(1-(n:0)/n,1, 1-(n:0)/n)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(wgreen.colors(100)); colramp(wgreen.colors(100, rev=TRUE))

wmagenta.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb(1,1-(n:0)/n, 1)
		else
			rgb(1,1-(0:n)/n, 1)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(wmagenta.colors(100)); colramp(wmagenta.colors(100, rev=TRUE))

###################
## red.colors
###################
red.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb((n:0)/n,0, 0)
		else
			rgb((0:n)/n,0, 0)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(red.colors(100)); colramp(red.colors(100, rev=TRUE))

wred.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb(1,1-(0:n)/n, 1-(0:n)/n)
		else
			rgb(1,1-(n:0)/n, 1-(n:0)/n)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(wred.colors(100)); colramp(wred.colors(100, rev=TRUE))

wcyan.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb(1-(n:0)/n, 1, 1)
		else
			rgb(1-(0:n)/n, 1, 1)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(wcyan.colors(100)); colramp(wcyan.colors(100, rev=TRUE))
###################
## blue.colors
###################
blue.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb(0,0,(n:0)/n)
		else
			rgb(0,0,(0:n)/n)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(blue.colors(100)); colramp(blue.colors(100, rev=TRUE))

wblue.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb(1-(0:n)/n, 1-(0:n)/n, 1)
		else
			rgb(1-(n:0)/n, 1-(n:0)/n,1)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(wblue.colors(100)); colramp(wblue.colors(100, rev=TRUE))

wyellow.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb( 1, 1 ,1-(n:0)/n)
		else
			rgb( 1, 1,1-(0:n)/n)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(wyellow.colors(100)); colramp(wyellow.colors(100, rev=TRUE))
###################
## blueyellow
###################

blueyellow.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb((n:0)/n,(n:0)/n, (0:n)/n)
		else
			rgb((0:n)/n,(0:n)/n, (n:0)/n)
    	}
    else character(0)
}
#
# par(mfrow=c(2,1)); colramp(blueyellow.colors(100)); colramp(blueyellow.colors(100, rev=TRUE))

###################
## blueyellow2, with light middle
###################

blueyellow2.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev) {
		q<-((n:0)/n  -0.5) *2
		}
		else {
		q<-((0:n)/n  -0.5) *2
		}
		qq <- ((q*q*sign(q)+1)/2)
		q1<- 1- ((q*q+1)/2)
		rgb(qq+q1,qq+q1, 1-qq+q1)

    	}
    else character(0)
}
# par(mfrow=c(2,1)); colramp(blueyellow2.colors(100)); colramp(blueyellow2.colors(100,rev=TRUE))

###################
## blueyellow4, with light middle
###################

blueyellow4.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev) {
		q<-((n:0)/n  -0.5) *2
		}
		else {
		q<-((0:n)/n  -0.5) *2
		}
		qq <- ((q*q*q*q*sign(q)+1)/2)
		q1<- 1- ((q*q*q*q+1)/2)
		rgb(qq+q1,qq+q1, 1-qq+q1)

    	}
    else character(0)
}
# par(mfrow=c(2,1)); colramp(blueyellow4.colors(100)); colramp(blueyellow4.colors(100,rev=TRUE))

###################
## bluered2, with light middle
###################

bluered2.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev) {
		q<-((n:0)/n  -0.5) *2 }
		else{
		q<-((0:n)/n  -0.5) *2
		}
		qq <- ((q*q*sign(q)+1)/2)
		q1<- 1- ((q*q+1)/2)
		rgb(qq+q1,2*q1, 1-qq+q1)
    	}
    else character(0)
}
# par(mfrow=c(2,1)); colramp(bluered2.colors(100)); colramp(bluered2.colors(100,rev=TRUE))

###################
## bluered4, with light middle
###################

bluered4.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev) {
		q<-((n:0)/n  -0.5) *2 }
		else{
		q<-((0:n)/n  -0.5) *2
		}
		qq <- ((q*q*q*q*sign(q)+1)/2)
		q1<- 1- ((q*q*q*q+1)/2)
		rgb(qq+q1,2*q1, 1-qq+q1)
    	}
    else character(0)
}
# par(mfrow=c(2,1)); colramp(bluered4.colors(100)); colramp(bluered4.colors(100,rev=TRUE))


###################
## greenred
###################

greenred.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev)
			rgb((0:n)/n,(n:0)/n, 0)
		else
			rgb((n:0)/n,(0:n)/n, 0)
    	}
    else character(0)
}

###################
###################
greenred2.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev) {
		q<-((n:0)/n  -0.5) *2 }
		else {
		q<-((0:n)/n  -0.5) *2
		}
		qq <- ((q*q*sign(q)+1)/2)
		q1<- 1- ((q*q+1)/2)
		rgb(qq+q1,1-qq+q1, 2*q1)
    	}
    else character(0)
}
# par(mfrow=c(2,1)); colramp(greenred2.colors(100)); colramp(greenred2.colors(100,rev=TRUE))

###################
###################
greenred4.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev) {
		q<-((n:0)/n  -0.5) *2 }
		else {
		q<-((0:n)/n  -0.5) *2
		}
		qq <- ((q*q*q*q*sign(q)+1)/2)
		q1<- 1- ((q*q*q*q+1)/2)
		rgb(qq+q1,1-qq+q1, 2*q1)
    	}
    else character(0)
}
# par(mfrow=c(2,1)); colramp(greenred4.colors(100)); colramp(greenred4.colors(100,rev=TRUE))

###################
###################
loggray.colors <- function (n,p=2, rev=FALSE)
{ 	
	if (rev) {
	x <- log(n:1)^p
	} else {x <- log(1:n)^p
	}
	x <- (x-min(x))/(max(x)-min(x))
	gray(x)
}


###################
###################
tail.colors <-
# highlight tails, asymetric
function (n=100, q1=0.125, q2=0.25, q3=1-q2,q4=1-q1, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		#if (missing(q3)) q3 <- 1-q2
		#if (missing(q4)) q4 <- 1-q1
		r<-rep(0,n); g <- r; b <- r
		
		n1<-n*q1
		n2<-n*q2
		n3<-n*q3
		n4<-n*q4
		if (rev) {
			#print(c(n1,n2,n3,n4))
			for (i in 1:n)
			if (i<n1) r[i]<-1 #red
			 else
				if (i<n2) {r[i]<-1; g[i]<-0.7} #yellow red
				else
					if (i<n3) {r[i]<-0.95; b[i]<-0.95; g[i]<-0.95} #lt grey
					else 
					if (i<n4) {r[i]<-0.7; g[i]<-1} #yellow green
					else g[i]<- 1 #green
    	}
		else {
			#print(c(n1,n2,n3,n4))
			for (i in 1:n)
			if (i<n1) g[i]<-1 #green
			 else
				if (i<n2) {r[i]<-0.7; g[i]<-1} #yellow green
				else
					if (i<n3) {r[i]<-0.95; b[i]<-0.95; g[i]<-0.95} #lt grey
					else 
					if (i<n4) {r[i]<-1; g[i]<-0.7} #yellow red
					else r[i]<- 1 #red
    	}
		rgb(r,g,b)
	}
    else character(0)
}
# par(mfrow=c(2,1)); colramp(tail.colors(100)); colramp(tail.colors(100,rev=TRUE))



################### wh colors
###################
###################
jet.colors <- function(n, rev=FALSE) 
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
  		n <- max(c(round(n/4), 1))
		e <- rep(1, n)
 		 x <- (1:n)/n;
  		y <- ((n/2):n)/n;
 		r <- c(0*y, 0*e, x, e, y[length(y):1])
  		g <- c(0*y, x, e, x[length(x):1], 0*y)
  		b <- c(y, e, x[length(x):1], 0*e, 0*y)
		if (rev){
		r <- r[length(r):1]
		g <- g[length(g):1]
		b <- b[length(b):1]
    	}
  rgb(r,g,b)
}
    else character(0)
}
# par(mfrow=c(2,1)); colramp(jet.colors(100)); colramp(jet.colors(100,rev=TRUE))

###################
###################
wh.colors <- function(n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1

  r  <- numeric(n)
  g  <- numeric(n)
  b  <- numeric(n)
  k2 <- round( 0.5*n)
  k3 <- round(0.75*n)
  
  k    <- 1:k2
  r[k] <- (k2-k) / (k2-1)
  g[k] <- (k2-k) / (k2-1)
  b[k] <- 1

  k <- (k2+1):(k3-1)
  r[k] <- sqrt((k-k2-1) / (n-k2))
  g[k] <- 0
  b[k] <- 1 - (k-k2-1) / (n-k2)

  k    <- k3:(n)
  r[k] <- sqrt((k-k2-1) / (n-k2))
  g[k] <- (k-k3) / (n-k3)
  b[k] <- 1 - (k-k2-1) / (n-k2)
 
  		if (rev){
		r <- r[length(r):1]
		g <- g[length(g):1]
		b <- b[length(b):1]
    	}

  rgb(r,g,b)
    	}
    else character(0)
} 
# par(mfrow=c(2,1)); colramp(wh.colors(100)); colramp(wh.colors(100,rev=TRUE))

# par(mfrow=c(2,1)); colramp(wh.colors(100)); colramp(wh.colors(100,rev=TRUE))
