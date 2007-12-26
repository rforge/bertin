#01bincode.R

bincodedata <- matrix(c(
	1,0,0,
	0,1,0,
	1,1,0,
	0,0,1,
	1,0,1,
	0,1,1,
	1,1,1),nrow=3,ncol=7
	,dimnames=list(c("b1","b2","b4"),
		c("v1","v2","v3","v4","v5","v6","v7")))

bincodedata

image.bertin(bincodedata)

# 

bincodedatav <- matrix(c(
	1,0,0,
	0,1/2,0,
	1/3,1/3,0,
	0,0,1/4,
	1/5,0,1/5,
	0,1/6,1/6,
	1/7,1/7,1/7),nrow=3,ncol=7
	,dimnames=list(c("b1","b2","b4"),
		c("v1","v2","v3","v4","v5","v6","v7")))

bincodedatav

image.bertin(bincodedatav)

