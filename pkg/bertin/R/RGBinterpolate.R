#$Id: 00bertinworksheet.R 77 2013-05-09 14:02:45Z gsawitzki $
RGBinterpolate <- function (n=100, fromRGB, toRGB, bias=1.0){
	 if ((n <- as.integer(n[1])) > 0) {
		 from <-col2rgb(fromRGB)/255
		 to <-col2rgb(toRGB)/255
		 ifrom <- ((n:0)/n)^bias; ito <- 1-ifrom
		rgb(ifrom*from[1]+ito*to[1],ifrom*from[2]+ito*to[2],ifrom*from[3]+ito*to[3])
		 }
     else character(0)
 }