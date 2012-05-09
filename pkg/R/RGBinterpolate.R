RGBinterpolate <- function (n=100, fromRGB, toRGB, igamma=1.0){
	 if ((n <- as.integer(n[1])) > 0) {
		 from <-col2rgb(fromRGB)/255
		 to <-col2rgb(toRGB)/255
		 ifrom <- ((n:0)/n)^igamma; ito <- ((0:n)/n)^igamma
		rgb(ifrom*from[1]+ito*to[1],ifrom*from[2]+ito*to[2],ifrom*from[3]+ito*to[3])
		 }
     else character(0)
 }