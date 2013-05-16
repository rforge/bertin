# make a neutral background for undefined slots.
# Note: colour should be adjusted to give a neutral grey when compared to the data points.
imageBG <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, zlim = range(z[is.finite(z)]), 
    xlim = range(x), ylim = range(y), col = "#80808080", add=TRUE, ...)
{
	z <- ifelse(is.na(e[[k]]),1,NA)
	image(x,y,z,zlim,xlim,ylim,col,add,...)
}
